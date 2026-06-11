(ns flatiron.group
  "Group-by aggregation using open-addressing hash tables.
   Two-pass single-threaded: Pass 1 builds group assignment,
   Pass 2 accumulates aggregation values per group."
  (:require [clojure.core.async :as a]
            [flatiron.column :as col]
            [flatiron.hash :as h]
            [flatiron.morsel :as m]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column SymColumn StrColumn]))

;; ════════════════════════════════════════════════════════════════════════
;; Fast hashing — avoid BigInteger wyhash for prehash step
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long HASH-K1 (unchecked-long 0xC6A4A7935BD1E995))
(def ^:const ^long HASH-K2 (unchecked-long 0xBF58476D1CE4E5B9))
(def ^:const ^long HASH-K3 (unchecked-long 0x94D049BB133111EB))
(def ^:const ^long HASH-K4 (unchecked-long 0x9E3779B97F4A7C15))

(defn- fast-int-hash ^long [h]
  "Fast 64-bit hash from a 32-bit integer. Good distribution, no BigInteger."
  (let [x (bit-xor (unchecked-int h) 0x9E3779B9)]
    (unchecked-multiply (long x) HASH-K1)))

(defn- fast-long-hash ^long [^long v]
  "Fast 64-bit hash from a 64-bit integer using split-mix64."
  (let [x (unchecked-add v HASH-K4)
        x (bit-xor x (unsigned-bit-shift-right x 30))
        x (unchecked-multiply x HASH-K2)
        x (bit-xor x (unsigned-bit-shift-right x 27))
        x (unchecked-multiply x HASH-K3)]
    (bit-xor x (unsigned-bit-shift-right x 31))))

(defn- fast-double-hash ^long [^double v]
  (let [bits (if (== v 0.0) 0 (Double/doubleToLongBits v))]
    (fast-long-hash bits)))

;; ════════════════════════════════════════════════════════════════════════
;; Hash table build
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long HT-EMPTY h/HT-EMPTY)

(defn- prehash-i64-column ^longs [key-col]
  (let [^I64Column c key-col
        ^longs data (.data c)
        off (.offset c)
        n (.len c)
        out (long-array n)]
    (loop [i (int 0)]
      (when (< i n)
        (aset out i (fast-long-hash (aget data (+ off i))))
        (recur (unchecked-inc i))))
    out))

(defn- prehash-f64-column ^longs [key-col]
  (let [^F64Column c key-col
        ^doubles data (.data c)
        off (.offset c)
        n (.len c)
        out (long-array n)]
    (loop [i (int 0)]
      (when (< i n)
        (aset out i (fast-double-hash (aget data (+ off i))))
        (recur (unchecked-inc i))))
    out))

(defn- prehash-obj-column ^longs [key-col]
  (let [^SymColumn c key-col
        ^objects data (.data c)
        off (.offset c)
        n (.len c)
        out (long-array n)]
    (loop [i (int 0)]
      (when (< i n)
        (let [obj (aget data (+ off i))
              h (if (nil? obj) 0 (.hashCode ^Object obj))]
          (aset out i (fast-int-hash h))
          (recur (unchecked-inc i)))))
    out))

(defn- prehash-column ^longs [key-col]
  (case (col/-type-tag key-col)
    :i64 (prehash-i64-column key-col)
    :f64 (prehash-f64-column key-col)
    (:sym :str) (prehash-obj-column key-col)
    (throw (IllegalArgumentException.
            (str "Unsupported group-by key type: " (col/-type-tag key-col))))))

(defn- col-val-eq?
  "Whether two rows hold equal values in a single key column."
  [c ^long a ^long b]
  (case (col/-type-tag c)
    :i64 (== (col/-get-long c a) (col/-get-long c b))
    :f64 (let [x (col/-get-double c a)
               y (col/-get-double c b)]
           ;; NaN counts as equal to NaN so null keys group together;
           ;; == handles -0.0 / +0.0 consistently with the hash.
           (if (Double/isNaN x) (Double/isNaN y) (== x y)))
    (= (col/-get-obj c a) (col/-get-obj c b))))

(defn- keys-equal?
  "Whether two rows are equal across every key column."
  [^objects kcols ^long nkeys ^long a ^long b]
  (loop [k 0]
    (if (< k nkeys)
      (if (col-val-eq? (aget kcols k) a b)
        (recur (unchecked-inc k))
        false)
      true)))

(defn build-groups
  "Assign each row a group-id via an open-addressing hash table.
   Slots are matched on the combined hash and then verified by comparing the
   actual key values, so hash collisions never merge distinct keys.
   Returns [n-groups row-groups]."
  [key-cols]
  (let [nrows  (col/-len (first key-cols))
        nkeys  (count key-cols)
        kcols  (object-array key-cols)
        cap    (h/ht-capacity nrows)
        cap-mask (unchecked-dec cap)
        ht-hash   (long-array cap)
        ht-row    (int-array cap)   ;; representative row per slot, -1 = empty
        ht-groups (int-array cap)
        row-groups (int-array nrows)
        prehashes (object-array nkeys)]
    (java.util.Arrays/fill ht-row (int -1))
    (dotimes [k nkeys]
      (aset prehashes k (prehash-column (nth key-cols k))))
    (let [next-gid (int-array [0])]
      (loop [row 0]
        (when (< row nrows)
          (let [combined
                (loop [k 0, ch 0]
                  (if (< k nkeys)
                    (let [kh (aget ^longs (aget prehashes k) row)]
                      (recur (unchecked-inc k)
                             (if (zero? k) kh (h/hash-combine ch kh))))
                    ch))
                start-slot (bit-and combined cap-mask)
                slot (loop [s start-slot]
                       (let [r (aget ht-row s)]
                         (cond
                           (== r -1) s
                           (and (== (aget ht-hash s) combined)
                                (keys-equal? kcols nkeys row r)) s
                           :else (recur (bit-and (unchecked-inc s) cap-mask)))))]
            (if (== (aget ht-row slot) -1)
              (let [gid (aget next-gid 0)]
                (aset ht-hash slot combined)
                (aset ht-row slot (int row))
                (aset ht-groups slot gid)
                (aset row-groups row gid)
                (aset next-gid 0 (unchecked-inc gid)))
              (aset row-groups row (aget ht-groups slot)))
            (recur (unchecked-inc row)))))
      [(long (aget next-gid 0)) row-groups])))

;; ════════════════════════════════════════════════════════════════════════
;; Aggregation accumulation — direct-access fast paths
;; Bypasses morsel layer entirely for non-nullable columns.
;; Reads directly from the column's backing primitive array.
;; ════════════════════════════════════════════════════════════════════════

(defn- accumulate-i64-sum! [val-col ^ints row-groups ^longs sums]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (let [null-sent col/NULL_I64]
        (loop [i (int 0)]
          (when (< i n)
            (let [v (aget data (+ col-off i))]
              (when (not= v null-sent)
                (let [g (aget row-groups i)]
                  (aset sums g (unchecked-add (aget sums g) v)))))
            (recur (unchecked-inc i)))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset sums g (unchecked-add (aget sums g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-i64-count! [val-col ^ints row-groups ^longs counts]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (let [null-sent col/NULL_I64]
        (loop [i (int 0)]
          (when (< i n)
            (let [v (aget data (+ col-off i))]
              (when (not= v null-sent)
                (let [g (aget row-groups i)]
                  (aset counts g (unchecked-inc (aget counts g)))))
              (recur (unchecked-inc i))))))
      (loop [i (int 0)]
        (when (< i n)
          (let [g (aget row-groups i)]
            (aset counts g (unchecked-inc (aget counts g))))
          (recur (unchecked-inc i)))))))

(defn- accumulate-i64-min! [val-col ^ints row-groups ^longs mins]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (let [null-sent col/NULL_I64]
        (loop [i (int 0)]
          (when (< i n)
            (let [v (aget data (+ col-off i))]
              (when (not= v null-sent)
                (let [g (aget row-groups i)]
                  (aset mins g (min (aget mins g) v))))
              (recur (unchecked-inc i))))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset mins g (min (aget mins g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-i64-max! [val-col ^ints row-groups ^longs maxs]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (let [null-sent col/NULL_I64]
        (loop [i (int 0)]
          (when (< i n)
            (let [v (aget data (+ col-off i))]
              (when (not= v null-sent)
                (let [g (aget row-groups i)]
                  (aset maxs g (max (aget maxs g) v))))
              (recur (unchecked-inc i))))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset maxs g (max (aget maxs g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-i64-avg! [val-col ^ints row-groups ^longs sums ^longs counts]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (let [null-sent col/NULL_I64]
        (loop [i (int 0)]
          (when (< i n)
            (let [v (aget data (+ col-off i))]
              (when (not= v null-sent)
                (let [g (aget row-groups i)]
                  (aset sums g (unchecked-add (aget sums g) v))
                  (aset counts g (unchecked-inc (aget counts g)))))
              (recur (unchecked-inc i))))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset sums g (unchecked-add (aget sums g) v))
            (aset counts g (unchecked-inc (aget counts g))))
          (recur (unchecked-inc i)))))))

;; F64 variants

(defn- accumulate-f64-sum! [val-col ^ints row-groups ^doubles sums]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not (Double/isNaN v))
              (let [g (aget row-groups i)]
                (aset sums g (+ (aget sums g) v))))
            (recur (unchecked-inc i)))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset sums g (+ (aget sums g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-f64-count! [val-col ^ints row-groups ^longs counts]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not (Double/isNaN v))
              (let [g (aget row-groups i)]
                (aset counts g (unchecked-inc (aget counts g)))))
            (recur (unchecked-inc i)))))
      (loop [i (int 0)]
        (when (< i n)
          (let [g (aget row-groups i)]
            (aset counts g (unchecked-inc (aget counts g))))
          (recur (unchecked-inc i)))))))

(defn- accumulate-f64-min! [val-col ^ints row-groups ^doubles mins]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)]
    (loop [i (int 0)]
      (when (< i n)
        (let [v (aget data (+ col-off i))]
          (when (not (Double/isNaN v))
            (let [g (aget row-groups i)]
              (aset mins g (min (aget mins g) v))))
          (recur (unchecked-inc i)))))))

(defn- accumulate-f64-max! [val-col ^ints row-groups ^doubles maxs]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)]
    (loop [i (int 0)]
      (when (< i n)
        (let [v (aget data (+ col-off i))]
          (when (not (Double/isNaN v))
            (let [g (aget row-groups i)]
              (aset maxs g (max (aget maxs g) v))))
          (recur (unchecked-inc i)))))))

(defn- accumulate-f64-avg! [val-col ^ints row-groups ^doubles sums ^longs counts]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not (Double/isNaN v))
              (let [g (aget row-groups i)]
                (aset sums g (+ (aget sums g) v))
                (aset counts g (unchecked-inc (aget counts g)))))
            (recur (unchecked-inc i)))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset sums g (+ (aget sums g) v))
            (aset counts g (unchecked-inc (aget counts g))))
          (recur (unchecked-inc i)))))))

;; ════════════════════════════════════════════════════════════════════════
;; Key column compression
;; ════════════════════════════════════════════════════════════════════════

(defn compress-key-column
  "Extract distinct group key values. Returns a column of length n-groups."
  [key-col row-groups ^long n-groups]
  (let [tag   (col/-type-tag key-col)
        nrows (col/-len key-col)
        seen  (boolean-array n-groups)]
    (case tag
      :i64
      (let [out (long-array n-groups)]
        (loop [row 0]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when (not (aget seen g))
                (aset seen g true)
                (aset out g (col/-get-long key-col row)))
              (recur (unchecked-inc row)))))
        (I64Column. out n-groups 0 (col/-has-nulls? key-col)))
      :f64
      (let [out (double-array n-groups)]
        (loop [row 0]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when (not (aget seen g))
                (aset seen g true)
                (aset out g (col/-get-double key-col row)))
              (recur (unchecked-inc row)))))
        (F64Column. out n-groups 0 (col/-has-nulls? key-col)))
      :sym
      (let [out (object-array n-groups)]
        (loop [row 0]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when (not (aget seen g))
                (aset seen g true)
                (aset out g (col/-get-obj key-col row)))
              (recur (unchecked-inc row)))))
        (SymColumn. out n-groups 0 (col/-has-nulls? key-col)))
      :str
      (let [out (object-array n-groups)]
        (loop [row 0]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when (not (aget seen g))
                (aset seen g true)
                (aset out g (col/-get-obj key-col row)))
              (recur (unchecked-inc row)))))
        (StrColumn. out n-groups 0 (col/-has-nulls? key-col)))
      (throw (IllegalArgumentException. (str "Unsupported key type: " tag))))))

;; ════════════════════════════════════════════════════════════════════════
;; Public API
;; ════════════════════════════════════════════════════════════════════════

(defn group-by
  "Group-by aggregation over a table.
   
   Usage:
     (group-by table
       :keys [:region]
       :aggs [{:agg :sum :col :amount :out :total}])
   
   Returns a new Table with key columns followed by aggregation result columns.
   
   Supported agg types: :sum, :count, :min, :max, :avg"
  [table & {:keys [keys aggs]}]
  (let [key-cols (mapv #(tbl/col table %) keys)
        _ (assert (seq key-cols) "At least one key column required")
        nrows (col/-len (first key-cols))
        _ (doseq [kc (rest key-cols)]
            (assert (= nrows (col/-len kc)) "All key columns must have same length"))
        ;; Pass 1: build groups
        [n-groups row-groups] (build-groups key-cols)]
    (if (zero? n-groups)
      (tbl/table [] [])
      (let [;; Key columns
            result-keys (mapv #(compress-key-column % row-groups n-groups) key-cols)
            key-names   (vec keys)
            ;; Aggregation columns
            [agg-names agg-cols]
            (loop [specs aggs, names [], cols []]
              (if (seq specs)
                (let [{:keys [agg col out]} (first specs)
                      val-col (tbl/col table col)
                       tag     (col/-type-tag val-col)]
                   (case tag
                     (:sym :str :bool)
                     (let [result
                           (case agg
                             :count (let [c (long-array n-groups)]
                                      (let [has-nulls (col/-has-nulls? val-col)
                                            ms  (m/obj-morsel-source val-col)
                                            buf (object-array m/MORSEL-SIZE)]
                                        (loop [off 0]
                                          (let [cnt (m/morsel-next-obj! ms buf 0 m/MORSEL-SIZE)]
                                            (when (pos? cnt)
                                              (if has-nulls
                                                (loop [i 0]
                                                  (when (< i cnt)
                                                    (let [g (aget row-groups (+ off i))]
                                                      (when (some? (aget buf i))
                                                        (aset c g (unchecked-inc (aget c g)))))
                                                    (recur (unchecked-inc i))))
                                                (dotimes [i cnt]
                                                  (let [g (aget row-groups (+ off i))]
                                                    (aset c g (unchecked-inc (aget c g))))))
                                              (recur (+ off cnt))))))
                                      (I64Column. c n-groups 0 false))
                             (throw (IllegalArgumentException.
                                     (str "Agg " agg " not supported for " tag " values"))))]
                       (recur (rest specs) (conj names out) (conj cols result)))
                     :i64
                    (let [result
                          (case agg
                            :sum   (let [s (long-array n-groups)]
                                     (accumulate-i64-sum! val-col row-groups s)
                                     (I64Column. s n-groups 0 false))
                            :count (let [c (long-array n-groups)]
                                     (accumulate-i64-count! val-col row-groups c)
                                     (I64Column. c n-groups 0 false))
                            :min   (let [m (long-array n-groups)]
                                     (java.util.Arrays/fill m Long/MAX_VALUE)
                                     (accumulate-i64-min! val-col row-groups m)
                                     (I64Column. m n-groups 0 false))
                            :max   (let [m (long-array n-groups)]
                                     (java.util.Arrays/fill m Long/MIN_VALUE)
                                     (accumulate-i64-max! val-col row-groups m)
                                     (I64Column. m n-groups 0 false))
                            :avg   (let [s (long-array n-groups)
                                         c (long-array n-groups)]
                                     (accumulate-i64-avg! val-col row-groups s c)
                                     (let [out (double-array n-groups)]
                                       (dotimes [i n-groups]
                                         (let [cnt (aget c i)]
                                           (aset out i
                                                 (if (zero? cnt)
                                                   Double/NaN
                                                   (/ (double (aget s i)) (double cnt))))))
                                       (F64Column. out n-groups 0 false))))]
                      (recur (rest specs) (conj names out) (conj cols result)))
                    :f64
                    (let [result
                          (case agg
                            :sum   (let [s (double-array n-groups)]
                                     (accumulate-f64-sum! val-col row-groups s)
                                     (F64Column. s n-groups 0 false))
                            :count (let [c (long-array n-groups)]
                                     (accumulate-f64-count! val-col row-groups c)
                                     (I64Column. c n-groups 0 false))
                            :min   (let [m (double-array n-groups)]
                                     (java.util.Arrays/fill m Double/MAX_VALUE)
                                     (accumulate-f64-min! val-col row-groups m)
                                     (F64Column. m n-groups 0 false))
                            :max   (let [m (double-array n-groups)]
                                     (java.util.Arrays/fill m (- Double/MAX_VALUE))
                                     (accumulate-f64-max! val-col row-groups m)
                                     (F64Column. m n-groups 0 false))
                            :avg   (let [s (double-array n-groups)
                                         c (long-array n-groups)]
                                     (accumulate-f64-avg! val-col row-groups s c)
                                     (let [out (double-array n-groups)]
                                       (dotimes [i n-groups]
                                         (let [cnt (aget c i)]
                                           (aset out i
                                                 (if (zero? cnt)
                                                   Double/NaN
                                                   (/ (aget s i) (double cnt))))))
                                       (F64Column. out n-groups 0 false))))]
                      (recur (rest specs) (conj names out) (conj cols result)))
                    (throw (IllegalArgumentException.
                            (str "Unsupported value type for agg: " tag)))))
                [names cols]))]
        (tbl/table (vec (concat key-names (vec agg-names)))
                   (vec (concat result-keys (vec agg-cols))))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel radix-partitioned group-by
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long DEFAULT-PARALLELISM 4)

(defn- radix-bits ^long [^long n-threads]
  "Number of radix bits for ~2× n-threads partitions (power of two)."
  (let [target (* 2 n-threads)]
    (loop [bits 2]
      (if (>= (bit-shift-left 1 bits) target)
        (min bits 10)
        (recur (unchecked-inc bits))))))

(defn- partition-local-group-by [key-cols key-names table aggs
                                 local-rows ^longs local-hashes n-local
                                 cap]
  "Build HT and accumulate for a single partition. Returns [schema cols].
   Called from parallel worker threads — no shared mutable state."
  (let [n-local (int n-local)
        cap (int cap)
        cap-mask (unchecked-dec cap)
        ht-hash  (long-array cap)
        ht-groups (int-array cap)
        row-groups (int-array n-local)
        next-gid (int-array 1)]
    (java.util.Arrays/fill ht-hash HT-EMPTY)
    (dotimes [i n-local]
      (let [hash (aget local-hashes i)
            slot (loop [s (bit-and hash cap-mask)]
                   (let [k (aget ht-hash s)]
                     (if (or (== k HT-EMPTY) (== k hash))
                       s
                       (recur (bit-and (unchecked-inc s) cap-mask)))))]
        (if (== (aget ht-hash slot) HT-EMPTY)
          (let [gid (aget next-gid 0)]
            (aset ht-hash slot hash)
            (aset ht-groups slot gid)
            (aset row-groups i gid)
            (aset next-gid 0 (unchecked-inc gid)))
          (aset row-groups i (aget ht-groups slot)))))
    (let [n-groups (aget next-gid 0)]
      (if (zero? n-groups)
        nil
        (let [result-key-cols
              (mapv (fn [kc]
                      (let [tag (col/-type-tag kc)
                            seen (boolean-array n-groups)]
                        (case tag
                          :i64
                          (let [^I64Column c kc
                                ^longs data (.data c)
                                off (.offset c)
                                out (long-array n-groups)]
                            (dotimes [i n-local]
                              (let [g (aget row-groups i)]
                                (when-not (aget seen g)
                                  (aset seen g true)
                                  (aset out g (aget data (+ off (aget local-rows i)))))))
                            (I64Column. out n-groups 0 (.has-nulls c)))
                          :f64
                          (let [^F64Column c kc
                                ^doubles data (.data c)
                                off (.offset c)
                                out (double-array n-groups)]
                            (dotimes [i n-local]
                              (let [g (aget row-groups i)]
                                (when-not (aget seen g)
                                  (aset seen g true)
                                  (aset out g (aget data (+ off (aget local-rows i)))))))
                            (F64Column. out n-groups 0 (.has-nulls c)))
                          (:sym :str)
                          (let [^objects data (.data kc)
                                off (.offset kc)
                                out (object-array n-groups)]
                            (dotimes [i n-local]
                              (let [g (aget row-groups i)]
                                (when-not (aget seen g)
                                  (aset seen g true)
                                  (aset out g (aget data (+ off (aget local-rows i)))))))
                            (if (= tag :sym)
                              (SymColumn. out n-groups 0 (col/-has-nulls? kc))
                              (StrColumn. out n-groups 0 (col/-has-nulls? kc))))
                          (throw (IllegalArgumentException. (str "Bad key type: " tag))))))
                   key-cols)
              result-agg-cols
              (mapv (fn [{:keys [agg col]}]
                      (let [val-col (tbl/col table col)
                            tag (col/-type-tag val-col)]
                        (case tag
                          :i64
                          (case agg
                            :sum (let [^longs s (long-array n-groups)]
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-long val-col (aget local-rows i))]
                                       (aset s g (unchecked-add (aget s g) v))))
                                   (I64Column. s n-groups 0 false))
                            :count (let [^longs c (long-array n-groups)]
                                     (dotimes [i n-local]
                                       (let [g (aget row-groups i)]
                                         (aset c g (unchecked-inc (aget c g)))))
                                     (I64Column. c n-groups 0 false))
                            :min (let [^longs m (long-array n-groups)]
                                   (java.util.Arrays/fill m Long/MAX_VALUE)
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-long val-col (aget local-rows i))]
                                       (aset m g (min (aget m g) v))))
                                   (I64Column. m n-groups 0 false))
                            :max (let [^longs mx (long-array n-groups)]
                                   (java.util.Arrays/fill mx Long/MIN_VALUE)
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-long val-col (aget local-rows i))]
                                       (aset mx g (max (aget mx g) v))))
                                   (I64Column. mx n-groups 0 false))
                            :avg (let [^longs s (long-array n-groups)
                                       ^longs c (long-array n-groups)]
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-long val-col (aget local-rows i))]
                                       (aset s g (unchecked-add (aget s g) v))
                                       (aset c g (unchecked-inc (aget c g)))))
                                   (let [out (double-array n-groups)]
                                     (dotimes [j n-groups]
                                       (let [cnt (aget c j)]
                                         (aset out j (if (zero? cnt) Double/NaN
                                                         (/ (double (aget s j)) (double cnt))))))
                                     (F64Column. out n-groups 0 false))))
                          :f64
                          (case agg
                            :sum (let [^doubles s (double-array n-groups)]
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-double val-col (aget local-rows i))]
                                       (aset s g (+ (aget s g) v))))
                                   (F64Column. s n-groups 0 false))
                            :count (let [^longs c (long-array n-groups)]
                                     (dotimes [i n-local]
                                       (let [g (aget row-groups i)]
                                         (aset c g (unchecked-inc (aget c g)))))
                                     (I64Column. c n-groups 0 false))
                            :min (let [^doubles m (double-array n-groups)]
                                   (java.util.Arrays/fill m Double/MAX_VALUE)
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-double val-col (aget local-rows i))]
                                       (aset m g (min (aget m g) v))))
                                   (F64Column. m n-groups 0 false))
                            :max (let [^doubles mx (double-array n-groups)]
                                   (java.util.Arrays/fill mx (- Double/MAX_VALUE))
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-double val-col (aget local-rows i))]
                                       (aset mx g (max (aget mx g) v))))
                                   (F64Column. mx n-groups 0 false))
                            :avg (let [^doubles s (double-array n-groups)
                                       ^longs c (long-array n-groups)]
                                   (dotimes [i n-local]
                                     (let [g (aget row-groups i)
                                           v (col/-get-double val-col (aget local-rows i))]
                                       (aset s g (+ (aget s g) v))
                                       (aset c g (unchecked-inc (aget c g)))))
                                   (let [out (double-array n-groups)]
                                     (dotimes [j n-groups]
                                       (let [cnt (aget c j)]
                                         (aset out j (if (zero? cnt) Double/NaN
                                                         (/ (aget s j) (double cnt))))))
                                     (F64Column. out n-groups 0 false))))
                          (throw (IllegalArgumentException. (str "Bad val type: " tag))))))
                    aggs)]
          [(vec (concat (mapv #(tbl/col-name table (tbl/col-idx table %)) key-names)
                        (mapv :out aggs)))
           (vec (concat result-key-cols result-agg-cols))])))))

(defn parallel-group-by
  "Parallel radix-partitioned group-by.
   Partitions rows by radix bits of the combined key hash,
   then processes each partition independently in core.async threads.
   Results are concatenated — no merging needed since partitions are disjoint.
   
   Usage: (parallel-group-by table :keys [:region] :aggs [{:agg :sum :col :amount :out :total}])
   
   Options:
     :n-threads — number of worker threads (default 4)"
  [table & {:keys [keys aggs n-threads]
            :or {n-threads DEFAULT-PARALLELISM}}]
  (let [key-cols (mapv #(tbl/col table %) keys)
        _ (assert (seq key-cols) "At least one key column required")
        nrows (col/-len (first key-cols))
        _ (doseq [kc (rest key-cols)]
            (assert (= nrows (col/-len kc)) "All key columns must have same length"))]
    (if (zero? nrows)
      (tbl/table [] [])
      (let [nkeys   (count key-cols)
            rbits   (radix-bits n-threads)
            n-parts (bit-shift-left 1 rbits)
            mask    (unchecked-dec n-parts)
            prehashes (object-array nkeys)]
        (dotimes [k nkeys]
          (aset prehashes k (prehash-column (nth key-cols k))))
        (let [part-rows (object-array n-parts)]
          (dotimes [p n-parts] (aset part-rows p (java.util.ArrayList.)))
          (dotimes [row nrows]
            (let [combined (loop [k 0, ch (long 0)]
                             (if (< k nkeys)
                               (let [kh (aget ^longs (aget prehashes k) row)]
                                 (recur (unchecked-inc k)
                                        (if (zero? k) kh (h/hash-combine ch kh))))
                               ch))
                  part (bit-and combined mask)]
              (.add ^java.util.ArrayList (aget part-rows part)
                    (long-array [row combined]))))
          (let [result-chan (a/chan n-parts)
                active (volatile! 0)]
            (dotimes [p n-parts]
              (let [^java.util.ArrayList rows (aget part-rows p)
                    n-local (.size rows)]
                (when (pos? n-local)
                  (vswap! active inc)
                  (a/thread
                    (let [cap (h/ht-capacity n-local)
                          local-rows (long-array n-local)
                          local-hashes (long-array n-local)]
                      (dotimes [i n-local]
                        (let [pair ^longs (.get rows i)]
                          (aset local-rows i (aget pair 0))
                          (aset local-hashes i (aget pair 1))))
                      (let [result (partition-local-group-by key-cols (vec keys) table aggs
                                      local-rows local-hashes n-local cap)]
                        (a/>!! result-chan result)))))))
            (let [active-count @active]
              (loop [remaining active-count
                     acc-schema nil
                     acc-cols nil]
                (if (zero? remaining)
                  (if acc-schema
                    (tbl/table acc-schema acc-cols)
                    (tbl/table (vec (concat (vec keys) (mapv :out aggs)))
                               (vec (repeat (+ (count keys) (count aggs))
                                            (I64Column. (long-array 0) 0 0 false)))))
                  (let [result (a/<!! result-chan)]
                    (if result
                      (let [[schema2 cols2] result]
                        (if (nil? acc-schema)
                          (recur (dec remaining) schema2 cols2)
                          (let [merged-cols
                                (mapv (fn [acc-col part-col]
                                        (let [tag (col/-type-tag acc-col)
                                              n-acc (col/-len acc-col)
                                              n-part (col/-len part-col)
                                              n-new (unchecked-add n-acc n-part)]
                                          (if (zero? n-part)
                                            acc-col
                                            (case tag
                                              :i64
                                              (let [data (long-array n-new)]
                                                (System/arraycopy (.data ^I64Column acc-col) (.offset ^I64Column acc-col) data 0 n-acc)
                                                (System/arraycopy (.data ^I64Column part-col) (.offset ^I64Column part-col) data n-acc n-part)
                                                (I64Column. data n-new 0 false))
                                              :f64
                                              (let [data (double-array n-new)]
                                                (System/arraycopy (.data ^F64Column acc-col) (.offset ^F64Column acc-col) data 0 n-acc)
                                                (System/arraycopy (.data ^F64Column part-col) (.offset ^F64Column part-col) data n-acc n-part)
                                                (F64Column. data n-new 0 false))
                                              (:sym :str)
                                              (let [data (object-array n-new)]
                                                (System/arraycopy (.data acc-col) (.offset acc-col) data 0 n-acc)
                                                (System/arraycopy (.data part-col) (.offset part-col) data n-acc n-part)
                                                (if (= tag :sym)
                                                  (SymColumn. data n-new 0 false)
                                                  (StrColumn. data n-new 0 false)))
                                              (throw (IllegalArgumentException. (str "Bad merge type: " tag)))))))
                                      acc-cols cols2)]
                            (recur (dec remaining) schema2 merged-cols))))
                      (recur (dec remaining) acc-schema acc-cols)))))))))))


)
;; ════════════════════════════════════════════════════════════════════════
;; Pivot (cross-tabulation)
;; ════════════════════════════════════════════════════════════════════════

(defn- build-key-column
  "Build a typed column from a seq of key values, matching `proto-col`'s type."
  [proto-col vals]
  (case (col/-type-tag proto-col)
    :i64  (col/i64-column (map (fn [v] (when (some? v) (long v))) vals))
    :f64  (col/f64-column vals)
    :sym  (col/sym-column vals)
    :str  (col/str-column vals)
    :bool (col/bool-column vals)
    (throw (IllegalArgumentException.
            (str "Unsupported pivot key type: " (col/-type-tag proto-col))))))

(defn- pivot-col-name
  "Turn a column-axis value into a keyword column name."
  [v]
  (cond
    (keyword? v) v
    (nil? v)     :__null__
    :else        (keyword (str v))))

(defn pivot-table
  "Cross-tabulate a table. Rows are the distinct values of `row-kw`, output
   columns are the distinct values of `col-kw`, and each cell is `agg` applied
   to `val-kw` over the matching (row, column) pair. Cells with no matching
   rows are null.

   `agg` is one of :sum :count :avg :min :max. Row and column orders follow
   first appearance in the input."
  [table row-kw col-kw val-kw agg]
  (let [rcol (tbl/col table row-kw)
        ccol (tbl/col table col-kw)
        vcol (tbl/col table val-kw)
        n    (col/-len rcol)
        vtag (col/-type-tag vcol)]
    (loop [i 0, acc {}, rorder [], rset #{}, corder [], cset #{}]
      (if (< i n)
        (let [rk (col/-get-obj rcol i)
              ck (col/-get-obj ccol i)
              v  (col/-get-obj vcol i)
              k  [rk ck]
              st (get acc k {:sum 0.0 :cnt 0 :min Double/POSITIVE_INFINITY :max Double/NEGATIVE_INFINITY})
              st (if (nil? v)
                   st
                   (let [d (double v)]
                     {:sum (+ (:sum st) d)
                      :cnt (inc (:cnt st))
                      :min (min (double (:min st)) d)
                      :max (max (double (:max st)) d)}))]
          (recur (unchecked-inc i)
                 (assoc acc k st)
                 (if (contains? rset rk) rorder (conj rorder rk))
                 (conj rset rk)
                 (if (contains? cset ck) corder (conj corder ck))
                 (conj cset ck)))
        (let [cell (fn [rk ck]
                     (when-let [st (get acc [rk ck])]
                       (case agg
                         :sum   (:sum st)
                         :count (:cnt st)
                         :avg   (when (pos? (:cnt st)) (/ (:sum st) (double (:cnt st))))
                         :min   (when (pos? (:cnt st)) (:min st))
                         :max   (when (pos? (:cnt st)) (:max st)))))
              cell-type (case agg
                          :count :i64
                          :avg   :f64
                          (case vtag :i64 :i64 :f64))
              row-key-col (build-key-column rcol rorder)
              value-cols  (mapv (fn [ck]
                                  (let [vals (map (fn [rk] (cell rk ck)) rorder)]
                                    (case cell-type
                                      :i64 (col/i64-column (map (fn [x] (when (some? x) (long x))) vals))
                                      :f64 (col/f64-column vals))))
                                corder)
              schema (into [row-kw] (map pivot-col-name corder))
              cols   (into [row-key-col] value-cols)]
          (tbl/table schema cols))))))
