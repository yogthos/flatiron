(ns flatiron.group
  "Group-by aggregation using open-addressing hash tables.
   Two-pass single-threaded: Pass 1 builds group assignment,
   Pass 2 accumulates aggregation values per group.
   Parallel variant radix-partitions rows by the high bits of the key hash
   and processes disjoint partitions on worker threads."
  (:refer-clojure :exclude [group-by])
  (:require [clojure.core.async :as a]
            [flatiron.column :as col]
            [flatiron.hash :as h]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column BoolColumn SymColumn StrColumn]))

(set! *warn-on-reflection* true)

;; ════════════════════════════════════════════════════════════════════════
;; Fast hashing — avoid BigInteger wyhash for prehash step
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long HASH-K1 (unchecked-long 0xC6A4A7935BD1E995))
(def ^:const ^long HASH-K2 (unchecked-long 0xBF58476D1CE4E5B9))
(def ^:const ^long HASH-K3 (unchecked-long 0x94D049BB133111EB))
(def ^:const ^long HASH-K4 (unchecked-long 0x9E3779B97F4A7C15))

(defn- fast-int-hash
  "Fast 64-bit hash from a 32-bit integer. Good distribution, no BigInteger."
  ^long [^long h]
  (let [x (bit-xor (unchecked-int h) 0x9E3779B9)]
    (unchecked-multiply (long x) HASH-K1)))

(defn- fast-long-hash
  "Fast 64-bit hash from a 64-bit integer using split-mix64."
  ^long [^long v]
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
;; Column data helpers
;; ════════════════════════════════════════════════════════════════════════

(defn- obj-data ^objects [c]
  (if (instance? SymColumn c)
    (.data ^SymColumn c)
    (.data ^StrColumn c)))

(defn- obj-offset ^long [c]
  (if (instance? SymColumn c)
    (.offset ^SymColumn c)
    (.offset ^StrColumn c)))

;; ════════════════════════════════════════════════════════════════════════
;; Prehashing
;; ════════════════════════════════════════════════════════════════════════

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
  (let [^objects data (obj-data key-col)
        off (obj-offset key-col)
        n (col/-len key-col)
        out (long-array n)]
    (loop [i (int 0)]
      (when (< i n)
        (let [obj (aget data (+ off i))
              hc (if (nil? obj) 0 (.hashCode ^Object obj))]
          (aset out i (fast-int-hash hc))
          (recur (unchecked-inc i)))))
    out))

(defn- prehash-column ^longs [key-col]
  (case (col/-type-tag key-col)
    :i64 (prehash-i64-column key-col)
    :f64 (prehash-f64-column key-col)
    (:sym :str) (prehash-obj-column key-col)
    (throw (IllegalArgumentException.
            (str "Unsupported group-by key type: " (col/-type-tag key-col))))))

(defn- combined-hashes
  "Per-row hash combined across all key columns. Returns long[nrows].
   Single key returns its (fresh) prehash array directly."
  ^longs [key-cols]
  (let [nkeys (count key-cols)
        ^longs out (prehash-column (first key-cols))
        nrows (alength out)]
    (loop [k 1]
      (when (< k nkeys)
        (let [^longs kh (prehash-column (nth key-cols k))]
          (loop [i (int 0)]
            (when (< i nrows)
              (aset out i (h/hash-combine (aget out i) (aget kh i)))
              (recur (unchecked-inc i)))))
        (recur (unchecked-inc k))))
    out))

;; ════════════════════════════════════════════════════════════════════════
;; Key equality
;; ════════════════════════════════════════════════════════════════════════

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

;; ════════════════════════════════════════════════════════════════════════
;; Hash table build
;; ════════════════════════════════════════════════════════════════════════

(defn- build-groups-hashed
  "Assign each row a group-id via an open-addressing hash table.
   Slots are matched on the combined hash and then verified by comparing the
   actual key values, so hash collisions never merge distinct keys.
   Returns [n-groups row-groups]."
  [key-cols ^longs hashes]
  (let [nrows  (long (col/-len (first key-cols)))
        nkeys  (long (count key-cols))
        kcols  (object-array key-cols)
        cap    (h/ht-capacity nrows)
        cap-mask (unchecked-dec cap)
        ht-hash   (long-array cap)
        ht-row    (int-array cap)   ;; representative row per slot, -1 = empty
        ht-groups (int-array cap)
        row-groups (int-array nrows)]
    (java.util.Arrays/fill ht-row (int -1))
    (let [n-groups
          (long
           (loop [row 0, gid 0]
             (if (< row nrows)
               (let [hv (aget hashes row)
                     slot (long (loop [s (bit-and hv cap-mask)]
                                  (let [r (aget ht-row s)]
                                    (cond
                                      (== r -1) s
                                      (and (== (aget ht-hash s) hv)
                                           (keys-equal? kcols nkeys row r)) s
                                      :else (recur (bit-and (unchecked-inc s) cap-mask))))))]
                 (if (== (aget ht-row slot) -1)
                   (do (aset ht-hash slot hv)
                       (aset ht-row slot (int row))
                       (aset ht-groups slot (int gid))
                       (aset row-groups row (int gid))
                       (recur (unchecked-inc row) (unchecked-inc gid)))
                   (do (aset row-groups row (aget ht-groups slot))
                       (recur (unchecked-inc row) gid))))
               gid)))]
      [n-groups row-groups])))

(defn build-groups
  "Assign each row a group-id. Returns [n-groups row-groups]."
  [key-cols]
  (build-groups-hashed key-cols (combined-hashes key-cols)))

;; ════════════════════════════════════════════════════════════════════════
;; Aggregation accumulation — direct-access fast paths
;; Bypasses morsel layer entirely; reads the column's backing array.
;; ════════════════════════════════════════════════════════════════════════

(defn- count-rows! [^ints row-groups ^longs counts ^long n]
  (loop [i (int 0)]
    (when (< i n)
      (let [g (aget row-groups i)]
        (aset counts g (unchecked-inc (aget counts g))))
      (recur (unchecked-inc i)))))

(defn- accumulate-i64-sum! [val-col ^ints row-groups ^longs sums]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)
        has-nulls (.has-nulls c)]
    (if has-nulls
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not= v col/NULL_I64)
              (let [g (aget row-groups i)]
                (aset sums g (unchecked-add (aget sums g) v)))))
          (recur (unchecked-inc i))))
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
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (when (not= (aget data (+ col-off i)) col/NULL_I64)
            (let [g (aget row-groups i)]
              (aset counts g (unchecked-inc (aget counts g)))))
          (recur (unchecked-inc i))))
      (count-rows! row-groups counts n))))

(defn- accumulate-i64-min! [val-col ^ints row-groups ^longs mins ^booleans seen]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not= v col/NULL_I64)
              (let [g (aget row-groups i)]
                (aset seen g true)
                (aset mins g (min (aget mins g) v)))))
          (recur (unchecked-inc i))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset mins g (min (aget mins g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-i64-max! [val-col ^ints row-groups ^longs maxs ^booleans seen]
  (let [^I64Column c val-col
        ^longs data (.data c)
        col-off (.offset c)
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not= v col/NULL_I64)
              (let [g (aget row-groups i)]
                (aset seen g true)
                (aset maxs g (max (aget maxs g) v)))))
          (recur (unchecked-inc i))))
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
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not= v col/NULL_I64)
              (let [g (aget row-groups i)]
                (aset sums g (unchecked-add (aget sums g) v))
                (aset counts g (unchecked-inc (aget counts g))))))
          (recur (unchecked-inc i))))
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
                (aset sums g (+ (aget sums g) v)))))
          (recur (unchecked-inc i))))
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
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (when (not (Double/isNaN (aget data (+ col-off i))))
            (let [g (aget row-groups i)]
              (aset counts g (unchecked-inc (aget counts g)))))
          (recur (unchecked-inc i))))
      (count-rows! row-groups counts n))))

(defn- accumulate-f64-min! [val-col ^ints row-groups ^doubles mins ^booleans seen]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not (Double/isNaN v))
              (let [g (aget row-groups i)]
                (aset seen g true)
                (aset mins g (min (aget mins g) v)))))
          (recur (unchecked-inc i))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset mins g (min (aget mins g) v)))
          (recur (unchecked-inc i)))))))

(defn- accumulate-f64-max! [val-col ^ints row-groups ^doubles maxs ^booleans seen]
  (let [^F64Column c val-col
        ^doubles data (.data c)
        col-off (.offset c)
        n (.len c)]
    (if (.has-nulls c)
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))]
            (when (not (Double/isNaN v))
              (let [g (aget row-groups i)]
                (aset seen g true)
                (aset maxs g (max (aget maxs g) v)))))
          (recur (unchecked-inc i))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset maxs g (max (aget maxs g) v)))
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
                (aset counts g (unchecked-inc (aget counts g))))))
          (recur (unchecked-inc i))))
      (loop [i (int 0)]
        (when (< i n)
          (let [v (aget data (+ col-off i))
                g (aget row-groups i)]
            (aset sums g (+ (aget sums g) v))
            (aset counts g (unchecked-inc (aget counts g))))
          (recur (unchecked-inc i)))))))

(defn- accumulate-obj-count! [val-col ^ints row-groups ^longs counts]
  (let [^objects data (obj-data val-col)
        off (obj-offset val-col)
        n (col/-len val-col)]
    (if (col/-has-nulls? val-col)
      (loop [i (int 0)]
        (when (< i n)
          (when (some? (aget data (+ off i)))
            (let [g (aget row-groups i)]
              (aset counts g (unchecked-inc (aget counts g)))))
          (recur (unchecked-inc i))))
      (count-rows! row-groups counts n))))

;; Groups that saw no non-null value get the null sentinel; returns whether
;; any group was filled (the result column's has-nulls flag).

(defn- fill-unseen-i64! [^longs arr ^booleans seen ^long n-groups]
  (loop [i (int 0), any false]
    (if (< i n-groups)
      (if (aget seen i)
        (recur (unchecked-inc i) any)
        (do (aset arr i col/NULL_I64)
            (recur (unchecked-inc i) true)))
      any)))

(defn- fill-unseen-f64! [^doubles arr ^booleans seen ^long n-groups]
  (loop [i (int 0), any false]
    (if (< i n-groups)
      (if (aget seen i)
        (recur (unchecked-inc i) any)
        (do (aset arr i Double/NaN)
            (recur (unchecked-inc i) true)))
      any)))

;; ════════════════════════════════════════════════════════════════════════
;; Aggregation result columns — shared by sequential and parallel paths
;; ════════════════════════════════════════════════════════════════════════

(defn- make-agg-column
  "Compute one aggregation output column over `val-col` given the per-row
   group assignment. Min/max/avg of a group with no non-null values is null."
  [val-col agg ^ints row-groups ^long n-groups]
  (let [tag (col/-type-tag val-col)
        has-nulls (boolean (col/-has-nulls? val-col))]
    (case tag
      (:sym :str :bool)
      (case agg
        :count (let [cnt (long-array n-groups)]
                 (if (= tag :bool)
                   ;; BoolColumn is non-nullable: count = group size
                   (count-rows! row-groups cnt (col/-len val-col))
                   (accumulate-obj-count! val-col row-groups cnt))
                 (I64Column. cnt n-groups 0 false))
        (throw (IllegalArgumentException.
                (str "Agg " agg " not supported for " tag " values"))))
      :i64
      (case agg
        :sum   (let [s (long-array n-groups)]
                 (accumulate-i64-sum! val-col row-groups s)
                 (I64Column. s n-groups 0 false))
        :count (let [c (long-array n-groups)]
                 (accumulate-i64-count! val-col row-groups c)
                 (I64Column. c n-groups 0 false))
        :min   (let [m (long-array n-groups)
                     seen (when has-nulls (boolean-array n-groups))]
                 (java.util.Arrays/fill m Long/MAX_VALUE)
                 (accumulate-i64-min! val-col row-groups m seen)
                 (I64Column. m n-groups 0
                             (boolean (and seen (fill-unseen-i64! m seen n-groups)))))
        :max   (let [m (long-array n-groups)
                     seen (when has-nulls (boolean-array n-groups))]
                 (java.util.Arrays/fill m Long/MIN_VALUE)
                 (accumulate-i64-max! val-col row-groups m seen)
                 (I64Column. m n-groups 0
                             (boolean (and seen (fill-unseen-i64! m seen n-groups)))))
        :avg   (let [s (long-array n-groups)
                     c (long-array n-groups)]
                 (accumulate-i64-avg! val-col row-groups s c)
                 (let [out (double-array n-groups)
                       any-null (loop [i (int 0), any false]
                                  (if (< i n-groups)
                                    (let [cnt (aget c i)]
                                      (if (zero? cnt)
                                        (do (aset out i Double/NaN)
                                            (recur (unchecked-inc i) true))
                                        (do (aset out i (/ (double (aget s i)) (double cnt)))
                                            (recur (unchecked-inc i) any))))
                                    any))]
                   (F64Column. out n-groups 0 (boolean any-null)))))
      :f64
      (case agg
        :sum   (let [s (double-array n-groups)]
                 (accumulate-f64-sum! val-col row-groups s)
                 (F64Column. s n-groups 0 false))
        :count (let [c (long-array n-groups)]
                 (accumulate-f64-count! val-col row-groups c)
                 (I64Column. c n-groups 0 false))
        :min   (let [m (double-array n-groups)
                     seen (when has-nulls (boolean-array n-groups))]
                 (java.util.Arrays/fill m Double/POSITIVE_INFINITY)
                 (accumulate-f64-min! val-col row-groups m seen)
                 (F64Column. m n-groups 0
                             (boolean (and seen (fill-unseen-f64! m seen n-groups)))))
        :max   (let [m (double-array n-groups)
                     seen (when has-nulls (boolean-array n-groups))]
                 (java.util.Arrays/fill m Double/NEGATIVE_INFINITY)
                 (accumulate-f64-max! val-col row-groups m seen)
                 (F64Column. m n-groups 0
                             (boolean (and seen (fill-unseen-f64! m seen n-groups)))))
        :avg   (let [s (double-array n-groups)
                     c (long-array n-groups)]
                 (accumulate-f64-avg! val-col row-groups s c)
                 (let [out (double-array n-groups)
                       any-null (loop [i (int 0), any false]
                                  (if (< i n-groups)
                                    (let [cnt (aget c i)]
                                      (if (zero? cnt)
                                        (do (aset out i Double/NaN)
                                            (recur (unchecked-inc i) true))
                                        (do (aset out i (/ (aget s i) (double cnt)))
                                            (recur (unchecked-inc i) any))))
                                    any))]
                   (F64Column. out n-groups 0 (boolean any-null)))))
      (throw (IllegalArgumentException.
              (str "Unsupported value type for agg: " tag))))))

;; ════════════════════════════════════════════════════════════════════════
;; Key column compression
;; ════════════════════════════════════════════════════════════════════════

(defn compress-key-column
  "Extract distinct group key values. Returns a column of length n-groups."
  [key-col ^ints row-groups ^long n-groups]
  (let [tag   (col/-type-tag key-col)
        nrows (col/-len key-col)
        seen  (boolean-array n-groups)]
    (case tag
      :i64
      (let [^I64Column c key-col
            ^longs data (.data c)
            off (.offset c)
            out (long-array n-groups)]
        (loop [row (int 0)]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when-not (aget seen g)
                (aset seen g true)
                (aset out g (aget data (+ off row)))))
            (recur (unchecked-inc row))))
        (I64Column. out n-groups 0 (.has-nulls c)))
      :f64
      (let [^F64Column c key-col
            ^doubles data (.data c)
            off (.offset c)
            out (double-array n-groups)]
        (loop [row (int 0)]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when-not (aget seen g)
                (aset seen g true)
                (aset out g (aget data (+ off row)))))
            (recur (unchecked-inc row))))
        (F64Column. out n-groups 0 (.has-nulls c)))
      (:sym :str)
      (let [^objects data (obj-data key-col)
            off (obj-offset key-col)
            out (object-array n-groups)]
        (loop [row (int 0)]
          (when (< row nrows)
            (let [g (aget row-groups row)]
              (when-not (aget seen g)
                (aset seen g true)
                (aset out g (aget data (+ off row)))))
            (recur (unchecked-inc row))))
        (if (= tag :sym)
          (SymColumn. out n-groups 0 (col/-has-nulls? key-col))
          (StrColumn. out n-groups 0 (col/-has-nulls? key-col))))
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
        [n-groups groups] (build-groups key-cols)
        n-groups (long n-groups)
        ^ints row-groups groups]
    (if (zero? n-groups)
      (tbl/table [] [])
      (let [result-keys (mapv #(compress-key-column % row-groups n-groups) key-cols)
            agg-cols    (mapv (fn [{:keys [agg col]}]
                                (make-agg-column (tbl/col table col) agg
                                                 row-groups n-groups))
                              aggs)]
        (tbl/table (vec (concat keys (mapv :out aggs)))
                   (vec (concat result-keys agg-cols)))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel radix-partitioned group-by
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long DEFAULT-PARALLELISM 4)

(defn- radix-bits
  "Number of radix bits for ~2× n-threads partitions (power of two)."
  ^long [^long n-threads]
  (let [target (* 2 n-threads)]
    (loop [bits 2]
      (if (>= (bit-shift-left 1 bits) target)
        (min bits 10)
        (recur (unchecked-inc bits))))))

(defn- gather-column
  "Materialize the rows part-rows[start..start+n-local) of column c into a
   fresh column. Gives partition workers contiguous, type-specialized data."
  [c ^ints part-rows ^long start ^long n-local]
  (case (col/-type-tag c)
    :i64 (let [^I64Column ic c
               ^longs data (.data ic)
               off (.offset ic)
               out (long-array n-local)]
           (loop [i (int 0)]
             (when (< i n-local)
               (aset out i (aget data (+ off (aget part-rows (+ start i)))))
               (recur (unchecked-inc i))))
           (I64Column. out n-local 0 (.has-nulls ic)))
    :f64 (let [^F64Column fc c
               ^doubles data (.data fc)
               off (.offset fc)
               out (double-array n-local)]
           (loop [i (int 0)]
             (when (< i n-local)
               (aset out i (aget data (+ off (aget part-rows (+ start i)))))
               (recur (unchecked-inc i))))
           (F64Column. out n-local 0 (.has-nulls fc)))
    :bool (let [^BoolColumn bc c
                ^bytes data (.data bc)
                off (.offset bc)
                out (byte-array n-local)]
            (loop [i (int 0)]
              (when (< i n-local)
                (aset out i (aget data (+ off (aget part-rows (+ start i)))))
                (recur (unchecked-inc i))))
            (BoolColumn. out n-local 0))
    (:sym :str)
    (let [^objects data (obj-data c)
          off (obj-offset c)
          out (object-array n-local)]
      (loop [i (int 0)]
        (when (< i n-local)
          (aset out i (aget data (+ off (aget part-rows (+ start i)))))
          (recur (unchecked-inc i))))
      (if (= :sym (col/-type-tag c))
        (SymColumn. out n-local 0 (col/-has-nulls? c))
        (StrColumn. out n-local 0 (col/-has-nulls? c))))
    (throw (IllegalArgumentException.
            (str "Unsupported column type: " (col/-type-tag c))))))

(defn- partition-local-group-by
  "Group one radix partition: rows part-rows[start..end) with matching
   combined hashes in part-hashes. Gathers partition-local columns, then
   reuses the sequential build/compress/aggregate kernels. Returns the
   result columns vector (keys followed by aggs), or nil for an empty
   partition. Runs on worker threads — no shared mutable state."
  [key-cols val-cols aggs ^ints part-rows ^longs part-hashes start end]
  (let [start (long start)
        end (long end)
        n-local (- end start)
        ^longs local-hashes (java.util.Arrays/copyOfRange part-hashes start end)
        local-keys (mapv #(gather-column % part-rows start n-local) key-cols)
        [n-groups groups] (build-groups-hashed local-keys local-hashes)
        n-groups (long n-groups)
        ^ints row-groups groups]
    (when (pos? n-groups)
      (let [result-keys (mapv #(compress-key-column % row-groups n-groups) local-keys)
            agg-cols (mapv (fn [val-col {:keys [agg]}]
                             (let [local-val (gather-column val-col part-rows start n-local)]
                               (make-agg-column local-val agg row-groups n-groups)))
                           val-cols aggs)]
        (vec (concat result-keys agg-cols))))))

(defn- concat-columns
  "Concatenate two result columns of the same type, preserving nullability."
  [acc-col part-col]
  (let [tag (col/-type-tag acc-col)
        n-acc (col/-len acc-col)
        n-part (col/-len part-col)
        n-new (+ n-acc n-part)
        has-nulls (boolean (or (col/-has-nulls? acc-col)
                               (col/-has-nulls? part-col)))]
    (case tag
      :i64
      (let [data (long-array n-new)]
        (System/arraycopy (.data ^I64Column acc-col) (.offset ^I64Column acc-col) data 0 n-acc)
        (System/arraycopy (.data ^I64Column part-col) (.offset ^I64Column part-col) data n-acc n-part)
        (I64Column. data n-new 0 has-nulls))
      :f64
      (let [data (double-array n-new)]
        (System/arraycopy (.data ^F64Column acc-col) (.offset ^F64Column acc-col) data 0 n-acc)
        (System/arraycopy (.data ^F64Column part-col) (.offset ^F64Column part-col) data n-acc n-part)
        (F64Column. data n-new 0 has-nulls))
      (:sym :str)
      (let [data (object-array n-new)]
        (System/arraycopy (obj-data acc-col) (obj-offset acc-col) data 0 n-acc)
        (System/arraycopy (obj-data part-col) (obj-offset part-col) data n-acc n-part)
        (if (= tag :sym)
          (SymColumn. data n-new 0 has-nulls)
          (StrColumn. data n-new 0 has-nulls)))
      (throw (IllegalArgumentException. (str "Bad merge type: " tag))))))

(defn parallel-group-by
  "Parallel radix-partitioned group-by.
   Partitions rows by the high radix bits of the combined key hash (disjoint
   from the low bits used for hash-table slots), then processes each
   partition independently in core.async threads. Results are concatenated —
   no merging needed since partitions are disjoint.

   Usage: (parallel-group-by table :keys [:region] :aggs [{:agg :sum :col :amount :out :total}])

   Options:
     :n-threads — number of worker threads (default 4)"
  [table & {:keys [keys aggs n-threads]
            :or {n-threads DEFAULT-PARALLELISM}}]
  (let [key-cols (mapv #(tbl/col table %) keys)
        _ (assert (seq key-cols) "At least one key column required")
        nrows (long (col/-len (first key-cols)))
        _ (doseq [kc (rest key-cols)]
            (assert (= nrows (col/-len kc)) "All key columns must have same length"))
        schema (vec (concat keys (mapv :out aggs)))
        val-cols (mapv #(tbl/col table (:col %)) aggs)]
    (if (zero? nrows)
      (tbl/table [] [])
      (let [rbits   (radix-bits n-threads)
            n-parts (int (bit-shift-left 1 rbits))
            shift   (- 64 rbits)
            hashes  (combined-hashes key-cols)
            ;; histogram → prefix sum → scatter (no per-row allocation)
            counts  (int-array n-parts)
            _ (loop [i (int 0)]
                (when (< i nrows)
                  (let [p (unsigned-bit-shift-right (aget hashes i) shift)]
                    (aset counts p (unchecked-inc (aget counts p))))
                  (recur (unchecked-inc i))))
            offsets (int-array (inc n-parts))
            _ (loop [p (int 0), acc (int 0)]
                (when (< p n-parts)
                  (aset offsets (unchecked-inc p) (unchecked-add-int acc (aget counts p)))
                  (recur (unchecked-inc p) (unchecked-add-int acc (aget counts p)))))
            part-rows   (int-array nrows)
            part-hashes (long-array nrows)
            cursor (java.util.Arrays/copyOf offsets n-parts)
            _ (loop [i (int 0)]
                (when (< i nrows)
                  (let [hv (aget hashes i)
                        p (unsigned-bit-shift-right hv shift)
                        idx (aget cursor p)]
                    (aset part-rows idx i)
                    (aset part-hashes idx hv)
                    (aset cursor p (unchecked-inc idx)))
                  (recur (unchecked-inc i))))
            result-chan (a/chan n-parts)
            active (loop [p 0, n 0]
                     (if (< p n-parts)
                       (recur (inc p)
                              (if (> (aget offsets (inc p)) (aget offsets p)) (inc n) n))
                       n))]
        (dotimes [p n-parts]
          (let [start (aget offsets p)
                end (aget offsets (inc p))]
            (when (> end start)
              (a/thread
                (a/>!! result-chan
                       (try
                         (partition-local-group-by key-cols val-cols aggs
                                                   part-rows part-hashes start end)
                         (catch Throwable t t)))))))
        (loop [remaining (long active)
               acc-cols nil]
          (if (zero? remaining)
            (if acc-cols
              (tbl/table schema acc-cols)
              (tbl/table schema
                         (vec (repeat (count schema)
                                      (I64Column. (long-array 0) 0 0 false)))))
            (let [result (a/<!! result-chan)]
              (cond
                (instance? Throwable result) (throw result)
                (nil? result) (recur (dec remaining) acc-cols)
                :else (recur (dec remaining)
                             (if (nil? acc-cols)
                               result
                               (mapv concat-columns acc-cols result)))))))))))

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
   non-null values are null.

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
              st (get acc k {:sum 0.0 :lsum 0 :cnt 0
                             :min Double/POSITIVE_INFINITY
                             :max Double/NEGATIVE_INFINITY})
              st (if (nil? v)
                   st
                   (let [d (double v)]
                     {:sum (+ (double (:sum st)) d)
                      :lsum (if (= vtag :i64)
                              (+ (long (:lsum st)) (long v))
                              0)
                      :cnt (inc (long (:cnt st)))
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
                         :sum   (when (pos? (long (:cnt st)))
                                  (if (= vtag :i64) (:lsum st) (:sum st)))
                         :count (:cnt st)
                         :avg   (when (pos? (long (:cnt st)))
                                  (/ (double (:sum st)) (double (:cnt st))))
                         :min   (when (pos? (long (:cnt st))) (:min st))
                         :max   (when (pos? (long (:cnt st))) (:max st)))))
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
