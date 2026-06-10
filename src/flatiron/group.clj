(ns flatiron.group
  "Group-by aggregation using open-addressing hash tables.
   Two-pass single-threaded: Pass 1 builds group assignment,
   Pass 2 accumulates aggregation values per group."
  (:require [flatiron.column :as col]
            [flatiron.hash :as h]
            [flatiron.morsel :as m]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column SymColumn StrColumn]))

;; ════════════════════════════════════════════════════════════════════════
;; Hash table build
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long HT-EMPTY h/HT-EMPTY)

(defn- prehash-i64-column ^longs [key-col]
  (let [nrows (col/-len key-col)
        out   (long-array nrows)
        ms    (m/i64-morsel-source key-col)
        buf   (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (aset out (+ off i) (h/hash-i64 (aget buf i)))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))
    out))

(defn- prehash-f64-column ^longs [key-col]
  (let [nrows (col/-len key-col)
        out   (long-array nrows)
        ms    (m/f64-morsel-source key-col)
        buf   (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (aset out (+ off i) (h/hash-f64 (aget buf i)))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))
    out))

(defn- prehash-obj-column ^longs [key-col]
  (let [nrows (col/-len key-col)
        out   (long-array nrows)
        ms    (m/obj-morsel-source key-col)
        buf   (object-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-obj! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [obj (aget buf i)]
                (aset out (+ off i)
                      (h/hash-i64 (if (nil? obj) 0 (long (.hashCode obj))))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))
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
;; Aggregation accumulation
;; ════════════════════════════════════════════════════════════════════════

(defn- accumulate-i64-sum! [val-col row-groups ^longs sums]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/i64-morsel-source val-col)
        buf (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (if has-nulls
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (when (not= v col/NULL_I64)
                    (aset sums g (unchecked-add (aget sums g) v))))
                (recur (unchecked-inc i))))
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (aset sums g (unchecked-add (aget sums g) v))
                  (recur (unchecked-inc i))))))
          (recur (+ off cnt)))))))

(defn- accumulate-i64-count! [val-col row-groups ^longs counts]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/i64-morsel-source val-col)
        buf (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (if has-nulls
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (when (not= v col/NULL_I64)
                    (aset counts g (unchecked-inc (aget counts g)))))
                (recur (unchecked-inc i))))
            (loop [i 0]
              (when (< i cnt)
                (let [g (aget row-groups (+ off i))]
                  (aset counts g (unchecked-inc (aget counts g)))
                  (recur (unchecked-inc i))))))
          (recur (+ off cnt)))))))

(defn- accumulate-i64-min! [val-col row-groups ^longs mins]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/i64-morsel-source val-col)
        buf (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (or (not has-nulls) (not= v col/NULL_I64))
                  (aset mins g (min (aget mins g) v))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

(defn- accumulate-i64-max! [val-col row-groups ^longs maxs]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/i64-morsel-source val-col)
        buf (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (or (not has-nulls) (not= v col/NULL_I64))
                  (aset maxs g (max (aget maxs g) v))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

(defn- accumulate-i64-avg! [val-col row-groups ^longs sums ^longs counts]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/i64-morsel-source val-col)
        buf (long-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (or (not has-nulls) (not= v col/NULL_I64))
                  (aset sums g (unchecked-add (aget sums g) v))
                  (aset counts g (unchecked-inc (aget counts g)))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

;; F64 variants

(defn- accumulate-f64-sum! [val-col row-groups ^doubles sums]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/f64-morsel-source val-col)
        buf (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (if has-nulls
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (when (not (Double/isNaN v))
                    (aset sums g (+ (aget sums g) v))))
                (recur (unchecked-inc i))))
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (aset sums g (+ (aget sums g) v))
                  (recur (unchecked-inc i))))))
          (recur (+ off cnt)))))))

(defn- accumulate-f64-count! [val-col row-groups ^longs counts]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/f64-morsel-source val-col)
        buf (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (if has-nulls
            (loop [i 0]
              (when (< i cnt)
                (let [v (aget buf i)
                      g (aget row-groups (+ off i))]
                  (when (not (Double/isNaN v))
                    (aset counts g (unchecked-inc (aget counts g)))))
                (recur (unchecked-inc i))))
            (loop [i 0]
              (when (< i cnt)
                (let [g (aget row-groups (+ off i))]
                  (aset counts g (unchecked-inc (aget counts g)))
                  (recur (unchecked-inc i))))))
          (recur (+ off cnt)))))))

(defn- accumulate-f64-min! [val-col row-groups ^doubles mins]
  (let [ms  (m/f64-morsel-source val-col)
        buf (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (not (Double/isNaN v))
                  (aset mins g (min (aget mins g) v))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

(defn- accumulate-f64-max! [val-col row-groups ^doubles maxs]
  (let [ms  (m/f64-morsel-source val-col)
        buf (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (not (Double/isNaN v))
                  (aset maxs g (max (aget maxs g) v))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

(defn- accumulate-f64-avg! [val-col row-groups ^doubles sums ^longs counts]
  (let [has-nulls (col/-has-nulls? val-col)
        ms  (m/f64-morsel-source val-col)
        buf (double-array m/MORSEL-SIZE)]
    (loop [off 0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (when (pos? cnt)
          (loop [i 0]
            (when (< i cnt)
              (let [v (aget buf i)
                    g (aget row-groups (+ off i))]
                (when (or (not has-nulls) (not (Double/isNaN v)))
                  (aset sums g (+ (aget sums g) v))
                  (aset counts g (unchecked-inc (aget counts g)))))
              (recur (unchecked-inc i))))
          (recur (+ off cnt)))))))

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
