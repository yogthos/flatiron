(ns flatiron.parallel
  "Parallel execution using core.async threads.
   Each worker runs in a real OS thread (a/thread), not a go block,
   because the work is CPU-bound primitive array loops.
   
   No Java interop for concurrency — pure core.async."
  (:require [clojure.core.async :as a]
            [flatiron.column :as col]
            [flatiron.group :as group]
            [flatiron.morsel :as m]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column BoolColumn]))

(def ^:const MORSEL-SIZE m/MORSEL-SIZE)
(def ^:const DEFAULT-PARALLELISM 4)

;; ════════════════════════════════════════════════════════════════════════
;; Task splitting
;; ════════════════════════════════════════════════════════════════════════

(defn- task-ranges
  "Split n rows into n-tasks roughly equal ranges.
   Returns [[start end] ...]"
  [^long n ^long n-tasks]
  (let [chunk (quot (+ n n-tasks -1) n-tasks)]
    (loop [ranges [], start 0]
      (if (>= start n)
        (vec ranges)
        (let [end (min (+ start chunk) n)]
          (recur (conj ranges [start end]) end))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel sum
;; ════════════════════════════════════════════════════════════════════════

(defn parallel-i64-sum
  "Sum an I64 column in parallel using core.async threads.
   Each worker processes a range of rows independently.
   Returns a long."
  ([col] (parallel-i64-sum col DEFAULT-PARALLELISM))
  ([col n-threads]
   (let [n-rows (col/-len col)
         n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n-rows n-tasks)
         result-chan (a/chan n-tasks)
         has-nulls (col/-has-nulls? col)
         null-sent col/NULL_I64
         ^I64Column icol col]
     (doseq [[start end] ranges :when (< start end)]
       (a/thread
         (loop [i start, total 0]
           (if (< i end)
             (let [v (col/-get-long icol i)]
               (recur (unchecked-inc i)
                      (if (and has-nulls (= v null-sent))
                        total
                        (unchecked-add total v))))
             (a/>!! result-chan total)))))
     (loop [remaining (count ranges), total 0]
       (if (zero? remaining)
         total
         (let [t (a/<!! result-chan)]
           (recur (dec remaining) (unchecked-add total t))))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel count
;; ════════════════════════════════════════════════════════════════════════

(defn parallel-count-non-null
  "Count non-null rows in a column in parallel."
  ([col] (parallel-count-non-null col DEFAULT-PARALLELISM))
  ([col n-threads]
   (let [n-rows (col/-len col)
         n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n-rows n-tasks)
         result-chan (a/chan n-tasks)
         has-nulls (col/-has-nulls? col)]
     (case (col/-type-tag col)
       :i64
       (let [null-sent col/NULL_I64
             ^I64Column icol col]
         (doseq [[start end] ranges :when (< start end)]
           (a/thread
             (loop [i start, cnt 0]
               (if (< i end)
                 (let [v (col/-get-long icol i)]
                   (recur (unchecked-inc i)
                          (if (and has-nulls (= v null-sent)) cnt (unchecked-inc cnt))))
                 (a/>!! result-chan cnt)))))
         (loop [remaining (count ranges), total 0]
           (if (zero? remaining)
             total
             (let [c (a/<!! result-chan)]
               (recur (dec remaining) (unchecked-add total c))))))
       :f64
       (let [^F64Column fcol col]
         (doseq [[start end] ranges :when (< start end)]
           (a/thread
             (loop [i start, cnt 0]
               (if (< i end)
                 (let [v (col/-get-double fcol i)]
                   (recur (unchecked-inc i)
                          (if (and has-nulls (Double/isNaN v)) cnt (unchecked-inc cnt))))
                 (a/>!! result-chan cnt)))))
         (loop [remaining (count ranges), total 0]
           (if (zero? remaining)
             total
             (let [c (a/<!! result-chan)]
               (recur (dec remaining) (unchecked-add total c))))))
       ;; Non-nullable: every row counts
       n-rows))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel filter: count passing rows
;; ════════════════════════════════════════════════════════════════════════

(defn parallel-filter-count
  "Count how many rows pass a filter predicate in parallel.
   pred-col is a BoolColumn (byte array, 1=pass 0=fail)."
  ([pred-col] (parallel-filter-count pred-col DEFAULT-PARALLELISM))
  ([pred-col n-threads]
   (let [n-rows (col/-len pred-col)
         n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n-rows n-tasks)
         result-chan (a/chan n-tasks)
         ^BoolColumn bcol pred-col]
     (doseq [[start end] ranges :when (< start end)]
       (a/thread
         (loop [i start, cnt 0]
           (if (< i end)
             (recur (unchecked-inc i)
                    (if (col/-get-obj bcol i) (unchecked-inc cnt) cnt))
             (a/>!! result-chan cnt)))))
     (loop [remaining (count ranges), total 0]
       (if (zero? remaining)
         total
         (let [c (a/<!! result-chan)]
           (recur (dec remaining) (unchecked-add total c))))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel min/max
;; ════════════════════════════════════════════════════════════════════════

(defn parallel-i64-min
  "Find the minimum value in an I64 column in parallel."
  ([col] (parallel-i64-min col DEFAULT-PARALLELISM))
  ([col n-threads]
   (let [n-rows (col/-len col)
         n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n-rows n-tasks)
         result-chan (a/chan n-tasks)
         has-nulls (col/-has-nulls? col)
         null-sent col/NULL_I64
         ^I64Column icol col]
     (doseq [[start end] ranges :when (< start end)]
       (a/thread
         (loop [i start, best Long/MAX_VALUE, found false]
           (if (< i end)
             (let [v (col/-get-long icol i)]
               (recur (unchecked-inc i)
                      (if (and has-nulls (= v null-sent)) best (min best v))
                      (or found (not (and has-nulls (= v null-sent))))))
             (a/>!! result-chan [best found])))))
     (loop [remaining (count ranges), best Long/MAX_VALUE, found false]
       (if (zero? remaining)
         (when found best)
         (let [[b f] (a/<!! result-chan)]
           (recur (dec remaining)
                  (if f (min best b) best)
                  (or found f))))))))

(defn parallel-i64-max
  "Find the maximum value in an I64 column in parallel."
  ([col] (parallel-i64-max col DEFAULT-PARALLELISM))
  ([col n-threads]
   (let [n-rows (col/-len col)
         n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n-rows n-tasks)
         result-chan (a/chan n-tasks)
         has-nulls (col/-has-nulls? col)
         null-sent col/NULL_I64
         ^I64Column icol col]
     (doseq [[start end] ranges :when (< start end)]
       (a/thread
         (loop [i start, best Long/MIN_VALUE, found false]
           (if (< i end)
             (let [v (col/-get-long icol i)]
               (recur (unchecked-inc i)
                      (if (and has-nulls (= v null-sent)) best (max best v))
                      (or found (not (and has-nulls (= v null-sent))))))
             (a/>!! result-chan [best found])))))
     (loop [remaining (count ranges), best Long/MIN_VALUE, found false]
       (if (zero? remaining)
         (when found best)
         (let [[b f] (a/<!! result-chan)]
           (recur (dec remaining)
                  (if f (max best b) best)
                  (or found f))))))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel element-wise arithmetic
;; ════════════════════════════════════════════════════════════════════════

(defn parallel-i64-add
  "Element-wise add two I64 columns in parallel. Returns new I64Column."
  ([a-col b-col] (parallel-i64-add a-col b-col DEFAULT-PARALLELISM))
  ([a-col b-col n-threads]
   (let [n (col/-len a-col)
         n-tasks (long (min n-threads (max 1 (quot (+ n MORSEL-SIZE -1) MORSEL-SIZE))))
         ranges (task-ranges n n-tasks)
         result-chan (a/chan n-tasks)
         has-nulls (or (col/-has-nulls? a-col) (col/-has-nulls? b-col))
         null-sent col/NULL_I64
         out (long-array n)
         ^I64Column a a-col
         ^I64Column b b-col]
     (doseq [[start end] ranges :when (< start end)]
       (a/thread
         (let [len   (- end start)
               local (long-array len)]
            (loop [i start]
              (if (< i end)
               (let [av (col/-get-long a i)
                     bv (col/-get-long b i)]
                 ;; Preserve row positions: write the null sentinel in place
                 ;; rather than compacting, so the result stays aligned with
                 ;; the inputs (matching arith/i64-add).
                 (aset local (- i start)
                       (if (and has-nulls (or (= av null-sent) (= bv null-sent)))
                         null-sent
                         (unchecked-add av bv)))
                 (recur (unchecked-inc i)))
                (a/>!! result-chan [start local len]))))))
     ;; Gather results into output array
     (loop [remaining (count ranges)]
       (when (pos? remaining)
         (let [[start local cnt] (a/<!! result-chan)]
           (System/arraycopy local 0 out start cnt)
           (recur (dec remaining)))))
     (I64Column. out n 0 has-nulls))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel group-by
;; ════════════════════════════════════════════════════════════════════════

(defn- parallel-ranges [^long n-rows ^long n-threads]
  (let [n-tasks (long (min n-threads (max 1 (quot (+ n-rows MORSEL-SIZE -1) MORSEL-SIZE))))]
    (filterv (fn [[s e]] (< s e)) (task-ranges n-rows n-tasks))))

(defn- par-collect
  "Run thread-fn over each [start end] range on its own thread.
   Returns a vector of the per-range results."
  [ranges thread-fn]
  (let [ch (a/chan (count ranges))]
    (doseq [[s e] ranges]
      (a/thread (a/>!! ch (thread-fn s e))))
    (loop [r (count ranges), acc []]
      (if (zero? r) acc (recur (dec r) (conj acc (a/<!! ch)))))))

(defn- merge-long-sum [^long n-groups parts]
  (let [out (long-array n-groups)]
    (doseq [^longs p parts]
      (dotimes [g n-groups] (aset out g (unchecked-add (aget out g) (aget p g)))))
    out))

(defn- par-group-agg
  "Compute one aggregation column for a parallel group-by. `row-groups` maps
   each row to its group id; the accumulation is split across `ranges`, each
   range building a partial per-group array that is then merged.
   Matches the semantics of group/group-by."
  [val-col agg n-groups ^ints row-groups ranges]
  (let [n-groups (long n-groups)
        tag   (col/-type-tag val-col)
        has-n (col/-has-nulls? val-col)]
    (case tag
      :i64
      (case agg
        :sum   (I64Column. (merge-long-sum n-groups
                             (par-collect ranges
                               (fn [s e]
                                 (let [loc (long-array n-groups)]
                                   (loop [i s]
                                     (when (< i e)
                                       (let [v (col/-get-long val-col i)]
                                         (when (or (not has-n) (not= v col/NULL_I64))
                                           (let [g (aget row-groups i)]
                                             (aset loc g (unchecked-add (aget loc g) v)))))
                                       (recur (unchecked-inc i))))
                                   loc))))
                           n-groups 0 false)
        :count (I64Column. (merge-long-sum n-groups
                             (par-collect ranges
                               (fn [s e]
                                 (let [loc (long-array n-groups)]
                                   (loop [i s]
                                     (when (< i e)
                                       (let [v (col/-get-long val-col i)]
                                         (when (or (not has-n) (not= v col/NULL_I64))
                                           (let [g (aget row-groups i)]
                                             (aset loc g (unchecked-inc (aget loc g))))))
                                       (recur (unchecked-inc i))))
                                   loc))))
                           n-groups 0 false)
        :min   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [loc (long-array n-groups)]
                                 (java.util.Arrays/fill loc Long/MAX_VALUE)
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-long val-col i)]
                                       (when (or (not has-n) (not= v col/NULL_I64))
                                         (let [g (aget row-groups i)]
                                           (aset loc g (min (aget loc g) v)))))
                                     (recur (unchecked-inc i))))
                                 loc)))
                     out (long-array n-groups)]
                 (java.util.Arrays/fill out Long/MAX_VALUE)
                 (doseq [^longs p parts] (dotimes [g n-groups] (aset out g (min (aget out g) (aget p g)))))
                 (I64Column. out n-groups 0 false))
        :max   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [loc (long-array n-groups)]
                                 (java.util.Arrays/fill loc Long/MIN_VALUE)
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-long val-col i)]
                                       (when (or (not has-n) (not= v col/NULL_I64))
                                         (let [g (aget row-groups i)]
                                           (aset loc g (max (aget loc g) v)))))
                                     (recur (unchecked-inc i))))
                                 loc)))
                     out (long-array n-groups)]
                 (java.util.Arrays/fill out Long/MIN_VALUE)
                 (doseq [^longs p parts] (dotimes [g n-groups] (aset out g (max (aget out g) (aget p g)))))
                 (I64Column. out n-groups 0 false))
        :avg   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [sums (long-array n-groups)
                                     cnts (long-array n-groups)]
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-long val-col i)]
                                       (when (or (not has-n) (not= v col/NULL_I64))
                                         (let [g (aget row-groups i)]
                                           (aset sums g (unchecked-add (aget sums g) v))
                                           (aset cnts g (unchecked-inc (aget cnts g))))))
                                     (recur (unchecked-inc i))))
                                 [sums cnts])))
                     sum (merge-long-sum n-groups (map first parts))
                     cnt (merge-long-sum n-groups (map second parts))
                     out (double-array n-groups)]
                 (dotimes [g n-groups]
                   (aset out g (if (zero? (aget cnt g))
                                 Double/NaN
                                 (/ (double (aget sum g)) (double (aget cnt g))))))
                 (F64Column. out n-groups 0 false))
        (throw (IllegalArgumentException. (str "Unknown agg: " agg))))
      :f64
      (case agg
        :sum   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [loc (double-array n-groups)]
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-double val-col i)]
                                       (when-not (Double/isNaN v)
                                         (let [g (aget row-groups i)]
                                           (aset loc g (+ (aget loc g) v)))))
                                     (recur (unchecked-inc i))))
                                 loc)))
                     out (double-array n-groups)]
                 (doseq [^doubles p parts] (dotimes [g n-groups] (aset out g (+ (aget out g) (aget p g)))))
                 (F64Column. out n-groups 0 false))
        :count (I64Column. (merge-long-sum n-groups
                             (par-collect ranges
                               (fn [s e]
                                 (let [loc (long-array n-groups)]
                                   (loop [i s]
                                     (when (< i e)
                                       (let [v (col/-get-double val-col i)]
                                         (when-not (Double/isNaN v)
                                           (let [g (aget row-groups i)]
                                             (aset loc g (unchecked-inc (aget loc g))))))
                                       (recur (unchecked-inc i))))
                                   loc))))
                           n-groups 0 false)
        :min   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [loc (double-array n-groups)]
                                 (java.util.Arrays/fill loc Double/MAX_VALUE)
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-double val-col i)]
                                       (when-not (Double/isNaN v)
                                         (let [g (aget row-groups i)]
                                           (aset loc g (min (aget loc g) v)))))
                                     (recur (unchecked-inc i))))
                                 loc)))
                     out (double-array n-groups)]
                 (java.util.Arrays/fill out Double/MAX_VALUE)
                 (doseq [^doubles p parts] (dotimes [g n-groups] (aset out g (min (aget out g) (aget p g)))))
                 (F64Column. out n-groups 0 false))
        :max   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [loc (double-array n-groups)]
                                 (java.util.Arrays/fill loc (- Double/MAX_VALUE))
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-double val-col i)]
                                       (when-not (Double/isNaN v)
                                         (let [g (aget row-groups i)]
                                           (aset loc g (max (aget loc g) v)))))
                                     (recur (unchecked-inc i))))
                                 loc)))
                     out (double-array n-groups)]
                 (java.util.Arrays/fill out (- Double/MAX_VALUE))
                 (doseq [^doubles p parts] (dotimes [g n-groups] (aset out g (max (aget out g) (aget p g)))))
                 (F64Column. out n-groups 0 false))
        :avg   (let [parts (par-collect ranges
                             (fn [s e]
                               (let [sums (double-array n-groups)
                                     cnts (long-array n-groups)]
                                 (loop [i s]
                                   (when (< i e)
                                     (let [v (col/-get-double val-col i)]
                                       (when-not (Double/isNaN v)
                                         (let [g (aget row-groups i)]
                                           (aset sums g (+ (aget sums g) v))
                                           (aset cnts g (unchecked-inc (aget cnts g))))))
                                     (recur (unchecked-inc i))))
                                 [sums cnts])))
                     sum (double-array n-groups)
                     cnt (merge-long-sum n-groups (map second parts))
                     out (double-array n-groups)]
                 (doseq [[^doubles ps _] parts] (dotimes [g n-groups] (aset sum g (+ (aget sum g) (aget ps g)))))
                 (dotimes [g n-groups]
                   (aset out g (if (zero? (aget cnt g))
                                 Double/NaN
                                 (/ (aget sum g) (double (aget cnt g))))))
                 (F64Column. out n-groups 0 false))
        (throw (IllegalArgumentException. (str "Unknown agg: " agg))))
      (:sym :str :bool)
      (case agg
        :count (I64Column. (merge-long-sum n-groups
                             (par-collect ranges
                               (fn [s e]
                                 (let [loc (long-array n-groups)]
                                   (loop [i s]
                                     (when (< i e)
                                       (let [v (col/-get-obj val-col i)]
                                         (when (or (not has-n) (some? v))
                                           (let [g (aget row-groups i)]
                                             (aset loc g (unchecked-inc (aget loc g))))))
                                       (recur (unchecked-inc i))))
                                   loc))))
                           n-groups 0 false)
        (throw (IllegalArgumentException. (str "Agg " agg " not supported for " tag " values"))))
      (throw (IllegalArgumentException. (str "Unsupported value type for agg: " tag))))))

(defn parallel-group-by
  "Parallel group-by aggregation over a table. Produces the same result as the
   single-threaded group/group-by, splitting the per-group accumulation across
   worker threads.

   Usage:
     (parallel-group-by table
       :keys [:Region]
       :aggs [{:agg :sum :col :Qty :out :total}])"
  [table & {:keys [keys aggs threads] :or {threads DEFAULT-PARALLELISM}}]
  (let [key-cols (mapv #(tbl/col table %) keys)
        _ (assert (seq key-cols) "At least one key column required")
        nrows (col/-len (first key-cols))
        _ (doseq [kc (rest key-cols)]
            (assert (= nrows (col/-len kc)) "All key columns must have same length"))
        [n-groups row-groups] (group/build-groups key-cols)]
    (if (zero? n-groups)
      (tbl/table [] [])
      (let [ranges      (parallel-ranges nrows threads)
            result-keys (mapv #(group/compress-key-column % row-groups n-groups) key-cols)
            key-names   (vec keys)
            agg-cols    (mapv (fn [{:keys [agg col]}]
                                (par-group-agg (tbl/col table col) agg n-groups row-groups ranges))
                              aggs)
            agg-names   (mapv :out aggs)]
        (tbl/table (vec (concat key-names agg-names))
                   (vec (concat result-keys agg-cols)))))))
