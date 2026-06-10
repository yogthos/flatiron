(ns flatiron.agg
  "Aggregation functions on columns.
   Single-pass morsel reduction — type dispatch once, then tight loop
   with unboxed accumulator."
  (:require [flatiron.column :as col]
            [flatiron.morsel :as m]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 aggregations
;; ════════════════════════════════════════════════════════════════════════

(defn i64-sum
  "Sum of an I64 column. Returns long. Nulls are skipped."
  ^long [col]
  (let [ms   (m/i64-morsel-source col)
        buf  (long-array m/MORSEL-SIZE)
        n    (m/morsel-count ms)
        has-n (col/-has-nulls? col)]
    (if (and has-n (zero? n))
      0
      (let [null-sent col/NULL_I64]
        (loop [total 0]
          (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
            (if (zero? cnt)
              total
              (recur (if has-n
                       (loop [i 0, acc total]
                         (if (< i cnt)
                           (let [v (aget buf i)]
                             (recur (unchecked-inc i)
                                    (if (= v null-sent) acc (unchecked-add acc v))))
                           acc))
                       (loop [i 0, acc total]
                         (if (< i cnt)
                           (recur (unchecked-inc i) (unchecked-add acc (aget buf i)))
                           acc)))))))))))

(defn i64-min
  "Minimum value in an I64 column. Returns nil if all null or empty."
  [col]
  (let [ms   (m/i64-morsel-source col)
        buf  (long-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)
        null-sent col/NULL_I64]
    (loop [best Long/MAX_VALUE, found false]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? cnt)
          (when found best)
          (let [[new-best new-found]
                (loop [i 0, b best, f found]
                  (if (< i cnt)
                    (let [v (aget buf i)]
                      (if (and has-n (= v null-sent))
                        (recur (unchecked-inc i) b f)
                        (recur (unchecked-inc i) (min b v) true)))
                    [b f]))]
            (recur new-best new-found)))))))

(defn i64-max
  "Maximum value in an I64 column. Returns nil if all null or empty."
  [col]
  (let [ms   (m/i64-morsel-source col)
        buf  (long-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)
        null-sent col/NULL_I64]
    (loop [best Long/MIN_VALUE, found false]
      (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? cnt)
          (when found best)
          (let [[new-best new-found]
                (loop [i 0, b best, f found]
                  (if (< i cnt)
                    (let [v (aget buf i)]
                      (if (and has-n (= v null-sent))
                        (recur (unchecked-inc i) b f)
                        (recur (unchecked-inc i) (max b v) true)))
                    [b f]))]
            (recur new-best new-found)))))))

(defn i64-avg
  "Average of an I64 column. Returns double. Nulls are skipped. Returns nil if no non-null rows."
  [col]
  (let [ms   (m/i64-morsel-source col)
        buf  (long-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)
        null-sent col/NULL_I64]
    (loop [sum 0.0, cnt 0]
      (let [n (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? n)
          (when (pos? cnt) (/ sum (double cnt)))
          (let [[new-sum new-cnt]
                (loop [i 0, s sum, c cnt]
                  (if (< i n)
                    (let [v (aget buf i)]
                      (if (and has-n (= v null-sent))
                        (recur (unchecked-inc i) s c)
                        (recur (unchecked-inc i) (+ s (double v)) (unchecked-inc c))))
                    [s c]))]
            (recur new-sum new-cnt)))))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 aggregations
;; ════════════════════════════════════════════════════════════════════════

(defn f64-sum
  "Sum of an F64 column. Returns double. Nulls (NaN) are skipped."
  ^double [col]
  (let [ms   (m/f64-morsel-source col)
        buf  (double-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)]
    (loop [total 0.0]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? cnt)
          total
          (recur (if has-n
                   (loop [i 0, acc total]
                     (if (< i cnt)
                       (let [v (aget buf i)]
                         (recur (unchecked-inc i)
                                (if (Double/isNaN v) acc (+ acc v))))
                       acc))
                   (loop [i 0, acc total]
                     (if (< i cnt)
                       (recur (unchecked-inc i) (+ acc (aget buf i)))
                       acc)))))))))

(defn f64-min
  "Minimum value in an F64 column. Returns nil if all null or empty."
  [col]
  (let [ms   (m/f64-morsel-source col)
        buf  (double-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)]
    (loop [best Double/MAX_VALUE, found false]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? cnt)
          (when found best)
          (let [[new-best new-found]
                (loop [i 0, b best, f found]
                  (if (< i cnt)
                    (let [v (aget buf i)]
                      (if (and has-n (Double/isNaN v))
                        (recur (unchecked-inc i) b f)
                        (recur (unchecked-inc i) (min b v) true)))
                    [b f]))]
            (recur new-best new-found)))))))

(defn f64-max
  "Maximum value in an F64 column. Returns nil if all null or empty."
  [col]
  (let [ms   (m/f64-morsel-source col)
        buf  (double-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)]
    (loop [best (- Double/MAX_VALUE), found false]
      (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? cnt)
          (when found best)
          (let [[new-best new-found]
                (loop [i 0, b best, f found]
                  (if (< i cnt)
                    (let [v (aget buf i)]
                      (if (and has-n (Double/isNaN v))
                        (recur (unchecked-inc i) b f)
                        (recur (unchecked-inc i) (max b v) true)))
                    [b f]))]
            (recur new-best new-found)))))))

(defn f64-avg
  "Average of an F64 column. Returns double. Nulls (NaN) are skipped. Returns nil if no non-null rows."
  [col]
  (let [ms   (m/f64-morsel-source col)
        buf  (double-array m/MORSEL-SIZE)
        has-n (col/-has-nulls? col)]
    (loop [sum 0.0, cnt 0]
      (let [n (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
        (if (zero? n)
          (when (pos? cnt) (/ sum (double cnt)))
          (let [[new-sum new-cnt]
                (loop [i 0, s sum, c cnt]
                  (if (< i n)
                    (let [v (aget buf i)]
                      (if (and has-n (Double/isNaN v))
                        (recur (unchecked-inc i) s c)
                        (recur (unchecked-inc i) (+ s v) (unchecked-inc c))))
                    [s c]))]
            (recur new-sum new-cnt)))))))

;; ════════════════════════════════════════════════════════════════════════
;; Count
;; ════════════════════════════════════════════════════════════════════════

(defn count-non-null
  "Count of non-null rows. Returns long."
  ^long [col]
  (let [tag (col/-type-tag col)]
    (case tag
      :i64  (let [ms   (m/i64-morsel-source col)
                  buf  (long-array m/MORSEL-SIZE)
                  has-n (col/-has-nulls? col)
                  null-sent col/NULL_I64]
              (if has-n
                (loop [total 0]
                  (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
                    (if (zero? cnt)
                      total
                      (recur (loop [i 0, acc total]
                               (if (< i cnt)
                                 (let [v (aget buf i)]
                                   (recur (unchecked-inc i)
                                          (if (= v null-sent) acc (unchecked-inc acc))))
                                 acc))))))
                (m/morsel-count col)))
      :f64  (let [ms   (m/f64-morsel-source col)
                  buf  (double-array m/MORSEL-SIZE)
                  has-n (col/-has-nulls? col)]
              (if has-n
                (loop [total 0]
                  (let [cnt (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)]
                    (if (zero? cnt)
                      total
                      (recur (loop [i 0, acc total]
                               (if (< i cnt)
                                 (let [v (aget buf i)]
                                   (recur (unchecked-inc i)
                                          (if (Double/isNaN v) acc (unchecked-inc acc))))
                                 acc))))))
                (m/morsel-count col)))
      ;; Non-nullable types: count = len
      (col/-len col))))
