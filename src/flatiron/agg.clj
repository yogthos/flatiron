(ns flatiron.agg
  "Aggregation functions on columns.
   Direct-access fast paths read from column internals when no nulls present.
   Morsel path retained for nullable columns and filtered/selected data."
  (:require [flatiron.column :as col]
            [flatiron.morsel :as m])
  (:import [flatiron.column I64Column F64Column]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 aggregations
;; ════════════════════════════════════════════════════════════════════════

(defn i64-sum
  "Sum of an I64 column. Returns long. Nulls are skipped."
  ^long [col]
  (let [has-n (col/-has-nulls? col)]
    (if has-n
      (let [ms   (m/i64-morsel-source col)
            buf  (long-array m/MORSEL-SIZE)
            null-sent col/NULL_I64]
        (loop [total 0]
          (let [cnt (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
            (if (zero? cnt)
              total
              (recur (loop [i 0, acc total]
                       (if (< i cnt)
                         (let [v (aget buf i)]
                           (recur (unchecked-inc i)
                                  (if (= v null-sent) acc (unchecked-add acc v))))
                         acc)))))))
      (let [^I64Column c col
            ^longs data (.data c)
            off (.offset c)
            n (.len c)]
        (loop [i (int 0), total (long 0)]
          (if (< i n)
            (recur (unchecked-inc i) (unchecked-add total (aget data (+ off i))))
            total))))))

(defn i64-min
  "Minimum value in an I64 column. Returns nil if all null or empty."
  [col]
  (let [^I64Column c col
        ^longs data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (let [null-sent col/NULL_I64]
          (loop [i (int 0), best Long/MAX_VALUE, found false]
            (if (< i n)
              (let [v (aget data (+ off i))]
                (if (= v null-sent)
                  (recur (unchecked-inc i) best found)
                  (recur (unchecked-inc i) (min best v) true)))
              (when found best))))
        (loop [i (int 0), best (aget data off)]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (recur (unchecked-inc i) (if (< v best) v best)))
            best))))))

(defn i64-max
  "Maximum value in an I64 column. Returns nil if all null or empty."
  [col]
  (let [^I64Column c col
        ^longs data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (let [null-sent col/NULL_I64]
          (loop [i (int 0), best Long/MIN_VALUE, found false]
            (if (< i n)
              (let [v (aget data (+ off i))]
                (if (= v null-sent)
                  (recur (unchecked-inc i) best found)
                  (recur (unchecked-inc i) (max best v) true)))
              (when found best))))
        (loop [i (int 0), best (aget data off)]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (recur (unchecked-inc i) (if (> v best) v best)))
            best))))))

(defn i64-avg
  "Average of an I64 column. Returns double. Nulls are skipped."
  [col]
  (let [^I64Column c col
        ^longs data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (let [null-sent col/NULL_I64]
          (loop [i (int 0), sum (double 0.0), cnt (int 0)]
            (if (< i n)
              (let [v (aget data (+ off i))]
                (if (= v null-sent)
                  (recur (unchecked-inc i) sum cnt)
                  (recur (unchecked-inc i) (+ sum (double v)) (unchecked-inc cnt))))
              (if (pos? cnt) (/ sum (double cnt)) nil))))
        (loop [i (int 0), sum (double 0.0)]
          (if (< i n)
            (recur (unchecked-inc i) (+ sum (double (aget data (+ off i)))))
            (/ sum (double n))))))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 aggregations
;; ════════════════════════════════════════════════════════════════════════

(defn f64-sum
  "Sum of an F64 column. Returns double. Nulls (NaN) are skipped."
  ^double [col]
  (let [^F64Column c col
        ^doubles data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if has-n
      (loop [i (int 0), total (double 0.0)]
        (if (< i n)
          (let [v (aget data (+ off i))]
            (recur (unchecked-inc i)
                   (if (Double/isNaN v) total (+ total v))))
          total))
      (loop [i (int 0), total (double 0.0)]
        (if (< i n)
          (recur (unchecked-inc i) (+ total (aget data (+ off i))))
          total)))))

(defn f64-min
  "Minimum value in an F64 column. Returns nil if all null or empty."
  [col]
  (let [^F64Column c col
        ^doubles data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (loop [i (int 0), best Double/MAX_VALUE, found false]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (if (Double/isNaN v)
                (recur (unchecked-inc i) best found)
                (recur (unchecked-inc i) (min best v) true)))
            (when found best)))
        (loop [i (int 0), best (aget data off)]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (recur (unchecked-inc i) (if (< v best) v best)))
            best))))))

(defn f64-max
  "Maximum value in an F64 column. Returns nil if all null or empty."
  [col]
  (let [^F64Column c col
        ^doubles data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (loop [i (int 0), best (- Double/MAX_VALUE), found false]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (if (Double/isNaN v)
                (recur (unchecked-inc i) best found)
                (recur (unchecked-inc i) (max best v) true)))
            (when found best)))
        (loop [i (int 0), best (aget data off)]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (recur (unchecked-inc i) (if (> v best) v best)))
            best))))))

(defn f64-avg
  "Average of an F64 column. Returns double. Nulls (NaN) are skipped."
  [col]
  (let [^F64Column c col
        ^doubles data (.data c)
        off (.offset c)
        n (.len c)
        has-n (.has-nulls c)]
    (if (zero? n)
      nil
      (if has-n
        (loop [i (int 0), sum (double 0.0), cnt (int 0)]
          (if (< i n)
            (let [v (aget data (+ off i))]
              (if (Double/isNaN v)
                (recur (unchecked-inc i) sum cnt)
                (recur (unchecked-inc i) (+ sum v) (unchecked-inc cnt))))
            (if (pos? cnt) (/ sum (double cnt)) nil)))
        (loop [i (int 0), sum (double 0.0)]
          (if (< i n)
            (recur (unchecked-inc i) (+ sum (aget data (+ off i))))
            (/ sum (double n))))))))

;; ════════════════════════════════════════════════════════════════════════
;; Count
;; ════════════════════════════════════════════════════════════════════════

(defn count-non-null
  "Count of non-null rows. Returns long."
  ^long [col]
  (let [tag (col/-type-tag col)]
    (case tag
      :i64  (let [^I64Column c col
                  n (.len c)
                  has-n (.has-nulls c)]
              (if has-n
                (let [^longs data (.data c)
                      off (.offset c)
                      null-sent col/NULL_I64]
                  (loop [i (int 0), cnt (int 0)]
                    (if (< i n)
                      (recur (unchecked-inc i)
                             (if (= (aget data (+ off i)) null-sent) cnt (unchecked-inc cnt)))
                      cnt)))
                n))
      :f64  (let [^F64Column c col
                  n (.len c)
                  has-n (.has-nulls c)]
              (if has-n
                (let [^doubles data (.data c)
                      off (.offset c)]
                  (loop [i (int 0), cnt (int 0)]
                    (if (< i n)
                      (recur (unchecked-inc i)
                             (if (Double/isNaN (aget data (+ off i))) cnt (unchecked-inc cnt)))
                      cnt)))
                n))
      (col/-len col))))
