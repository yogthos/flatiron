(ns flatiron.cmp
  "Element-wise comparison operations on columns.
   Produces BoolColumn (backed by byte array, 1=true 0=false).
   All operations use morsel iteration — type dispatch happens once
   per morsel, then inner loops run on raw primitive arrays."
  (:require [flatiron.column :as col]
            [flatiron.morsel :as m])
  (:import [flatiron.column BoolColumn]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 comparisons
;; ════════════════════════════════════════════════════════════════════════

(defn- i64-cmp
  "Binary comparison of two I64 columns. Returns BoolColumn."
  [op a-col b-col]
  (let [^flatiron.column.I64Column acol a-col
        ^flatiron.column.I64Column bcol b-col
        n      (.len acol)
        out    (byte-array n)
        ams    (m/i64-morsel-source a-col)
        bms    (m/i64-morsel-source b-col)
        abuf   (long-array m/MORSEL-SIZE)
        bbuf   (long-array m/MORSEL-SIZE)
        null-sent col/NULL_I64]
    (loop [dst-off 0]
      (let [a-cnt (long (m/morsel-next-i64! ams abuf 0 m/MORSEL-SIZE))
            b-cnt (long (m/morsel-next-i64! bms bbuf 0 m/MORSEL-SIZE))]
        (assert (= a-cnt b-cnt) "Column length mismatch")
        (if (zero? a-cnt)
          (BoolColumn. out n 0)
          (do
            (dotimes [i a-cnt]
              (let [a (aget abuf i)
                    b (aget bbuf i)]
                (aset out (+ dst-off i)
                      (byte (if (or (= a null-sent) (= b null-sent))
                              0
                              (if (op a b) 1 0))))))
            (recur (+ dst-off a-cnt))))))))

(defn i64-eq [a b] (i64-cmp == a b))
(defn i64-ne [a b] (i64-cmp (fn [x y] (not= x y)) a b))
(defn i64-lt [a b] (i64-cmp <  a b))
(defn i64-le [a b] (i64-cmp <= a b))
(defn i64-gt [a b] (i64-cmp >  a b))
(defn i64-ge [a b] (i64-cmp >= a b))

;; ════════════════════════════════════════════════════════════════════════
;; F64 comparisons
;; ════════════════════════════════════════════════════════════════════════

(defn- f64-cmp
  "Binary comparison of two F64 columns. Returns BoolColumn.
   NaN compares as false (matching C rayforce NaN != NaN convention)."
  [op a-col b-col]
  (let [^flatiron.column.F64Column acol a-col
        ^flatiron.column.F64Column bcol b-col
        n      (.len acol)
        out    (byte-array n)
        ams    (m/f64-morsel-source a-col)
        bms    (m/f64-morsel-source b-col)
        abuf   (double-array m/MORSEL-SIZE)
        bbuf   (double-array m/MORSEL-SIZE)]
    (loop [dst-off 0]
      (let [a-cnt (long (m/morsel-next-f64! ams abuf 0 m/MORSEL-SIZE))
            b-cnt (long (m/morsel-next-f64! bms bbuf 0 m/MORSEL-SIZE))]
        (assert (= a-cnt b-cnt) "Column length mismatch")
        (if (zero? a-cnt)
          (BoolColumn. out n 0)
          (do
            (dotimes [i a-cnt]
              (let [a (aget abuf i)
                    b (aget bbuf i)]
                (aset out (+ dst-off i)
                      (byte (if (or (Double/isNaN a) (Double/isNaN b))
                              0
                              (if (op a b) 1 0))))))
            (recur (+ dst-off a-cnt))))))))

(defn f64-eq [a b] (f64-cmp == a b))
(defn f64-ne [a b] (f64-cmp (fn [x y] (not= x y)) a b))
(defn f64-lt [a b] (f64-cmp <  a b))
(defn f64-le [a b] (f64-cmp <= a b))
(defn f64-gt [a b] (f64-cmp >  a b))
(defn f64-ge [a b] (f64-cmp >= a b))
