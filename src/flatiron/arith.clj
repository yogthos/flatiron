(ns flatiron.arith
  "Element-wise arithmetic operations on columns.
   All operations use morsel iteration — type dispatch happens once
   per morsel, then inner loops run on raw primitive arrays."
  (:require [flatiron.column :as col]
            [flatiron.morsel :as m])
  (:import [flatiron.column I64Column F64Column]))

(set! *warn-on-reflection* true)

;; ════════════════════════════════════════════════════════════════════════
;; I64 unary ops
;; ════════════════════════════════════════════════════════════════════════

(defn i64-neg
  "Negate every element of an I64 column."
  [col]
  (let [^I64Column icol col
        n      (.len icol)
        out    (long-array n)
        has-n  (.has-nulls icol)
        ms     (m/i64-morsel-source col)
        buf    (long-array m/MORSEL-SIZE)
        null-sent col/NULL_I64]
    (loop [dst-off 0]
      (let [cnt (long (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE))]
        (if (zero? cnt)
          (I64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i cnt]
                (let [v (aget buf i)]
                  (aset out (+ dst-off i)
                        (if (= v null-sent) null-sent (- v)))))
              (dotimes [i cnt]
                (aset out (+ dst-off i) (- (aget buf i)))))
            (recur (+ dst-off cnt))))))))

(defn i64-abs
  "Absolute value of every element in an I64 column."
  [col]
  (let [^I64Column icol col
        n      (.len icol)
        out    (long-array n)
        has-n  (.has-nulls icol)
        ms     (m/i64-morsel-source col)
        buf    (long-array m/MORSEL-SIZE)
        null-sent col/NULL_I64]
    (loop [dst-off 0]
      (let [cnt (long (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE))]
        (if (zero? cnt)
          (I64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i cnt]
                (let [v (aget buf i)]
                  (aset out (+ dst-off i)
                        (if (= v null-sent) null-sent (Math/abs v)))))
              (dotimes [i cnt]
                (aset out (+ dst-off i) (Math/abs (aget buf i)))))
            (recur (+ dst-off cnt))))))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 unary ops
;; ════════════════════════════════════════════════════════════════════════

(defn f64-neg
  "Negate every element of an F64 column."
  [col]
  (let [^F64Column fcol col
        n      (.len fcol)
        out    (double-array n)
        has-n  (.has-nulls fcol)
        ms     (m/f64-morsel-source col)
        buf    (double-array m/MORSEL-SIZE)]
    (loop [dst-off 0]
      (let [cnt (long (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE))]
        (if (zero? cnt)
          (F64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i cnt]
                (let [v (aget buf i)]
                  (aset out (+ dst-off i)
                        (if (Double/isNaN v) v (- v)))))
              (dotimes [i cnt]
                (aset out (+ dst-off i) (- (aget buf i)))))
            (recur (+ dst-off cnt))))))))

(defn f64-abs
  "Absolute value of every element in an F64 column."
  [col]
  (let [^F64Column fcol col
        n      (.len fcol)
        out    (double-array n)
        has-n  (.has-nulls fcol)
        ms     (m/f64-morsel-source col)
        buf    (double-array m/MORSEL-SIZE)]
    (loop [dst-off 0]
      (let [cnt (long (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE))]
        (if (zero? cnt)
          (F64Column. out n 0 has-n nil)
          (do
            (dotimes [i cnt]
              (aset out (+ dst-off i) (Math/abs (aget buf i))))
            (recur (+ dst-off cnt))))))))

;; ════════════════════════════════════════════════════════════════════════
;; I64 binary ops
;; ════════════════════════════════════════════════════════════════════════

(defn- i64-binary-op
  "Helper: apply a binary long op to two I64 columns, producing an I64 result."
  [op a-col b-col]
  (let [^flatiron.column.I64Column acol a-col
        ^flatiron.column.I64Column bcol b-col
        n      (.len acol)
        out    (long-array n)
        has-n  (or (.has-nulls acol) (.has-nulls bcol))
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
          (I64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i a-cnt]
                (let [a (aget abuf i)
                      b (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (or (= a null-sent) (= b null-sent))
                          null-sent
                          (long (op a b))))))
              (dotimes [i a-cnt]
                (aset out (+ dst-off i) (long (op (aget abuf i) (aget bbuf i))))))
            (recur (+ dst-off a-cnt))))))))

(defn i64-add [a b] (i64-binary-op + a b))
(defn i64-sub [a b] (i64-binary-op - a b))
(defn i64-mul [a b] (i64-binary-op * a b))

(defn i64-div
  "Integer division. Produces F64 result. Null if divisor is 0 or either input is null."
  [a b]
  (let [^flatiron.column.I64Column acol a
        ^flatiron.column.I64Column bcol b
        n      (.len acol)
        out    (double-array n)
        has-n  (or (.has-nulls acol) (.has-nulls bcol))
        ams    (m/i64-morsel-source a)
        bms    (m/i64-morsel-source b)
        abuf   (long-array m/MORSEL-SIZE)
        bbuf   (long-array m/MORSEL-SIZE)
        null-sent col/NULL_I64]
    (loop [dst-off 0]
      (let [a-cnt (long (m/morsel-next-i64! ams abuf 0 m/MORSEL-SIZE))
            b-cnt (long (m/morsel-next-i64! bms bbuf 0 m/MORSEL-SIZE))]
        (assert (= a-cnt b-cnt) "Column length mismatch")
        (if (zero? a-cnt)
          (F64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i a-cnt]
                (let [av (aget abuf i)
                      bv (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (or (= av null-sent) (= bv null-sent) (zero? bv))
                          Double/NaN
                          (/ (double av) (double bv))))))
              (dotimes [i a-cnt]
                (let [bv (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (zero? bv)
                          Double/NaN
                          (/ (double (aget abuf i)) (double bv)))))))
            (recur (+ dst-off a-cnt))))))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 binary ops
;; ════════════════════════════════════════════════════════════════════════

(defn- f64-binary-op
  "Helper: apply a binary double op to two F64 columns, producing an F64 result."
  [op a-col b-col]
  (let [^flatiron.column.F64Column acol a-col
        ^flatiron.column.F64Column bcol b-col
        n      (.len acol)
        out    (double-array n)
        has-n  (or (.has-nulls acol) (.has-nulls bcol))
        ams    (m/f64-morsel-source a-col)
        bms    (m/f64-morsel-source b-col)
        abuf   (double-array m/MORSEL-SIZE)
        bbuf   (double-array m/MORSEL-SIZE)]
    (loop [dst-off 0]
      (let [a-cnt (long (m/morsel-next-f64! ams abuf 0 m/MORSEL-SIZE))
            b-cnt (long (m/morsel-next-f64! bms bbuf 0 m/MORSEL-SIZE))]
        (assert (= a-cnt b-cnt) "Column length mismatch")
        (if (zero? a-cnt)
          (F64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i a-cnt]
                (let [av (aget abuf i)
                      bv (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (or (Double/isNaN av) (Double/isNaN bv))
                          Double/NaN
                          (double (op av bv))))))
              (dotimes [i a-cnt]
                (aset out (+ dst-off i) (double (op (aget abuf i) (aget bbuf i))))))
            (recur (+ dst-off a-cnt))))))))

(defn f64-add [a b] (f64-binary-op + a b))
(defn f64-sub [a b] (f64-binary-op - a b))
(defn f64-mul [a b] (f64-binary-op * a b))

(defn f64-div
  "Element-wise division of two F64 columns. Null if divisor is zero or either input is null."
  [a b]
  (let [^flatiron.column.F64Column acol a
        ^flatiron.column.F64Column bcol b
        n      (.len acol)
        out    (double-array n)
        has-n  (or (.has-nulls acol) (.has-nulls bcol))
        ams    (m/f64-morsel-source a)
        bms    (m/f64-morsel-source b)
        abuf   (double-array m/MORSEL-SIZE)
        bbuf   (double-array m/MORSEL-SIZE)]
    (loop [dst-off 0]
      (let [a-cnt (long (m/morsel-next-f64! ams abuf 0 m/MORSEL-SIZE))
            b-cnt (long (m/morsel-next-f64! bms bbuf 0 m/MORSEL-SIZE))]
        (assert (= a-cnt b-cnt) "Column length mismatch")
        (if (zero? a-cnt)
          (F64Column. out n 0 has-n nil)
          (do
            (if has-n
              (dotimes [i a-cnt]
                (let [av (aget abuf i)
                      bv (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (or (Double/isNaN av) (Double/isNaN bv) (zero? bv))
                          Double/NaN
                          (/ av bv)))))
              (dotimes [i a-cnt]
                (let [bv (aget bbuf i)]
                  (aset out (+ dst-off i)
                        (if (zero? bv)
                          Double/NaN
                          (/ (aget abuf i) bv))))))
            (recur (+ dst-off a-cnt))))))))
