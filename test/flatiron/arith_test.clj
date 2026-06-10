(ns flatiron.arith-test
  "Specification tests for element-wise arithmetic operations."
  (:require [flatiron.arith :as arith]
            [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 unary
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-neg-basic
  (let [c  (col/i64-column [1 -2 3 -4 5])
        r  (arith/i64-neg c)]
    (t/is (= 5 (col/-len r)))
    (t/is (= :i64 (col/-type-tag r)))
    (t/is (= -1 (col/-get-long r 0)))
    (t/is (= 2  (col/-get-long r 1)))
    (t/is (= -3 (col/-get-long r 2)))
    (t/is (= 4  (col/-get-long r 3)))
    (t/is (= -5 (col/-get-long r 4)))))

(t/deftest i64-neg-with-nulls
  (let [c (col/i64-column [1 nil 3])
        r (arith/i64-neg c)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= -1 (col/-get-long r 0)))
    (t/is (= col/NULL_I64 (col/-get-long r 1)))
    (t/is (= -3 (col/-get-long r 2)))))

(t/deftest i64-abs-basic
  (let [c (col/i64-column [-10 20 -30 0])
        r (arith/i64-abs c)]
    (t/is (= 10 (col/-get-long r 0)))
    (t/is (= 20 (col/-get-long r 1)))
    (t/is (= 30 (col/-get-long r 2)))
    (t/is (= 0  (col/-get-long r 3)))))

(t/deftest i64-abs-with-nulls
  (let [c (col/i64-column [-10 nil 30])
        r (arith/i64-abs c)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= 10 (col/-get-long r 0)))
    (t/is (= col/NULL_I64 (col/-get-long r 1)))
    (t/is (= 30 (col/-get-long r 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; I64 binary
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-add-basic
  (let [a (col/i64-column [1 2 3])
        b (col/i64-column [10 20 30])
        r (arith/i64-add a b)]
    (t/is (= 11 (col/-get-long r 0)))
    (t/is (= 22 (col/-get-long r 1)))
    (t/is (= 33 (col/-get-long r 2)))))

(t/deftest i64-sub-basic
  (let [a (col/i64-column [10 20 30])
        b (col/i64-column [1 2 3])
        r (arith/i64-sub a b)]
    (t/is (= 9  (col/-get-long r 0)))
    (t/is (= 18 (col/-get-long r 1)))
    (t/is (= 27 (col/-get-long r 2)))))

(t/deftest i64-mul-basic
  (let [a (col/i64-column [2 3 4])
        b (col/i64-column [10 20 30])
        r (arith/i64-mul a b)]
    (t/is (= 20  (col/-get-long r 0)))
    (t/is (= 60  (col/-get-long r 1)))
    (t/is (= 120 (col/-get-long r 2)))))

(t/deftest i64-add-with-nulls
  (let [a (col/i64-column [1 nil 3])
        b (col/i64-column [10 20 30])
        r (arith/i64-add a b)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= 11 (col/-get-long r 0)))
    (t/is (= col/NULL_I64 (col/-get-long r 1)))
    (t/is (= 33 (col/-get-long r 2)))))

(t/deftest i64-sub-nulls-both
  (let [a (col/i64-column [1 2 nil])
        b (col/i64-column [10 nil 30])
        r (arith/i64-sub a b)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= -9 (col/-get-long r 0)))
    (t/is (= col/NULL_I64 (col/-get-long r 1)))
    (t/is (= col/NULL_I64 (col/-get-long r 2)))))

(t/deftest i64-div-basic
  (let [a (col/i64-column [10 20 30])
        b (col/i64-column [2 4 3])
        r (arith/i64-div a b)]
    (t/is (= :f64 (col/-type-tag r)))  ;; division produces F64
    (t/is (= 5.0 (col/-get-double r 0)))
    (t/is (= 5.0 (col/-get-double r 1)))
    (t/is (= 10.0 (col/-get-double r 2)))))

(t/deftest i64-div-by-zero
  (let [a (col/i64-column [10 20])
        b (col/i64-column [0 4])
        r (arith/i64-div a b)]
    (t/is (Double/isNaN (col/-get-double r 0)))
    (t/is (= 5.0 (col/-get-double r 1)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 unary
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-neg-basic
  (let [c (col/f64-column [1.5 -2.5 3.0])
        r (arith/f64-neg c)]
    (t/is (= -1.5 (col/-get-double r 0)))
    (t/is (= 2.5  (col/-get-double r 1)))
    (t/is (= -3.0 (col/-get-double r 2)))))

(t/deftest f64-neg-with-nulls
  (let [c (col/f64-column [1.0 nil 3.0])
        r (arith/f64-neg c)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= -1.0 (col/-get-double r 0)))
    (t/is (Double/isNaN (col/-get-double r 1)))
    (t/is (= -3.0 (col/-get-double r 2)))))

(t/deftest f64-abs-basic
  (let [c (col/f64-column [-1.5 2.5 -3.0])
        r (arith/f64-abs c)]
    (t/is (= 1.5 (col/-get-double r 0)))
    (t/is (= 2.5 (col/-get-double r 1)))
    (t/is (= 3.0 (col/-get-double r 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 binary
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-add-basic
  (let [a (col/f64-column [1.0 2.0 3.0])
        b (col/f64-column [0.5 1.5 2.5])
        r (arith/f64-add a b)]
    (t/is (= 1.5 (col/-get-double r 0)))
    (t/is (= 3.5 (col/-get-double r 1)))
    (t/is (= 5.5 (col/-get-double r 2)))))

(t/deftest f64-sub-basic
  (let [a (col/f64-column [5.0 4.0 3.0])
        b (col/f64-column [1.0 2.0 3.0])
        r (arith/f64-sub a b)]
    (t/is (= 4.0 (col/-get-double r 0)))
    (t/is (= 2.0 (col/-get-double r 1)))
    (t/is (= 0.0 (col/-get-double r 2)))))

(t/deftest f64-mul-basic
  (let [a (col/f64-column [2.0 3.0])
        b (col/f64-column [4.0 5.0])
        r (arith/f64-mul a b)]
    (t/is (= 8.0  (col/-get-double r 0)))
    (t/is (= 15.0 (col/-get-double r 1)))))

(t/deftest f64-div-basic
  (let [a (col/f64-column [10.0 20.0 30.0])
        b (col/f64-column [2.0  4.0  8.0])
        r (arith/f64-div a b)]
    (t/is (= 5.0  (col/-get-double r 0)))
    (t/is (= 5.0  (col/-get-double r 1)))
    (t/is (= 3.75 (col/-get-double r 2)))))

(t/deftest f64-div-by-zero
  (let [a (col/f64-column [10.0 20.0])
        b (col/f64-column [0.0  4.0])
        r (arith/f64-div a b)]
    (t/is (Double/isNaN (col/-get-double r 0)))
    (t/is (= 5.0 (col/-get-double r 1)))))

(t/deftest f64-add-with-nulls
  (let [a (col/f64-column [1.0 nil 3.0])
        b (col/f64-column [0.5 1.5 2.5])
        r (arith/f64-add a b)]
    (t/is (true? (col/-has-nulls? r)))
    (t/is (= 1.5 (col/-get-double r 0)))
    (t/is (Double/isNaN (col/-get-double r 1)))
    (t/is (= 5.5 (col/-get-double r 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; Large column (multi-morsel)
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-add-large
  "Multi-morsel: 3K rows should exercise multiple morsel chunks."
  (let [n  (* 3 1024)
        a  (col/i64-column (mapv long (range n)))
        b  (col/i64-column (mapv long (range n)))
        r  (arith/i64-add a b)]
    (t/is (= n (col/-len r)))
    (t/is (= 0   (col/-get-long r 0)))
    (t/is (= 2   (col/-get-long r 1)))
    (t/is (= (* 2 (dec n)) (col/-get-long r (dec n))))))

;; ════════════════════════════════════════════════════════════════════════
;; Sliced column arithmetic
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-add-sliced
  "Arithmetic on sliced columns respects the offset."
  (let [base (col/i64-column [0 1 2 3 4 5 6 7 8 9])
        a    (col/-slice base 2 3)  ;; [2 3 4]
        b    (col/-slice base 5 3)  ;; [5 6 7]
        r    (arith/i64-add a b)]
    (t/is (= 3 (col/-len r)))
    (t/is (= 7  (col/-get-long r 0)))   ;; 2+5
    (t/is (= 9  (col/-get-long r 1)))   ;; 3+6
    (t/is (= 11 (col/-get-long r 2))))) ;; 4+7
