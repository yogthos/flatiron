(ns flatiron.cmp-test
  "Specification tests for element-wise comparison operations."
  (:require [flatiron.cmp :as cmp]
            [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 comparisons
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-eq-basic
  (let [a (col/i64-column [1 2 3 4 5])
        b (col/i64-column [1 0 3 0 5])
        r (cmp/i64-eq a b)]
    (t/is (= :bool (col/-type-tag r)))
    (t/is (true?  (col/-get-obj r 0)))   ;; 1==1
    (t/is (false? (col/-get-obj r 1)))   ;; 2!=0
    (t/is (true?  (col/-get-obj r 2)))   ;; 3==3
    (t/is (false? (col/-get-obj r 3)))   ;; 4!=0
    (t/is (true?  (col/-get-obj r 4))))) ;; 5==5

(t/deftest i64-lt-basic
  (let [a (col/i64-column [1 2 3 4 5])
        b (col/i64-column [5 4 3 2 1])
        r (cmp/i64-lt a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 1<5
    (t/is (true?  (col/-get-obj r 1)))   ;; 2<4
    (t/is (false? (col/-get-obj r 2)))   ;; 3<3 = false
    (t/is (false? (col/-get-obj r 3)))   ;; 4<2
    (t/is (false? (col/-get-obj r 4))))) ;; 5<1

(t/deftest i64-gt-basic
  (let [a (col/i64-column [5 4 3 2 1])
        b (col/i64-column [1 2 3 4 5])
        r (cmp/i64-gt a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 5>1
    (t/is (true?  (col/-get-obj r 1)))   ;; 4>2
    (t/is (false? (col/-get-obj r 2)))   ;; 3>3
    (t/is (false? (col/-get-obj r 3)))   ;; 2>4
    (t/is (false? (col/-get-obj r 4))))) ;; 1>5

(t/deftest i64-le-basic
  (let [a (col/i64-column [1 2 3 4 5])
        b (col/i64-column [1 2 3 0 0])
        r (cmp/i64-le a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 1<=1
    (t/is (true?  (col/-get-obj r 1)))   ;; 2<=2
    (t/is (true?  (col/-get-obj r 2)))   ;; 3<=3
    (t/is (false? (col/-get-obj r 3)))   ;; 4<=0
    (t/is (false? (col/-get-obj r 4))))) ;; 5<=0

(t/deftest i64-cmp-with-nulls
  "Null sentinel in either input → result is false (0)."
  (let [a (col/i64-column [1 nil 3])
        b (col/i64-column [1 2 nil])
        r (cmp/i64-eq a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 1==1
    (t/is (false? (col/-get-obj r 1)))   ;; nil != 2
    (t/is (false? (col/-get-obj r 2))))) ;; 3 != nil

(t/deftest i64-cmp-large
  "Multi-morsel: 3K rows."
  (let [n  (* 3 1024)
        a  (col/i64-column (mapv long (range n)))
        b  (col/i64-column (mapv #(* 2 %) (range n)))
        r  (cmp/i64-lt a b)]     ;; a < b should be true for all positive rows
    (t/is (= n (col/-len r)))
    (t/is (true?  (col/-get-obj r 1)))
    (t/is (false? (col/-get-obj r 0))))) ;; 0<0 is false

;; ════════════════════════════════════════════════════════════════════════
;; F64 comparisons
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-eq-basic
  (let [a (col/f64-column [1.0 2.0 3.0])
        b (col/f64-column [1.0 0.0 3.0])
        r (cmp/f64-eq a b)]
    (t/is (true?  (col/-get-obj r 0)))
    (t/is (false? (col/-get-obj r 1)))
    (t/is (true?  (col/-get-obj r 2)))))

(t/deftest f64-lt-basic
  (let [a (col/f64-column [1.0 2.0 3.0])
        b (col/f64-column [3.0 2.0 1.0])
        r (cmp/f64-lt a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 1<3
    (t/is (false? (col/-get-obj r 1)))   ;; 2<2
    (t/is (false? (col/-get-obj r 2))))) ;; 3<1

(t/deftest f64-cmp-nan
  "NaN in either input → result is false."
  (let [a (col/f64-column [1.0 Double/NaN 3.0])
        b (col/f64-column [1.0 2.0      Double/NaN])
        r (cmp/f64-eq a b)]
    (t/is (true?  (col/-get-obj r 0)))   ;; 1.0==1.0
    (t/is (false? (col/-get-obj r 1)))   ;; NaN!=2.0
    (t/is (false? (col/-get-obj r 2))))) ;; 3.0!=NaN

(t/deftest f64-cmp-with-nulls
  (let [a (col/f64-column [1.0 nil 3.0])
        b (col/f64-column [1.0 2.0 nil])
        r (cmp/f64-gt a b)]
    (t/is (false? (col/-get-obj r 0)))   ;; 1>1 = false
    (t/is (false? (col/-get-obj r 1)))   ;; null > 2 = false
    (t/is (false? (col/-get-obj r 2))))) ;; 3 > null = false

;; ════════════════════════════════════════════════════════════════════════
;; Sliced column comparisons
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-eq-sliced
  "Comparison on sliced columns."
  (let [base (col/i64-column [0 1 2 3 4 5 6 7])
        a    (col/-slice base 1 3)  ;; [1 2 3]
        b    (col/-slice base 4 3)  ;; [4 5 6]
        r    (cmp/i64-lt a b)]
    (t/is (true? (col/-get-obj r 0)))   ;; 1<4
    (t/is (true? (col/-get-obj r 1)))   ;; 2<5
    (t/is (true? (col/-get-obj r 2))))) ;; 3<6
