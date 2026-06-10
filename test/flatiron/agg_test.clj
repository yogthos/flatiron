(ns flatiron.agg-test
  "Specification tests for aggregation functions."
  (:require [flatiron.agg :as agg]
            [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 sum
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-sum-basic
  (let [c (col/i64-column [1 2 3 4 5])]
    (t/is (= 15 (agg/i64-sum c)))))

(t/deftest i64-sum-with-nulls
  (let [c (col/i64-column [1 nil 3 nil 5])]
    (t/is (= 9 (agg/i64-sum c)))))

(t/deftest i64-sum-all-null
  (let [c (col/i64-column [nil nil nil])]
    (t/is (= 0 (agg/i64-sum c)))))

(t/deftest i64-sum-empty
  (let [c (col/i64-column [])]
    (t/is (= 0 (agg/i64-sum c)))))

(t/deftest i64-sum-large
  (let [n  (* 3 1024)
        c  (col/i64-column (mapv long (range n)))]
    (t/is (= (long (/ (* n (dec n)) 2)) (agg/i64-sum c)))))

;; ════════════════════════════════════════════════════════════════════════
;; I64 min / max
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-min-basic
  (let [c (col/i64-column [3 1 4 1 5 9 2])]
    (t/is (= 1 (agg/i64-min c)))))

(t/deftest i64-max-basic
  (let [c (col/i64-column [3 1 4 1 5 9 2])]
    (t/is (= 9 (agg/i64-max c)))))

(t/deftest i64-min-with-nulls
  (let [c (col/i64-column [3 nil 1 nil 5])]
    (t/is (= 1 (agg/i64-min c)))))

(t/deftest i64-max-with-nulls
  (let [c (col/i64-column [3 nil 1 nil 5])]
    (t/is (= 5 (agg/i64-max c)))))

(t/deftest i64-min-all-null
  (let [c (col/i64-column [nil nil])]
    (t/is (nil? (agg/i64-min c)))))

(t/deftest i64-max-all-null
  (let [c (col/i64-column [nil nil])]
    (t/is (nil? (agg/i64-max c)))))

(t/deftest i64-min-negative
  (let [c (col/i64-column [-5 -2 -10 3])]
    (t/is (= -10 (agg/i64-min c)))))

;; ════════════════════════════════════════════════════════════════════════
;; I64 avg
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-avg-basic
  (let [c (col/i64-column [2 4 6])]
    (t/is (= 4.0 (agg/i64-avg c)))))

(t/deftest i64-avg-with-nulls
  (let [c (col/i64-column [2 nil 6 nil 10])]
    (t/is (= 6.0 (agg/i64-avg c)))))

(t/deftest i64-avg-all-null
  (let [c (col/i64-column [nil nil])]
    (t/is (nil? (agg/i64-avg c)))))

(t/deftest i64-avg-empty
  (let [c (col/i64-column [])]
    (t/is (nil? (agg/i64-avg c)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 sum
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-sum-basic
  (let [c (col/f64-column [1.5 2.5 3.0])]
    (t/is (= 7.0 (agg/f64-sum c)))))

(t/deftest f64-sum-with-nulls
  (let [c (col/f64-column [1.0 nil 3.0])]
    (t/is (= 4.0 (agg/f64-sum c)))))

(t/deftest f64-sum-all-nan
  (let [c (col/f64-column [nil nil])]
    (t/is (= 0.0 (agg/f64-sum c)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 min / max
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-min-basic
  (let [c (col/f64-column [3.0 1.0 4.5 0.5])]
    (t/is (= 0.5 (agg/f64-min c)))))

(t/deftest f64-max-basic
  (let [c (col/f64-column [3.0 1.0 4.5 0.5])]
    (t/is (= 4.5 (agg/f64-max c)))))

(t/deftest f64-min-with-nulls
  (let [c (col/f64-column [3.0 nil 1.0])]
    (t/is (= 1.0 (agg/f64-min c)))))

(t/deftest f64-max-with-nulls
  (let [c (col/f64-column [3.0 nil 1.0])]
    (t/is (= 3.0 (agg/f64-max c)))))

(t/deftest f64-min-all-null
  (let [c (col/f64-column [nil nil])]
    (t/is (nil? (agg/f64-min c)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 avg
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-avg-basic
  (let [c (col/f64-column [2.0 4.0 6.0])]
    (t/is (= 4.0 (agg/f64-avg c)))))

(t/deftest f64-avg-with-nulls
  (let [c (col/f64-column [2.0 nil 6.0])]
    (t/is (= 4.0 (agg/f64-avg c)))))

(t/deftest f64-avg-all-null
  (let [c (col/f64-column [nil nil])]
    (t/is (nil? (agg/f64-avg c)))))

;; ════════════════════════════════════════════════════════════════════════
;; Count
;; ════════════════════════════════════════════════════════════════════════

(t/deftest count-non-null-i64
  (let [c (col/i64-column [1 nil 3 nil 5])]
    (t/is (= 3 (agg/count-non-null c)))))

(t/deftest count-non-null-f64
  (let [c (col/f64-column [1.0 nil 3.0 nil 5.0])]
    (t/is (= 3 (agg/count-non-null c)))))

(t/deftest count-non-nullable
  (let [c (col/bool-column [true false true])]
    (t/is (= 3 (agg/count-non-null c)))))
