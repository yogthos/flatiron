(ns flatiron.window-test
  "Specification tests for window functions."
  (:require [flatiron.window :as w]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.sort :as sort]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; row-number
;; ════════════════════════════════════════════════════════════════════════

(t/deftest row-number-basic
  (let [sym-c (col/sym-column [:a :b :c])
        table (tbl/table [:sym] [sym-c])
        rn    (w/row-number table)]
    (t/is (= 3 (col/-len rn)))
    (t/is (= 1 (col/-get-long rn 0)))
    (t/is (= 2 (col/-get-long rn 1)))
    (t/is (= 3 (col/-get-long rn 2)))))

(t/deftest row-number-empty
  (let [table (tbl/table [] [])
        rn    (w/row-number table)]
    (t/is (= 0 (col/-len rn)))))

;; ════════════════════════════════════════════════════════════════════════
;; rank
;; ════════════════════════════════════════════════════════════════════════

(t/deftest rank-basic
  (let [rank-col (col/i64-column [10 10 20 20 30])
        order-col (col/i64-column [1 2 1 2 1])
        r (w/rank rank-col order-col)]
    (t/is (= 5 (col/-len r)))
    ;; Rows 0,1 (tied): rank=1; rows 2,3 (tied): rank=3; row 4: rank=5
    (t/is (= 1 (col/-get-long r 0)))
    (t/is (= 1 (col/-get-long r 1)))
    (t/is (= 3 (col/-get-long r 2)))
    (t/is (= 3 (col/-get-long r 3)))
    (t/is (= 5 (col/-get-long r 4)))))

(t/deftest rank-no-ties
  (let [rank-col (col/i64-column [10 20 30])
        order-col (col/i64-column [1 1 1])
        r (w/rank rank-col order-col)]
    (t/is (= 1 (col/-get-long r 0)))
    (t/is (= 2 (col/-get-long r 1)))
    (t/is (= 3 (col/-get-long r 2)))))

(t/deftest rank-sym
  (let [rank-col (col/sym-column [:a :a :b])
        order-col (col/i64-column [1 2 1])
        r (w/rank rank-col order-col)]
    (t/is (= 1 (col/-get-long r 0)))
    (t/is (= 1 (col/-get-long r 1)))
    (t/is (= 3 (col/-get-long r 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; dense-rank
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dense-rank-basic
  (let [c (col/i64-column [10 10 20 20 30])
        r (w/dense-rank c)]
    (t/is (= 1 (col/-get-long r 0)))
    (t/is (= 1 (col/-get-long r 1)))
    (t/is (= 2 (col/-get-long r 2)))
    (t/is (= 2 (col/-get-long r 3)))
    (t/is (= 3 (col/-get-long r 4)))))

(t/deftest dense-rank-no-ties
  (let [c (col/i64-column [10 20 30])
        r (w/dense-rank c)]
    (t/is (= 1 (col/-get-long r 0)))
    (t/is (= 2 (col/-get-long r 1)))
    (t/is (= 3 (col/-get-long r 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; lag
;; ════════════════════════════════════════════════════════════════════════

(t/deftest lag-i64-default-offset
  (let [c (col/i64-column [10 20 30 40])
        l (w/lag c 1 nil)]
    (t/is (= 4 (col/-len l)))
    ;; First row gets null sentinel (offset=1, no previous row)
    (t/is (= col/NULL_I64 (col/-get-long l 0)))
    (t/is (= 10 (col/-get-long l 1)))
    (t/is (= 20 (col/-get-long l 2)))
    (t/is (= 30 (col/-get-long l 3)))))

(t/deftest lag-i64-custom-default
  (let [c (col/i64-column [10 20 30])
        l (w/lag c 1 -1)]
    (t/is (= -1 (col/-get-long l 0)))    ;; default
    (t/is (= 10 (col/-get-long l 1)))
    (t/is (= 20 (col/-get-long l 2)))))

(t/deftest lag-i64-offset-2
  (let [c (col/i64-column [10 20 30 40])
        l (w/lag c 2 0)]
    (t/is (= 0  (col/-get-long l 0)))
    (t/is (= 0  (col/-get-long l 1)))
    (t/is (= 10 (col/-get-long l 2)))
    (t/is (= 20 (col/-get-long l 3)))))

(t/deftest lag-f64
  (let [c (col/f64-column [1.5 2.5 3.5])
        l (w/lag c 1 nil)]
    (t/is (Double/isNaN (col/-get-double l 0)))
    (t/is (= 1.5 (col/-get-double l 1)))
    (t/is (= 2.5 (col/-get-double l 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; lead
;; ════════════════════════════════════════════════════════════════════════

(t/deftest lead-i64-default-offset
  (let [c (col/i64-column [10 20 30 40])
        l (w/lead c 1 nil)]
    (t/is (= 20 (col/-get-long l 0)))
    (t/is (= 30 (col/-get-long l 1)))
    (t/is (= 40 (col/-get-long l 2)))
    ;; Last row gets null sentinel
    (t/is (= col/NULL_I64 (col/-get-long l 3)))))

(t/deftest lead-i64-custom-default
  (let [c (col/i64-column [10 20 30])
        l (w/lead c 1 -1)]
    (t/is (= 20 (col/-get-long l 0)))
    (t/is (= 30 (col/-get-long l 1)))
    (t/is (= -1 (col/-get-long l 2)))))

(t/deftest lead-i64-offset-2
  (let [c (col/i64-column [10 20 30 40])
        l (w/lead c 2 0)]
    (t/is (= 30 (col/-get-long l 0)))
    (t/is (= 40 (col/-get-long l 1)))
    (t/is (= 0  (col/-get-long l 2)))
    (t/is (= 0  (col/-get-long l 3)))))
