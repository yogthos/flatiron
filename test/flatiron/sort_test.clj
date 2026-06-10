(ns flatiron.sort-test
  "Specification tests for sort operations."
  (:require [flatiron.sort :as sort]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; sort-index — single column
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sort-index-i64-asc
  (let [c   (col/i64-column [30 10 20])
        idx (sort/sort-index c :asc)]
    (t/is (= 3 (alength idx)))
    ;; idx gives sorted positions: original index of smallest, middle, largest
    (t/is (= 1 (aget idx 0)))  ;; 10 was at position 1
    (t/is (= 2 (aget idx 1)))  ;; 20 was at position 2
    (t/is (= 0 (aget idx 2))))) ;; 30 was at position 0

(t/deftest sort-index-i64-desc
  (let [c   (col/i64-column [30 10 20])
        idx (sort/sort-index c :desc)]
    (t/is (= 0 (aget idx 0)))  ;; 30
    (t/is (= 2 (aget idx 1)))  ;; 20
    (t/is (= 1 (aget idx 2))))) ;; 10

(t/deftest sort-index-f64
  (let [c   (col/f64-column [3.5 1.0 2.5])
        idx (sort/sort-index c :asc)]
    (t/is (= 1 (aget idx 0)))  ;; 1.0 at position 1
    (t/is (= 2 (aget idx 1)))  ;; 2.5 at position 2
    (t/is (= 0 (aget idx 2))))) ;; 3.5 at position 0

(t/deftest sort-index-sym
  (let [c   (col/sym-column [:c :a :b])
        idx (sort/sort-index c :asc)]
    (t/is (= 1 (aget idx 0)))  ;; :a
    (t/is (= 2 (aget idx 1)))  ;; :b
    (t/is (= 0 (aget idx 2))))) ;; :c

(t/deftest sort-index-str
  (let [c   (col/str-column ["zz" "aa" "mm"])
        idx (sort/sort-index c :asc)]
    (t/is (= 1 (aget idx 0)))  ;; "aa"
    (t/is (= 2 (aget idx 1)))  ;; "mm"
    (t/is (= 0 (aget idx 2))))) ;; "zz"

(t/deftest sort-index-stability
  "Equal keys should preserve original order."
  (let [c   (col/i64-column [5 5 5])
        idx (sort/sort-index c :asc)]
    (t/is (= 0 (aget idx 0)))
    (t/is (= 1 (aget idx 1)))
    (t/is (= 2 (aget idx 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; sort-index-by — multi-key
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sort-index-by-two-keys
  (let [a (col/i64-column [1 2 1 2])
        b (col/i64-column [200 100 100 200])
        idx (sort/sort-index-by [[a :asc] [b :asc]])]
    ;; Primary sort by a: rows 0,2 (a=1) before 1,3 (a=2)
    ;; Within a=1: b=100 at row 2, b=200 at row 0 → idx[0]=2, idx[1]=0
    ;; Within a=2: b=100 at row 1, b=200 at row 3 → idx[2]=1, idx[3]=3
    (t/is (= 2 (aget idx 0)))
    (t/is (= 0 (aget idx 1)))
    (t/is (= 1 (aget idx 2)))
    (t/is (= 3 (aget idx 3)))))

(t/deftest sort-index-by-mixed-direction
  (let [a (col/i64-column [1 1 2 2])
        b (col/i64-column [10 20 10 20])]
    (let [idx (sort/sort-index-by [[a :asc] [b :desc]])]
      ;; a=1: b desc → 20 at row 1, 10 at row 0 → idx[0]=1, idx[1]=0
      ;; a=2: b desc → 20 at row 3, 10 at row 2 → idx[2]=3, idx[3]=2
      (t/is (= 1 (aget idx 0)))
      (t/is (= 0 (aget idx 1)))
      (t/is (= 3 (aget idx 2)))
      (t/is (= 2 (aget idx 3))))))

;; ════════════════════════════════════════════════════════════════════════
;; gather-column
;; ════════════════════════════════════════════════════════════════════════

(t/deftest gather-reorder-i64
  (let [c   (col/i64-column [30 10 20])
        idx (int-array [1 2 0])
        g   (sort/gather-column c idx)]
    (t/is (= 10 (col/-get-long g 0)))
    (t/is (= 20 (col/-get-long g 1)))
    (t/is (= 30 (col/-get-long g 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; sort-table — end-to-end
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sort-table-single-key
  (let [sym-c (col/sym-column [:b :c :a])
        qty-c (col/i64-column [200 300 100])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        result (sort/sort-table table [[:sym :asc]])]
    (t/is (= 3 (tbl/nrows result)))
    (t/is (= :a (col/-get-obj (tbl/col result :sym) 0)))
    (t/is (= :b (col/-get-obj (tbl/col result :sym) 1)))
    (t/is (= :c (col/-get-obj (tbl/col result :sym) 2)))
    ;; qty column should follow
    (t/is (= 100 (col/-get-long (tbl/col result :qty) 0)))
    (t/is (= 200 (col/-get-long (tbl/col result :qty) 1)))
    (t/is (= 300 (col/-get-long (tbl/col result :qty) 2)))))

(t/deftest sort-table-two-keys
  (let [date-c (col/i64-column [1 1 2 2])
        amt-c  (col/i64-column [200 100 300 100])
        table  (tbl/table [:date :amt] [date-c amt-c])
        result (sort/sort-table table [[:date :asc] [:amt :desc]])]
    ;; date=1: amt desc → [200 100] = original rows 0,1
    ;; date=2: amt desc → [300 100] = original rows 2,3
    (t/is (= 1   (col/-get-long (tbl/col result :date) 0)))
    (t/is (= 200 (col/-get-long (tbl/col result :amt) 0)))
    (t/is (= 1   (col/-get-long (tbl/col result :date) 1)))
    (t/is (= 100 (col/-get-long (tbl/col result :amt) 1)))
    (t/is (= 2   (col/-get-long (tbl/col result :date) 2)))
    (t/is (= 300 (col/-get-long (tbl/col result :amt) 2)))
    (t/is (= 2   (col/-get-long (tbl/col result :date) 3)))
    (t/is (= 100 (col/-get-long (tbl/col result :amt) 3)))))

;; ════════════════════════════════════════════════════════════════════════
;; Large sort — multi-morsel
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sort-large
  (let [n     (* 3 1024)
        c     (col/i64-column (mapv long (reverse (range n))))
        idx   (sort/sort-index c :asc)]
    (t/is (= n (alength idx)))
    ;; First element should be the smallest value (0), which was at position n-1
    (t/is (= (dec n) (aget idx 0)))
    ;; Last element should be the largest value (n-1), at position 0
    (t/is (= 0 (aget idx (dec n))))))
