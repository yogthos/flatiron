(ns flatiron.dsl-test
  "Specification tests for the query DSL."
  (:require [flatiron.dsl :as dsl :refer [sum count avg min max]]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.sort :as sort]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — select with group-by aggregation
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dsl-select-simple-group-by
  "select with one key and one aggregate compiles to group-by."
  (let [sym-c (col/sym-column [:a :b :a :b :a])
        qty-c (col/i64-column [10 20 30 40 50])
        table (tbl/table [:Symbol :Qty] [sym-c qty-c])
        result (dsl/select table :Symbol (sum :Qty))]
    (t/is (= 2 (tbl/nrows result)))
    ;; Key column present
    (let [kc (tbl/col result :Symbol)]
      (t/is (some? kc)))
    ;; Agg column present
    (let [ac (tbl/col result :sum-Qty)]
      (t/is (some? ac))
      (t/is (= :i64 (col/-type-tag ac)))
      ;; Find sum for each group
      (let [total (reduce + (for [i (range (tbl/nrows result))]
                              (col/-get-long ac i)))]
        (t/is (= 150 total))))))  ;; 10+20+30+40+50 = 150

(t/deftest dsl-select-multiple-keys
  "select with two keys compiles correctly."
  (let [r-c   (col/sym-column [:US :US :CA :CA :MX])
        s-c   (col/sym-column [:buy :sell :buy :sell :buy])
        qty-c (col/i64-column [100 200 150 300 175])
        table (tbl/table [:Region :Side :Qty] [r-c s-c qty-c])
        result (dsl/select table :Region :Side (sum :Qty))]
    (t/is (= 5 (tbl/nrows result)))
    (let [groups (for [i (range 5)]
                   [(col/-get-obj (tbl/col result :Region) i)
                    (col/-get-obj (tbl/col result :Side) i)
                    (col/-get-long (tbl/col result :sum-Qty) i)])]
      (t/is (contains? (set groups) [:US :buy 100]))
      (t/is (contains? (set groups) [:US :sell 200]))
      (t/is (contains? (set groups) [:CA :buy 150]))
      (t/is (contains? (set groups) [:CA :sell 300]))
      (t/is (contains? (set groups) [:MX :buy 175])))))

(t/deftest dsl-select-multiple-aggs
  "Multiple aggregate functions on the same table."
  (let [sym-c (col/sym-column [:a :a :b :b])
        qty-c (col/i64-column [10 20 30 40])
        table (tbl/table [:Symbol :Qty] [sym-c qty-c])
        result (dsl/select table :Symbol (sum :Qty) (count :Qty))]
    (t/is (= 2 (tbl/nrows result)))
    (t/is (= 3 (tbl/ncols result)))
    ;; Verify both agg columns exist
    (t/is (some? (tbl/col result :sum-Qty)))
    (t/is (some? (tbl/col result :count-Qty)))))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — Rayfall REPL examples from README
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dsl-rayfall-example-1
  "Match the first Rayfall example from the README:
   (set t (table [Symbol Side Qty] ...))
   (select {from:t by: Symbol Qty: (sum Qty)})"
  (let [sym-c (col/sym-column ["AAPL" "GOOG" "MSFT" "AAPL" "GOOG"])
        side-c (col/sym-column ["Buy" "Sell" "Buy" "Sell" "Buy"])
        qty-c (col/i64-column [100 200 150 300 250])
        t (tbl/table [:Symbol :Side :Qty] [sym-c side-c qty-c])
        result (dsl/select t :Symbol (sum :Qty))]
    (let [kc (tbl/col result :Symbol)
          ac (tbl/col result :sum-Qty)]
      ;; Check that we have 3 groups (AAPL, GOOG, MSFT)
      (t/is (= 3 (tbl/nrows result)))
      ;; AAPL: 100+300=400, GOOG: 200+250=450, MSFT: 150
      (let [groups (for [i (range 3)]
                     [(col/-get-obj kc i) (col/-get-long ac i)])]
        (t/is (contains? (set groups) ["AAPL" 400]))
        (t/is (contains? (set groups) ["GOOG" 450]))
        (t/is (contains? (set groups) ["MSFT" 150]))))))

(t/deftest dsl-rayfall-example-2-pivot
  "Match the second Rayfall example:
   (pivot t 'Symbol 'Side 'Qty sum)"
  (let [sym-c (col/sym-column ["AAPL" "GOOG" "MSFT" "AAPL" "GOOG"])
        side-c (col/sym-column ["Buy" "Sell" "Buy" "Sell" "Buy"])
        qty-c (col/i64-column [100 200 150 300 250])
        t (tbl/table [:Symbol :Side :Qty] [sym-c side-c qty-c])
        result (dsl/pivot t :Symbol :Side :Qty sum)]
    (t/is (= 3 (tbl/nrows result)))
    (t/is (some? (tbl/col result :Symbol)))))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — Edge cases
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dsl-select-empty
  (let [table (tbl/table [:k :v] [(col/i64-column []) (col/i64-column [])])
        result (dsl/select table :k (sum :v))]
    (t/is (= 0 (tbl/ncols result)))))

(t/deftest dsl-select-no-agg
  "select with only key columns (no agg) — still works as group-by with count."
  (let [sym-c (col/sym-column [:a :b :a :b :a])
        table (tbl/table [:Symbol] [sym-c])
        result (dsl/select table :Symbol)]
    (t/is (= 2 (tbl/nrows result)))))

(t/deftest dsl-select-f64-values
  "Aggregation on F64 values."
  (let [sym-c (col/sym-column [:a :b :a])
        f64-c (col/f64-column [1.5 2.5 3.5])
        table (tbl/table [:Symbol :Val] [sym-c f64-c])
        result (dsl/select table :Symbol (sum :Val))]
    (t/is (= 2 (tbl/nrows result)))
    (let [ac (tbl/col result :sum-Val)
          total (reduce + (for [i (range (tbl/nrows result))]
                           (col/-get-double ac i)))]
      (t/is (= 7.5 total)))))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — integration with sort
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dsl-select-with-sort
  "Chain: select then sort the result."
  (let [sym-c (col/sym-column [:b :c :a :b :a])
        qty-c (col/i64-column [10 20 30 40 50])
        table (tbl/table [:Symbol :Qty] [sym-c qty-c])
        ;; Step 1: group-by
        grouped (dsl/select table :Symbol (sum :Qty))
        ;; Step 2: sort by Symbol
        sorted (sort/sort-table grouped [[:Symbol :asc]])]
    (t/is (= :a (col/-get-obj (tbl/col sorted :Symbol) 0)))
    (t/is (= :b (col/-get-obj (tbl/col sorted :Symbol) 1)))
    (t/is (= :c (col/-get-obj (tbl/col sorted :Symbol) 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — where filtering
;; ════════════════════════════════════════════════════════════════════════

(defn- col->vec [c]
  (mapv #(col/-get-obj c %) (range (col/-len c))))

(t/deftest where-filters-rows
  (let [t (tbl/table [:Qty] [(col/i64-column [50 100 150 200])])
        r (dsl/where t (> :Qty 100))]
    (t/is (= [150 200] (col->vec (tbl/col r :Qty))))))

(t/deftest where-ge-includes-equal
  (let [t (tbl/table [:Qty] [(col/i64-column [50 100 150 200])])
        r (dsl/where t (>= :Qty 100))]
    (t/is (= [100 150 200] (col->vec (tbl/col r :Qty))))))

(t/deftest where-le-includes-equal
  (let [t (tbl/table [:Qty] [(col/i64-column [50 100 150 200])])
        r (dsl/where t (<= :Qty 100))]
    (t/is (= [50 100] (col->vec (tbl/col r :Qty))))))

(t/deftest where-coerces-int-literal-against-f64-column
  (let [t (tbl/table [:Price] [(col/f64-column [1.0 5.0 10.0])])
        r (dsl/where t (> :Price 5))]
    (t/is (= [10.0] (col->vec (tbl/col r :Price))))))

(t/deftest where-and-combines-predicates
  (let [t (tbl/table [:Qty :Price]
                     [(col/i64-column [50 150 250 150])
                      (col/f64-column [10.0 20.0 30.0 60.0])])
        r (dsl/where t (and (> :Qty 100) (< :Price 50.0)))]
    (t/is (= [150 250] (col->vec (tbl/col r :Qty))))))

(t/deftest where-then-select
  (let [t (tbl/table [:Symbol :Qty]
                     [(col/sym-column [:a :b :a :b])
                      (col/i64-column [100 50 200 300])])
        r (-> t (dsl/where (> :Qty 100)) (dsl/select :Symbol (sum :Qty)))]
    (t/is (= 2 (tbl/nrows r)))
    (let [groups (set (for [i (range 2)]
                        [(col/-get-obj (tbl/col r :Symbol) i)
                         (col/-get-long (tbl/col r :sum-Qty) i)]))]
      (t/is (contains? groups [:a 200]))
      (t/is (contains? groups [:b 300])))))

;; ════════════════════════════════════════════════════════════════════════
;; DSL — pivot
;; ════════════════════════════════════════════════════════════════════════

(t/deftest pivot-cross-tabulates
  (let [t (tbl/table [:Symbol :Side :Qty]
                     [(col/sym-column [:AAPL :AAPL :GOOG :GOOG :AAPL])
                      (col/sym-column [:Buy :Sell :Buy :Sell :Buy])
                      (col/i64-column [100 200 150 300 50])])
        r (dsl/pivot t :Symbol :Side :Qty sum)
        rows (into {} (for [i (range (tbl/nrows r))]
                        [(col/-get-obj (tbl/col r :Symbol) i) i]))]
    (t/is (= #{:AAPL :GOOG} (set (keys rows))))
    (t/is (= 150 (col/-get-long (tbl/col r :Buy) (rows :AAPL))))
    (t/is (= 200 (col/-get-long (tbl/col r :Sell) (rows :AAPL))))
    (t/is (= 150 (col/-get-long (tbl/col r :Buy) (rows :GOOG))))
    (t/is (= 300 (col/-get-long (tbl/col r :Sell) (rows :GOOG))))))

(t/deftest pivot-missing-cells-are-null
  (let [t (tbl/table [:R :C :V]
                     [(col/sym-column [:x :y])
                      (col/sym-column [:a :b])
                      (col/i64-column [10 20])])
        r (dsl/pivot t :R :C :V sum)
        rows (into {} (for [i (range (tbl/nrows r))]
                        [(col/-get-obj (tbl/col r :R) i) i]))]
    (t/is (= 10 (col/-get-long (tbl/col r :a) (rows :x))))
    (t/is (nil? (col/-get-obj (tbl/col r :b) (rows :x))))
    (t/is (nil? (col/-get-obj (tbl/col r :a) (rows :y))))
    (t/is (= 20 (col/-get-long (tbl/col r :b) (rows :y))))))
