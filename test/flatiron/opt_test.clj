(ns flatiron.opt-test
  "Specification tests for the query optimizer."
  (:require [flatiron.opt :as opt]
            [flatiron.plan :as plan]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.group :as group]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; Projection pushdown
;; ════════════════════════════════════════════════════════════════════════

(t/deftest projection-pushdown-prunes-unused-columns
  "When only a subset of columns is referenced, the optimizer should
   drop unreferenced columns from the table."
  (let [sym-c (col/sym-column [:a :b :a])
        qty-c (col/i64-column [10 20 30])
        prc-c (col/f64-column [1.0 2.0 3.0])
        ;; Table with 3 columns: sym, qty, price
        table (tbl/table [:sym :qty :price] [sym-c qty-c prc-c])
        ;; Plan only references :sym and :qty (not :price)
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}])
        optimized (opt/optimize p)]
    ;; Original table had 3 columns
    (t/is (= 3 (tbl/ncols (:table p))))
    ;; Optimized table should have only 2 columns (:sym and :qty)
    (t/is (= 2 (tbl/ncols (:table optimized))))
    ;; The dropped column (:price) should not be present
    (t/is (nil? (tbl/col (:table optimized) :price)))
    ;; Referenced columns should still be present
    (t/is (some? (tbl/col (:table optimized) :sym)))
    (t/is (some? (tbl/col (:table optimized) :qty)))))

(t/deftest projection-pushdown-all-columns-used
  "When all columns are referenced, the table should not be copied."
  (let [sym-c (col/sym-column [:a :b])
        qty-c (col/i64-column [10 20])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}])
        optimized (opt/optimize p)]
    ;; Should be the same table object (identity — no copy needed)
    (t/is (identical? (:table p) (:table optimized)))))

(t/deftest projection-pushdown-no-aggs
  "Plan with only keys and no aggs still prunes correctly."
  (let [sym-c (col/sym-column [:a :b])
        qty-c (col/i64-column [10 20])
        prc-c (col/f64-column [1.0 2.0])
        table (tbl/table [:sym :qty :price] [sym-c qty-c prc-c])
        p (plan/select-plan table [:sym] [])
        optimized (opt/optimize p)]
    (t/is (= 1 (tbl/ncols (:table optimized))))
    (t/is (some? (tbl/col (:table optimized) :sym)))))

;; ════════════════════════════════════════════════════════════════════════
;; Predicate pushdown
;; ════════════════════════════════════════════════════════════════════════

(t/deftest predicate-pushdown-no-predicate
  "Plan without predicate passes through unchanged."
  (let [sym-c (col/sym-column [:a :b])
        qty-c (col/i64-column [10 20])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/select-plan table [:sym] [{:agg :sum :col :qty :out :total}])
        optimized (opt/optimize p)]
    ;; Plan structure unchanged
    (t/is (= (:type p) (:type optimized)))
    (t/is (= (:keys p) (:keys optimized)))
    (t/is (= (:aggs p) (:aggs optimized)))))

(t/deftest predicate-pushdown-preserves-keys
  "Even with a predicate, key columns are still present."
  (let [sym-c (col/sym-column [:a :b :a])
        qty-c (col/i64-column [10 20 30])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}]
            :predicate {:col :qty :op :gt :val 15})
        optimized (opt/optimize p)]
    ;; Predicate column is in the referenced set, so it's kept
    (t/is (some? (tbl/col (:table optimized) :qty)))))

;; ════════════════════════════════════════════════════════════════════════
;; Plan analysis
;; ════════════════════════════════════════════════════════════════════════

(t/deftest analyze-plan-select
  (let [sym-c (col/sym-column [:a :b :a])
        qty-c (col/i64-column [10 20 30])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}])
        analysis (opt/analyze-plan p)]
    (t/is (= :select (:type analysis)))
    (t/is (= 3 (:n-rows analysis)))
    (t/is (= 2 (:n-cols-referenced analysis)))
    (t/is (= 2 (:n-cols-total analysis)))
    (t/is (false? (:has-predicate analysis)))))

(t/deftest analyze-plan-with-predicate
  (let [sym-c (col/sym-column [:a :b :a])
        qty-c (col/i64-column [10 20 30])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}]
            :predicate {:col :qty :op :gt :val 15})
        analysis (opt/analyze-plan p)]
    (t/is (true? (:has-predicate analysis)))
    (t/is (= 2 (:n-cols-referenced analysis)))))  ;; :sym + :qty (from predicate)

;; ════════════════════════════════════════════════════════════════════════
;; Integration: optimize then execute
;; ════════════════════════════════════════════════════════════════════════

(t/deftest optimize-and-execute-select
  "Optimized plan should produce correct results when executed."
  (let [sym-c (col/sym-column [:a :b :a :b :a])
        qty-c (col/i64-column [10 20 30 40 50])
        prc-c (col/f64-column [1.0 2.0 3.0 4.0 5.0])
        ;; Table with extra column that isn't referenced
        table (tbl/table [:sym :qty :price] [sym-c qty-c prc-c])
        p (plan/select-plan table
            [:sym]
            [{:agg :sum :col :qty :out :total}])
        optimized (opt/optimize p)
        ;; Execute the optimized plan
        result (group/group-by (:table optimized)
                 :keys (:keys optimized)
                 :aggs (:aggs optimized))]
    ;; Price column should be gone from the optimized table
    (t/is (= 2 (tbl/ncols (:table optimized))))
    ;; Result should be correct
    (t/is (= 2 (tbl/nrows result)))
    (let [total (reduce + (for [i (range (tbl/nrows result))]
                           (col/-get-long (tbl/col result :total) i)))]
      (t/is (= 150 total)))))

(t/deftest optimize-identity-pass-through
  "Non-select plans pass through optimizer unchanged."
  (let [sym-c (col/sym-column [:a :b])
        qty-c (col/i64-column [10 20])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        p (plan/sort-plan table [[:sym :asc]])
        optimized (opt/optimize p)]
    (t/is (= p optimized))))
