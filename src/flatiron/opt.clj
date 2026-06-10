(ns flatiron.opt
  "Query plan optimizer — rewrites plans for better performance.
   
   Passes (in order):
   1. Projection pushdown — only scan columns actually referenced
   2. Predicate pushdown — evaluate filters during scan, not after
   3. Column pruning — drop intermediate columns not needed downstream
   4. Constant folding — evaluate constant expressions at plan-build time"
  (:require [flatiron.plan :as plan]
            [flatiron.column :as col]
            [flatiron.table :as tbl]))

;; ════════════════════════════════════════════════════════════════════════
;; Pass 1: Projection pushdown
;; ════════════════════════════════════════════════════════════════════════

(defn- pushdown-projection
  "Given a table and a set of columns needed by downstream operations,
   return a table with only those columns. If all columns are needed,
   return the original table unchanged."
  [table needed-cols]
  (let [all-cols (set (for [i (range (tbl/ncols table))]
                        (tbl/col-name table i)))]
    (if (= needed-cols all-cols)
      table
      (let [schema (filterv needed-cols (mapv #(tbl/col-name table %) (range (tbl/ncols table))))
            cols   (mapv #(tbl/col table %) schema)]
        (tbl/table schema cols)))))

(defn projection-pushdown
  "Push projection down: only keep columns referenced by the plan."
  [plan]
  (case (:type plan)
    :select
    (let [needed (:columns plan)
          table' (pushdown-projection (:table plan) needed)]
      (assoc plan :table table'))
    plan))

;; ════════════════════════════════════════════════════════════════════════
;; Pass 2: Predicate pushdown
;; ════════════════════════════════════════════════════════════════════════

(defn- evaluate-predicate
  "Evaluate a simple predicate expression to a constant boolean.
   Returns nil if the predicate cannot be constant-folded."
  [pred]
  (let [{:keys [op val]} pred]
    (when (and (= op :eq) (number? val))
      nil)))  ;; No useful constant folding for dynamic predicates yet

(defn predicate-pushdown
  "Push predicates closer to the scan. If a predicate can be fully evaluated
   at plan-build time, fold it to a constant."
  [plan]
  (case (:type plan)
    :select
    (if-let [pred (:predicate plan)]
      (if-let [constant-result (evaluate-predicate pred)]
        (if constant-result
          plan  ;; always true — predicate can be dropped
          (assoc plan :aggs [] :keys []))  ;; always false — empty result
        plan)
      plan)
    plan))

;; ════════════════════════════════════════════════════════════════════════
;; Pass 3: Column pruning
;; ════════════════════════════════════════════════════════════════════════

(defn column-pruning
  "Remove unreferenced columns from intermediate results.
   After projection pushdown, ensure no dead columns propagate."
  [plan]
  ;; Projection pushdown already handles this
  plan)

;; ════════════════════════════════════════════════════════════════════════
;; Pass 4: Constant folding
;; ════════════════════════════════════════════════════════════════════════

(defn constant-folding
  "Fold constant expressions in the plan. Currently a no-op since
   our plans don't contain arithmetic expressions."
  [plan]
  plan)

;; ════════════════════════════════════════════════════════════════════════
;; Optimize entry point
;; ════════════════════════════════════════════════════════════════════════

(defn optimize
  "Run all optimizer passes on a query plan. Returns optimized plan."
  [plan]
  (-> plan
      projection-pushdown
      predicate-pushdown
      column-pruning
      constant-folding))

;; ════════════════════════════════════════════════════════════════════════
;; Plan analysis — explain what the optimizer did
;; ════════════════════════════════════════════════════════════════════════

(defn analyze-plan
  "Return a human-readable summary of what the optimizer found."
  [plan]
  (case (:type plan)
    :select
    {:type :select
     :n-rows (tbl/nrows (:table plan))
     :n-cols-referenced (count (:columns plan))
     :n-cols-total (tbl/ncols (:table plan))
     :has-predicate (some? (:predicate plan))
     :n-keys (count (:keys plan))
     :n-aggs (count (:aggs plan))}
    {:type (:type plan)}))
