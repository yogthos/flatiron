(ns flatiron.plan
  "Query plan representation — the intermediate form between the DSL
   and execution. The optimizer rewrites plans, then they execute."
  (:require [flatiron.column :as col]))

;; ════════════════════════════════════════════════════════════════════════
;; Plan types
;; ════════════════════════════════════════════════════════════════════════

;; A select plan: group-by with optional filtering
;; {:type       :select
;;  :table      <Table>
;;  :keys       [keyword ...]          ;; group-by key columns
;;  :aggs       [{:agg :sum :col kw :out kw} ...]
;;  :predicate  nil | {:col kw :op :gt :val 100}  ;; optional pre-filter
;;  :columns    #{keyword ...}}        ;; all referenced columns (for projection pushdown)

;; A sort plan
;; {:type       :sort
;;  :table      <Table>
;;  :keys       [[kw :asc|:desc] ...]}

;; A window plan
;; {:type       :window
;;  :table      <Table>
;;  :fn         :row-number|:rank|:dense-rank|:lag|:lead
;;  :args       [...]}

(defn select-plan
  "Create a select (group-by) plan."
  [table keys aggs & {:keys [predicate]}]
  (let [columns (into (set keys) (map :col aggs))]
    {:type :select
     :table table
     :keys keys
     :aggs aggs
     :predicate predicate
     :columns columns}))

(defn sort-plan [table key-specs]
  {:type :sort
   :table table
   :keys key-specs})

(defn window-plan [table fn-sym & args]
  {:type :window
   :table table
   :fn fn-sym
   :args (vec args)})

;; ════════════════════════════════════════════════════════════════════════
;; Plan analysis helpers
;; ════════════════════════════════════════════════════════════════════════

(defn referenced-columns
  "All column keywords referenced by a plan."
  [plan]
  (case (:type plan)
    :select (:columns plan)
    :sort   (into #{} (map first) (:keys plan))
    :window (case (:fn plan)
              :row-number #{}
              (let [args (:args plan)]
                (into #{} (filter keyword? args))))))

(defn predicate-columns
  "Column keywords referenced by a predicate, if any."
  [predicate]
  (when predicate
    #{(:col predicate)}))
