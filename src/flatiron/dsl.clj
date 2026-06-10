(ns flatiron.dsl
  "Query DSL — Clojure macros that compile to flatiron operations.
   
   Usage:
     (-> trades
         (where (> :Qty 100))
         (select :Symbol (sum :Qty)))
   
   Macros expand at compile time — zero runtime parsing overhead."
  (:require [flatiron.table :as tbl]
            [flatiron.group :as group]
            [flatiron.filter :as filt]))

;; ════════════════════════════════════════════════════════════════════════
;; Aggregate function markers — resolve as symbols for macro dispatch
;; ════════════════════════════════════════════════════════════════════════

(def sum   'sum)
(def count 'count)
(def avg   'avg)
(def min   'min)
(def max   'max)

;; ════════════════════════════════════════════════════════════════════════
;; Predicate compilation: (> :Qty 100) → boolean mask construction
;; ════════════════════════════════════════════════════════════════════════

(def ^:private op->kw
  {'>    :gt
   '<    :lt
   '>=   :ge
   '<=   :le
   '=    :eq
   '==   :eq
   'not= :ne})

(defn- compile-pred-mask
  "Compile a predicate form into an expression that produces a BoolColumn mask
   when evaluated against `table-sym`.

   Supports leaf comparisons (> :Qty 100), and the connectives `and`, `or`,
   and `not` over nested predicates."
  [table-sym form]
  (when-not (seq? form)
    (throw (IllegalArgumentException. (str "Malformed predicate: " form))))
  (let [head (first form)]
    (case head
      (and) `(filt/bool-and ~@(map #(compile-pred-mask table-sym %) (rest form)))
      (or)  `(filt/bool-or  ~@(map #(compile-pred-mask table-sym %) (rest form)))
      (not) `(filt/bool-not ~(compile-pred-mask table-sym (second form)))
      (let [[op col-kw val] form
            opk (or (op->kw op)
                    (throw (IllegalArgumentException.
                            (str "Unsupported predicate operator: " op))))]
        `(filt/scalar-pred (tbl/col ~table-sym ~col-kw) ~opk ~val)))))

;; ════════════════════════════════════════════════════════════════════════
;; Aggregation compilation: (sum :Qty) → {:agg :sum :col :Qty :out :Qty}
;; ════════════════════════════════════════════════════════════════════════

(defn- compile-agg
  "Compile an aggregate spec into {:agg kw :col kw :out kw}."
  [form]
  (let [[agg-fn col-kw] form
        agg-name (name agg-fn)]
    {:agg  (case agg-name
             "sum"   :sum
             "count" :count
             "avg"   :avg
             "min"   :min
             "max"   :max
             (throw (IllegalArgumentException.
                     (str "Unknown aggregate: " agg-fn))))
     :col  col-kw
     :out  (keyword (str agg-name "-" (name col-kw)))}))

;; ════════════════════════════════════════════════════════════════════════
;; where macro
;; ════════════════════════════════════════════════════════════════════════

(defmacro where
  "Filter a table by a predicate.

   Usage:
     (where table (> :Qty 100))
     (where table (and (> :Qty 100) (< :Price 50.0)))

   Returns a table with only matching rows."
  [table pred-form]
  (let [t (gensym "table")]
    `(let [~t ~table]
       (filt/filter-rows ~t ~(compile-pred-mask t pred-form)))))

;; ════════════════════════════════════════════════════════════════════════
;; select macro — compile to group-by
;; ════════════════════════════════════════════════════════════════════════

(defmacro select
  "Select columns with optional group-by and aggregation.
   
   Usage:
     (select table :Symbol (sum :Qty))
   
   Non-aggregate columns become group-by keys.
   Aggregate forms like (sum :Qty) become aggregation specs."
  [table & forms]
  (let [;; Separate key columns from aggregates
        specs (reduce (fn [{:keys [keys aggs]} form]
                        (if (list? form)
                          {:keys keys :aggs (conj aggs (compile-agg form))}
                          {:keys (conj keys form) :aggs aggs}))
                      {:keys [] :aggs []}
                      forms)]
    `(group/group-by ~table
       :keys ~(:keys specs)
       :aggs ~(:aggs specs))))

;; ════════════════════════════════════════════════════════════════════════
;; pivot macro — cross-tabulation
;; ════════════════════════════════════════════════════════════════════════

(defmacro pivot
  "Create a pivot table (cross-tabulation).

   Usage:
     (pivot table :Symbol :Side :Qty sum)

   Rows become unique values of the first column, columns become unique values
   of the second column, and each cell is the aggregation of the value column
   for that (row, column) pair. Cells with no matching rows are null."
  [table row-col col-col val-col agg-fn]
  (let [agg-kw (keyword (name agg-fn))]
    (when-not (#{:sum :count :avg :min :max} agg-kw)
      (throw (IllegalArgumentException. (str "Unknown aggregate: " agg-fn))))
    `(group/pivot-table ~table ~row-col ~col-col ~val-col ~agg-kw)))
