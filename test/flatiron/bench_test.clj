(ns flatiron.bench-test
  "Flatiron benchmarks using Criterium.
   Compares Flatiron columnar operations against raw Java array loops."
  (:require [criterium.core :as crit]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.agg :as agg]
            [flatiron.group :as group]
            [flatiron.sort :as sort]
            [flatiron.dsl :as dsl :refer [sum count avg min max]])
  (:import [flatiron.column I64Column F64Column SymColumn]
           [java.util Random]))

;; ═══════════════════════════════════════════════════════════════════════════
;; Test data generation
;; ═══════════════════════════════════════════════════════════════════════════

(def ^:private POOL
  (object-array [:AAPL :GOOG :MSFT :AMZN :META :TSLA :NVDA :JPM :BAC :WMT]))

(defn- rng ^Random []
  (Random. 42))

(defn- random-longs [^long n]
  (let [^longs arr (long-array n)
        ^Random r (rng)]
    (loop [i (int 0)]
      (when (< i n)
        (aset arr i (.nextLong r))
        (recur (unchecked-inc i))))
    arr))

(defn- random-doubles [^long n]
  (let [^doubles arr (double-array n)
        ^Random r (rng)]
    (loop [i (int 0)]
      (when (< i n)
        (aset arr i (.nextDouble r))
        (recur (unchecked-inc i))))
    arr))

(defn- random-syms [^long n]
  (let [^objects arr (object-array n)
        ^Random r (rng)]
    (loop [i (int 0)]
      (when (< i n)
        (aset arr i (aget POOL (.nextInt r 10)))
        (recur (unchecked-inc i))))
    arr))

(defmacro ^:private defcol [name n gen]
  `(let [a# (~gen ~n)]
     (intern 'flatiron.bench-test '~name
             (case (first '~gen)
               random-longs   (I64Column. a# (alength a#) 0 false)
               random-doubles (F64Column. a# (alength a#) 0 false)
               random-syms    (SymColumn. a# (alength a#) 0 false)))))

;; ═══════════════════════════════════════════════════════════════════════════
;; Shared data
;; ═══════════════════════════════════════════════════════════════════════════

(def sizes [100000 1000000])
(def group-sizes [10000])  ;; group-by/sort are O(n log n) — keep small for Criterium

(defn build-tables []
  (println "\nBuilding test data...")
  (flush)
  (doseq [n (distinct (concat sizes group-sizes))]
    (println (format "  %d rows..." n))
    (flush)
    (let [qty-col   (I64Column. (random-longs n) n 0 false)
          price-col (F64Column. (random-doubles n) n 0 false)
          sym-col   (SymColumn. (random-syms n) n 0 false)
          table     (tbl/table [:Sym :Qty :Price] [sym-col qty-col price-col])]
      (intern 'flatiron.bench-test (symbol (str "table-" n)) table)
      (intern 'flatiron.bench-test (symbol (str "qty-" n)) qty-col)
      (intern 'flatiron.bench-test (symbol (str "price-" n)) price-col)
      (intern 'flatiron.bench-test (symbol (str "sym-" n)) sym-col)))
  (println "  done.\n")
  (flush))

;; ═══════════════════════════════════════════════════════════════════════════
;; Raw Java array baselines
;; ═══════════════════════════════════════════════════════════════════════════

(defn- raw-long-sum ^long [^longs arr]
  (loop [i 0, acc 0]
    (if (< i (alength arr))
      (recur (unchecked-inc i) (unchecked-add acc (aget arr i)))
      acc)))

(defn- raw-double-sum ^double [^doubles arr]
  (loop [i 0, acc 0.0]
    (if (< i (alength arr))
      (recur (unchecked-inc i) (+ acc (aget arr i)))
      acc)))

(defn- raw-long-avg ^double [^longs arr]
  (let [n (alength arr)]
    (loop [i 0, acc 0.0]
      (if (< i n)
        (recur (unchecked-inc i) (+ acc (aget arr i)))
        (* 1.0 (/ acc n))))))

;; ═══════════════════════════════════════════════════════════════════════════
;; Helper: get benchmark var
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-var [base n]
  (let [sym (symbol (str base "-" n))]
    @(or (ns-resolve 'flatiron.bench-test sym)
         (throw (Exception. (str "Benchmark var not found: " sym))))))

(defmacro ^:private bm-label [label]
  `(do (print ~label) (flush) (crit/quick-bench ~(last label))))

;; ═══════════════════════════════════════════════════════════════════════════
;; I64 SUM
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-i64-sum []
  (println "\n═══ I64 Sum ═══")
  (doseq [n sizes]
    (let [col    (bench-var "qty" n)
          n-rows (col/-len col)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron i64-sum: ")
      (flush)
      (crit/quick-bench (agg/i64-sum col))
      (print "  Raw long[] loop:  ")
      (flush)
      (let [^longs arr (.data ^I64Column col)]
        (crit/quick-bench (raw-long-sum arr))))))

;; ═══════════════════════════════════════════════════════════════════════════
;; F64 SUM
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-f64-sum []
  (println "\n═══ F64 Sum ═══")
  (doseq [n sizes]
    (let [col    (bench-var "price" n)
          n-rows (col/-len col)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron f64-sum:  ")
      (flush)
      (crit/quick-bench (agg/f64-sum col))
      (print "  Raw double[] loop: ")
      (flush)
      (let [^doubles arr (.data ^F64Column col)]
        (crit/quick-bench (raw-double-sum arr))))))

;; ═══════════════════════════════════════════════════════════════════════════
;; I64 AVG
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-i64-avg []
  (println "\n═══ I64 Average ═══")
  (doseq [n sizes]
    (let [col    (bench-var "qty" n)
          n-rows (col/-len col)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron i64-avg:      ")
      (flush)
      (crit/quick-bench (agg/i64-avg col))
      (print "  Raw long[] loop (avg): ")
      (flush)
      (let [^longs arr (.data ^I64Column col)]
        (crit/quick-bench (raw-long-avg arr))))))

;; ═══════════════════════════════════════════════════════════════════════════
;; GROUP-BY: single sym key, i64 sum
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-group-by []
  (println "\n═══ Group-By (sym key, i64 sum) ═══")
  (doseq [n group-sizes]
    (let [table  (bench-var "table" n)
          n-rows (tbl/nrows table)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron group-by:  ")
      (flush)
      (crit/quick-bench (group/group-by table
                          :keys [:Sym]
                          :aggs [{:agg :sum :col :Qty :out :total}])))))

;; ═══════════════════════════════════════════════════════════════════════════
;; GROUP-BY: single sym key, multiple aggs (Flatiron + DSL only)
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-group-by-multi []
  (println "\n═══ Group-By (sym key, sum + count + avg) ═══")
  (doseq [n group-sizes]
    (let [table  (bench-var "table" n)
          n-rows (tbl/nrows table)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron group-by multi-agg: ")
      (flush)
      (crit/quick-bench (group/group-by table
                          :keys [:Sym]
                          :aggs [{:agg :sum :col :Qty :out :total-qty}
                                 {:agg :count :col :Qty :out :cnt}
                                 {:agg :avg :col :Qty :out :avg-qty}]))
      (print "  DSL select (3 aggs): ")
      (flush)
      (crit/quick-bench (dsl/select table :Sym (sum :Qty) (count :Qty) (avg :Qty))))))

;; ═══════════════════════════════════════════════════════════════════════════
;; SORT (Flatiron only — 1M rows is slow enough)
;; ═══════════════════════════════════════════════════════════════════════════

(defn bench-sort []
  (println "\n═══ Sort (by :Qty asc) ═══")
  (doseq [n group-sizes]
    (let [table  (bench-var "table" n)
          n-rows (tbl/nrows table)]
      (println (format "\n--- %d rows ---" n-rows))
      (print "  Flatiron sort-table: ")
      (flush)
      (crit/quick-bench (sort/sort-table table [[:Qty :asc]])))))

;; ═══════════════════════════════════════════════════════════════════════════
;; Run all
;; ═══════════════════════════════════════════════════════════════════════════

(defn run-all []
  (println "Flatiron Benchmark Suite")
  (println "========================")
  (println (format "Scales: %s rows\n" (clojure.string/join ", " sizes)))
  (build-tables)
  (bench-i64-sum)
  (bench-f64-sum)
  (bench-i64-avg)
  (bench-group-by)
  (bench-group-by-multi)
  (bench-sort)
  (println "\nDone."))
