(ns flatiron.parallel-test
  "Specification tests for parallel execution."
    (:require [flatiron.parallel :as p]
              [flatiron.agg :as agg]
              [flatiron.arith :as arith]
              [flatiron.column :as col]
              [flatiron.table :as tbl]
              [flatiron.group :as group]
              [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; Task splitting
;; ════════════════════════════════════════════════════════════════════════

(t/deftest task-ranges-exact-division
  "4 tasks over 8 rows → [0 2] [2 4] [4 6] [6 8]"
  (let [ranges (#'flatiron.parallel/task-ranges 8 4)]
    (t/is (= 4 (count ranges)))
    (t/is (= [0 2] (nth ranges 0)))
    (t/is (= [2 4] (nth ranges 1)))
    (t/is (= [4 6] (nth ranges 2)))
    (t/is (= [6 8] (nth ranges 3)))))

(t/deftest task-ranges-uneven
  "3 tasks over 10 rows → [0 4] [4 8] [8 10]"
  (let [ranges (#'flatiron.parallel/task-ranges 10 3)]
    (t/is (= 3 (count ranges)))
    (t/is (= [0 4] (nth ranges 0)))
    (t/is (= [4 8] (nth ranges 1)))
    (t/is (= [8 10] (nth ranges 2)))))

(t/deftest task-ranges-fewer-rows-than-tasks
  "3 rows with 5 tasks → only 3 ranges"
  (let [ranges (#'flatiron.parallel/task-ranges 3 5)]
    (t/is (= 3 (count ranges)))))

(t/deftest task-ranges-single-row
  "1 row, any tasks → one range [0 1]"
  (let [ranges (#'flatiron.parallel/task-ranges 1 4)]
    (t/is (= 1 (count ranges)))
    (t/is (= [0 1] (first ranges)))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel sum (correctness matches sequential)
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-i64-sum-matches-sequential
  (let [n  (* 3 1024)   ;; 3 morsels
        c  (col/i64-column (mapv long (range n)))
        ps (p/parallel-i64-sum c)
        ss (agg/i64-sum c)]
    (t/is (= ss ps))))

(t/deftest parallel-i64-sum-with-nulls
  (let [n  1000
        c  (col/i64-column (mapv #(when (not= (rem % 3) 0) (long %)) (range n)))
        ps (p/parallel-i64-sum c)
        ss (agg/i64-sum c)]
    (t/is (= ss ps))))

(t/deftest parallel-i64-sum-small
  "Small column below parallelism threshold — still correct."
  (let [c  (col/i64-column [1 2 3 4 5])
        ps (p/parallel-i64-sum c)
        ss (agg/i64-sum c)]
    (t/is (= 15 ps))
    (t/is (= ss ps))))

(t/deftest parallel-i64-sum-empty
  (let [c  (col/i64-column [])
        ps (p/parallel-i64-sum c)]
    (t/is (= 0 ps))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel count
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-count-matches-sequential
  (let [n  2000
        c  (col/i64-column (mapv #(when (odd? %) (long %)) (range n)))
        pc (p/parallel-count-non-null c)
        sc (agg/count-non-null c)]
    (t/is (= sc pc))
    (t/is (= 1000 pc))))  ;; exactly half are non-null

(t/deftest parallel-count-f64
  (let [n  500
        c  (col/f64-column (mapv #(when (even? %) (double %)) (range n)))
        pc (p/parallel-count-non-null c)
        sc (agg/count-non-null c)]
    (t/is (= sc pc))))

(t/deftest parallel-count-bool-non-nullable
  (let [c  (col/bool-column (repeat 100 true))
        pc (p/parallel-count-non-null c)]
    (t/is (= 100 pc))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel min/max
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-i64-min-matches-sequential
  (let [c  (col/i64-column (mapv long (shuffle (range 5000))))
        pm (p/parallel-i64-min c)
        sm (agg/i64-min c)]
    (t/is (= sm pm))))

(t/deftest parallel-i64-max-matches-sequential
  (let [c  (col/i64-column (mapv long (shuffle (range 5000))))
        pm (p/parallel-i64-max c)
        sm (agg/i64-max c)]
    (t/is (= sm pm))))

(t/deftest parallel-i64-min-with-nulls
  (let [c  (col/i64-column [nil 5 nil 3 nil 8])
        pm (p/parallel-i64-min c)
        sm (agg/i64-min c)]
    (t/is (= sm pm))
    (t/is (= 3 pm))))

(t/deftest parallel-i64-max-all-null
  (let [c  (col/i64-column [nil nil])
        pm (p/parallel-i64-max c)]
    (t/is (nil? pm))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel filter count
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-filter-count-basic
  (let [c (col/bool-column (mapv #(even? %) (range 5000)))
        pc (p/parallel-filter-count c)]
    (t/is (= 2500 pc))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel arithmetic
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-i64-add-matches-sequential
  (let [n  3000
        a  (col/i64-column (mapv long (range n)))
        b  (col/i64-column (mapv #(* 2 %) (range n)))
        pr (p/parallel-i64-add a b)
        sr (arith/i64-add a b)]
    (t/is (= n (col/-len pr)))
    (t/is (= (col/-get-long sr 0) (col/-get-long pr 0)))
    (t/is (= (col/-get-long sr 100) (col/-get-long pr 100)))
    (t/is (= (col/-get-long sr (dec n)) (col/-get-long pr (dec n))))))

(t/deftest parallel-i64-add-small
  "Small column — still correct."
  (let [a  (col/i64-column [1 2 3])
        b  (col/i64-column [10 20 30])
        pr (p/parallel-i64-add a b)]
    (t/is (= 3 (col/-len pr)))
    (t/is (= 11 (col/-get-long pr 0)))
    (t/is (= 22 (col/-get-long pr 1)))
    (t/is (= 33 (col/-get-long pr 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel arithmetic with nulls
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-add-with-nulls-matches-sequential
  (let [a  (col/i64-column [1 nil 3 4 nil 6 7 8])
        b  (col/i64-column [10 20 30 40 50 60 70 80])
        pr (p/parallel-i64-add a b 3)
        sr (arith/i64-add a b)]
    (t/is (= (col/-len sr) (col/-len pr)))
    (doseq [i (range (col/-len sr))]
      (t/is (= (col/-get-long sr i) (col/-get-long pr i))
            (str "row " i)))))

;; ════════════════════════════════════════════════════════════════════════
;; Parallel group-by
;; ════════════════════════════════════════════════════════════════════════

(t/deftest parallel-group-by-matches-sequential
  (let [n   5000
        reg (col/sym-column (mapv #(keyword (str "r" (rem % 7))) (range n)))
        qty (col/i64-column (mapv #(long (rem % 13)) (range n)))
        t   (tbl/table [:Region :Qty] [reg qty])
        seq-r (group/group-by t :keys [:Region]
                              :aggs [{:agg :sum :col :Qty :out :s}
                                     {:agg :count :col :Qty :out :c}
                                     {:agg :min :col :Qty :out :mn}
                                     {:agg :max :col :Qty :out :mx}
                                     {:agg :avg :col :Qty :out :a}])
        par-r (p/parallel-group-by t :keys [:Region] :threads 4
                                   :aggs [{:agg :sum :col :Qty :out :s}
                                          {:agg :count :col :Qty :out :c}
                                          {:agg :min :col :Qty :out :mn}
                                          {:agg :max :col :Qty :out :mx}
                                          {:agg :avg :col :Qty :out :a}])
        idx-of (fn [table]
                 (into {} (for [i (range (tbl/nrows table))]
                            [(col/-get-obj (tbl/col table :Region) i) i])))
        si (idx-of seq-r)
        pi (idx-of par-r)]
    (t/is (= (tbl/nrows seq-r) (tbl/nrows par-r)))
    (doseq [k (keys si)]
      (let [a (si k) b (pi k)]
        (t/is (= (col/-get-long (tbl/col seq-r :s) a) (col/-get-long (tbl/col par-r :s) b)))
        (t/is (= (col/-get-long (tbl/col seq-r :c) a) (col/-get-long (tbl/col par-r :c) b)))
        (t/is (= (col/-get-long (tbl/col seq-r :mn) a) (col/-get-long (tbl/col par-r :mn) b)))
        (t/is (= (col/-get-long (tbl/col seq-r :mx) a) (col/-get-long (tbl/col par-r :mx) b)))
        (t/is (= (col/-get-double (tbl/col seq-r :a) a) (col/-get-double (tbl/col par-r :a) b)))))))
