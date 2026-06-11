(ns flatiron.group-regression-test
  "Regression tests for group-by correctness bugs found in review of the
   aggregation/group-by optimization commit."
  (:require [flatiron.group :as g]
            [flatiron.column :as col]
            [flatiron.filter :as filt]
            [flatiron.table :as tbl]
            [flatiron.agg :as agg]
            [clojure.test :as t]))

;; fast-long-hash (splitmix64) maps this value to exactly -1, which collides
;; with the HT-EMPTY slot sentinel in the parallel partition hash table.
(def ^:const hash-neg-one-key 3558559446808474027)

(defn- result-rows
  "Normalize a group-by result table to a set of row vectors of objects."
  [result]
  (let [nc (tbl/ncols result)
        nr (tbl/nrows result)]
    (set (for [r (range nr)]
           (mapv #(col/-get-obj (tbl/col-by-idx result %) r) (range nc))))))

;; ── Bug 1: parallel HT treats hash == HT-EMPTY as an empty slot ───────
(t/deftest parallel-key-hashing-to-empty-sentinel
  (let [k (col/i64-column (repeat 5 hash-neg-one-key))
        v (col/i64-column [1 2 3 4 5])
        table (tbl/table [:k :v] [k v])
        result (g/parallel-group-by table :keys [:k]
                                    :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 1 (tbl/nrows result)) "all rows share one key, expect one group")
    (t/is (= #{[hash-neg-one-key 15]} (result-rows result)))))

;; ── Bug 2: parallel merge drops has-nulls on key columns ──────────────
(t/deftest parallel-preserves-null-keys
  (let [ks (map #(if (zero? (mod % 7)) nil %) (range 64))
        table (tbl/table [:k :v] [(col/i64-column ks)
                                  (col/i64-column (range 64))])
        st (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])
        pt (g/parallel-group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= (result-rows st) (result-rows pt)))
    (t/is (contains? (set (map first (result-rows pt))) nil)
          "null key must come back as nil from the parallel path")))

;; ── Bug 3: :count agg over a bool column throws ───────────────────────
(t/deftest count-agg-on-bool-column
  (let [table (tbl/table [:k :b] [(col/i64-column [1 1 2])
                                  (col/bool-column [true false true])])
        result (g/group-by table :keys [:k] :aggs [{:agg :count :col :b :out :cnt}])]
    (t/is (= #{[1 2] [2 1]} (result-rows result)))))

;; ── Bug 4: min/max over an all-null group yields sentinel garbage ─────
(t/deftest min-max-all-null-group-i64
  (let [table (tbl/table [:k :v] [(col/i64-column [1 1 2 2])
                                  (col/i64-column [nil nil 5 9])])
        result (g/group-by table :keys [:k]
                           :aggs [{:agg :min :col :v :out :mn}
                                  {:agg :max :col :v :out :mx}])]
    (t/is (= #{[1 nil nil] [2 5 9]} (result-rows result)))))

(t/deftest min-max-all-null-group-f64
  (let [table (tbl/table [:k :v] [(col/i64-column [1 1 2 2])
                                  (col/f64-column [nil nil 1.0 2.0])])
        result (g/group-by table :keys [:k]
                           :aggs [{:agg :min :col :v :out :mn}
                                  {:agg :max :col :v :out :mx}])]
    (t/is (= #{[1 nil nil] [2 1.0 2.0]} (result-rows result)))))

;; ── Bug 5: f64 min/max seeded with ±MAX_VALUE mishandle infinities ────
(t/deftest f64-min-max-infinity-agg
  (t/is (= ##Inf (agg/f64-min (col/f64-column [##Inf nil]))))
  (t/is (= ##-Inf (agg/f64-max (col/f64-column [##-Inf nil])))))

(t/deftest f64-min-max-infinity-group
  (let [table (tbl/table [:k :v] [(col/i64-column [1 1])
                                  (col/f64-column [##Inf nil])])
        result (g/group-by table :keys [:k]
                           :aggs [{:agg :min :col :v :out :mn}]) ]
    (t/is (= #{[1 ##Inf]} (result-rows result)))))

;; ── Bug 6: pivot :sum returns 0.0 for cells whose rows are all null ───
(t/deftest pivot-sum-all-null-cell
  (let [table (tbl/table [:r :c :v]
                         [(col/sym-column [:a :a :b])
                          (col/sym-column [:x :y :x])
                          (col/i64-column [nil 2 3])])
        result (g/pivot-table table :r :c :v :sum)
        rows (result-rows result)]
    ;; row :a / col :x has one row whose value is null — the cell is null
    (t/is (contains? rows [:a nil 2]))
    (t/is (contains? rows [:b 3 nil]))))

;; ── Str keys: prehash used to cast every obj key column to SymColumn ──
(t/deftest group-by-str-keys
  (let [table (tbl/table [:k :v] [(col/str-column ["a" "b" "a"])
                                  (col/i64-column [1 2 3])])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])
        presult (g/parallel-group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= #{["a" 4] ["b" 2]} (result-rows result)))
    (t/is (= #{["a" 4] ["b" 2]} (result-rows presult)))))

;; ── Fused filter+group-by: :where must equal filter-then-group ────────
(t/deftest fused-where-group-by
  (let [n 2000
        rnd (java.util.Random. 7)
        ks (repeatedly n #(let [x (.nextInt rnd 20)] (when (pos? x) (long x))))
        vs (repeatedly n #(long (.nextInt rnd 1000)))
        table (tbl/table [:k :v] [(col/i64-column ks) (col/i64-column vs)])
        mask (filt/scalar-pred (tbl/col table :v) :gt 500)
        aggs [{:agg :sum :col :v :out :s} {:agg :min :col :v :out :mn}]
        expected (result-rows (g/group-by (filt/filter-rows table mask)
                                          :keys [:k] :aggs aggs))]
    (t/is (= expected (result-rows (g/group-by table :keys [:k] :aggs aggs
                                               :where mask))))
    (t/is (= expected (result-rows (g/parallel-group-by table :keys [:k] :aggs aggs
                                                        :where mask))))))

(t/deftest fused-where-empty-selection
  (let [table (tbl/table [:k :v] [(col/i64-column [1 2]) (col/i64-column [3 4])])
        mask (filt/scalar-pred (tbl/col table :v) :gt 100)]
    (t/is (zero? (tbl/nrows (g/group-by table :keys [:k]
                                        :aggs [{:agg :sum :col :v :out :s}]
                                        :where mask))))
    (t/is (zero? (tbl/nrows (g/parallel-group-by table :keys [:k]
                                                 :aggs [{:agg :sum :col :v :out :s}]
                                                 :where mask))))))

;; ── Equivalence: parallel must agree with single-threaded ─────────────
(t/deftest parallel-equals-single-threaded
  (let [n 5000
        rnd (java.util.Random. 42)
        ks1 (repeatedly n #(let [x (.nextInt rnd 50)] (when (pos? x) (long x))))
        ks2 (repeatedly n #(nth [:a :b :c :d] (.nextInt rnd 4)))
        vs1 (repeatedly n #(let [x (.nextInt rnd 1000)] (when (pos? (mod x 11)) (long x))))
        vs2 (repeatedly n #(let [x (.nextInt rnd 1000)] (when (pos? (mod x 13)) (double x))))
        table (tbl/table [:k1 :k2 :v1 :v2]
                         [(col/i64-column ks1) (col/sym-column ks2)
                          (col/i64-column vs1) (col/f64-column vs2)])
        aggs [{:agg :sum :col :v1 :out :s1}
              {:agg :min :col :v1 :out :mn1}
              {:agg :max :col :v1 :out :mx1}
              {:agg :count :col :v1 :out :c1}
              {:agg :sum :col :v2 :out :s2}
              {:agg :avg :col :v2 :out :a2}]]
    (doseq [keyset [[:k1] [:k1 :k2]]]
      (let [st (apply g/group-by table (mapcat identity {:keys keyset :aggs aggs}))
            pt (apply g/parallel-group-by table (mapcat identity {:keys keyset :aggs aggs}))]
        (t/is (= (tbl/nrows st) (tbl/nrows pt)))
        (t/is (= (result-rows st) (result-rows pt))
              (str "parallel != single for keys " keyset))))))
