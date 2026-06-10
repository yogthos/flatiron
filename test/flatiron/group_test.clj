(ns flatiron.group-test
  "Specification tests for group-by aggregation."
  (:require [flatiron.group :as g]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.agg :as agg]
            [clojure.test :as t]))

;; ── Single key, single agg ────────────────────────────────────────────
(t/deftest group-by-sum
  (let [sym-c (col/sym-column [:a :b :a :b :a])
        qty-c (col/i64-column [10 20 30 40 50])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        result (g/group-by table :keys [:sym] :aggs [{:agg :sum :col :qty :out :total}])]
    (t/is (= 2 (tbl/ncols result)))
    (t/is (= 2 (tbl/nrows result)))
    (let [kc (tbl/col result :sym)
          tc (tbl/col result :total)
          a-idx (if (= :a (col/-get-obj kc 0)) 0 1)
          b-idx (if (= :b (col/-get-obj kc 0)) 0 1)]
      (t/is (= 90 (col/-get-long tc a-idx)))   ;; 10+30+50
      (t/is (= 60 (col/-get-long tc b-idx)))))) ;; 20+40

(t/deftest group-by-count
  (let [sym-c (col/sym-column [:x :y :x :x :y :z])
        table (tbl/table [:sym] [sym-c])
        result (g/group-by table :keys [:sym] :aggs [{:agg :count :col :sym :out :cnt}])]
    (t/is (= 3 (tbl/nrows result)))
    (let [kc (tbl/col result :sym)
          cc (tbl/col result :cnt)]
      (dotimes [i 3]
        (let [s (col/-get-obj kc i)
              c (col/-get-long cc i)]
          (case s :x (t/is (= 3 c)) :y (t/is (= 2 c)) :z (t/is (= 1 c))))))))

;; ── Multiple keys ─────────────────────────────────────────────────────
(t/deftest group-by-two-keys
  (let [r-c (col/sym-column [:US :US :CA :CA :MX])
        s-c (col/sym-column [:buy :sell :buy :sell :buy])
        qty-c (col/i64-column [100 200 150 300 175])
        table (tbl/table [:region :side :qty] [r-c s-c qty-c])
        result (g/group-by table
                :keys [:region :side]
                :aggs [{:agg :sum :col :qty :out :total}])]
    (t/is (= 5 (tbl/nrows result)))
    (let [groups (set (for [i (range 5)]
                        [(col/-get-obj (tbl/col result :region) i)
                         (col/-get-obj (tbl/col result :side) i)
                         (col/-get-long (tbl/col result :total) i)]))]
      (t/is (contains? groups [:US :buy 100]))
      (t/is (contains? groups [:US :sell 200]))
      (t/is (contains? groups [:CA :buy 150]))
      (t/is (contains? groups [:CA :sell 300]))
      (t/is (contains? groups [:MX :buy 175])))))

;; ── Multiple aggregations ─────────────────────────────────────────────
(t/deftest group-by-multiple-aggs
  (let [sym-c (col/sym-column [:a :a :b :b])
        qty-c (col/i64-column [10 20 30 40])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        result (g/group-by table
                :keys [:sym]
                :aggs [{:agg :sum :col :qty :out :total}
                       {:agg :count :col :qty :out :cnt}
                       {:agg :min :col :qty :out :min-qty}
                       {:agg :max :col :qty :out :max-qty}
                       {:agg :avg :col :qty :out :avg-qty}])]
    (t/is (= 2 (tbl/nrows result)))
    (t/is (= 6 (tbl/ncols result)))
    (let [sc (tbl/col result :sym)
          a-idx (if (= :a (col/-get-obj sc 0)) 0 1)
          b-idx (if (= :b (col/-get-obj sc 0)) 0 1)]
      (t/is (= 30 (col/-get-long (tbl/col result :total) a-idx)))
      (t/is (= 70 (col/-get-long (tbl/col result :total) b-idx)))
      (t/is (= 2 (col/-get-long (tbl/col result :cnt) a-idx)))
      (t/is (= 2 (col/-get-long (tbl/col result :cnt) b-idx)))
      (t/is (= 10 (col/-get-long (tbl/col result :min-qty) a-idx)))
      (t/is (= 30 (col/-get-long (tbl/col result :min-qty) b-idx)))
      (t/is (= 20 (col/-get-long (tbl/col result :max-qty) a-idx)))
      (t/is (= 40 (col/-get-long (tbl/col result :max-qty) b-idx)))
      (t/is (= 15.0 (col/-get-double (tbl/col result :avg-qty) a-idx)))
      (t/is (= 35.0 (col/-get-double (tbl/col result :avg-qty) b-idx))))))

;; ── F64 values ────────────────────────────────────────────────────────
(t/deftest group-by-f64-values
  (let [sym-c (col/sym-column [:a :b :a])
        f64-c (col/f64-column [1.5 2.5 3.5])
        table (tbl/table [:sym :val] [sym-c f64-c])
        result (g/group-by table
                :keys [:sym]
                :aggs [{:agg :sum :col :val :out :total}
                       {:agg :avg :col :val :out :mean}])]
    (t/is (= 2 (tbl/nrows result)))
    (let [sc (tbl/col result :sym)
          a-idx (if (= :a (col/-get-obj sc 0)) 0 1)
          b-idx (if (= :b (col/-get-obj sc 0)) 0 1)]
      (t/is (= 5.0 (col/-get-double (tbl/col result :total) a-idx)))
      (t/is (= 2.5 (col/-get-double (tbl/col result :total) b-idx)))
      (t/is (= 2.5 (col/-get-double (tbl/col result :mean) a-idx)))
      (t/is (= 2.5 (col/-get-double (tbl/col result :mean) b-idx))))))

;; ── I64 keys ──────────────────────────────────────────────────────────
(t/deftest group-by-i64-key
  (let [k-c (col/i64-column [1 2 1 2 1])
        v-c (col/i64-column [10 20 30 40 50])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 2 (tbl/nrows result)))
    (let [kc (tbl/col result :k)
          sc (tbl/col result :s)
          i1 (if (= 1 (col/-get-long kc 0)) 0 1)
          i2 (if (= 2 (col/-get-long kc 0)) 0 1)]
      (t/is (= 90 (col/-get-long sc i1)))
      (t/is (= 60 (col/-get-long sc i2))))))

;; ── Empty / edge cases ────────────────────────────────────────────────
(t/deftest group-by-empty-table
  (let [table (tbl/table [:k :v] [(col/i64-column []) (col/i64-column [])])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 0 (tbl/ncols result)))
    (t/is (= 0 (tbl/nrows result)))))

(t/deftest group-by-all-same-key
  (let [k-c (col/i64-column [5 5 5 5])
        v-c (col/i64-column [1 2 3 4])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k]
                :aggs [{:agg :sum :col :v :out :s} {:agg :count :col :v :out :c}])]
    (t/is (= 1 (tbl/nrows result)))
    (t/is (= 3 (tbl/ncols result)))
    (t/is (= 5 (col/-get-long (tbl/col result :k) 0)))
    (t/is (= 10 (col/-get-long (tbl/col result :s) 0)))
    (t/is (= 4 (col/-get-long (tbl/col result :c) 0)))))

(t/deftest group-by-unique-keys
  (let [k-c (col/i64-column [1 2 3 4 5])
        v-c (col/i64-column [10 20 30 40 50])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 5 (tbl/nrows result)))
    (t/is (= 150 (agg/i64-sum (tbl/col result :s))))))

;; ── Null handling ─────────────────────────────────────────────────────
(t/deftest group-by-nulls-in-i64-value
  (let [k-c (col/i64-column [1 1 1])
        v-c (col/i64-column [10 nil 30])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k]
                :aggs [{:agg :sum :col :v :out :s} {:agg :count :col :v :out :c}])]
    (t/is (= 1 (tbl/nrows result)))
    (t/is (= 40 (col/-get-long (tbl/col result :s) 0)))
    (t/is (= 2 (col/-get-long (tbl/col result :c) 0)))))

(t/deftest group-by-nulls-in-f64-value
  (let [k-c (col/i64-column [1 1])
        v-c (col/f64-column [1.5 nil])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 1.5 (col/-get-double (tbl/col result :s) 0)))))

(t/deftest group-by-null-key
  (let [k-c (col/i64-column [1 nil 1])
        v-c (col/i64-column [10 20 30])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 2 (tbl/nrows result)))
    (let [kc (tbl/col result :k)
          sc (tbl/col result :s)]
      (dotimes [i 2]
        (let [k (col/-get-obj kc i)]
          (if (nil? k)
            (t/is (= 20 (col/-get-long sc i)))
            (t/is (= 40 (col/-get-long sc i)))))))))

;; ── Large table ───────────────────────────────────────────────────────
(t/deftest group-by-large
  (let [n-rows (* 3 1024)
        k-c (col/i64-column (mapv #(long (rem % 10)) (range n-rows)))
        v-c (col/i64-column (mapv long (range n-rows)))
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k] :aggs [{:agg :sum :col :v :out :s}])]
    (t/is (= 10 (tbl/nrows result)))
    (let [total-sum (agg/i64-sum (tbl/col result :s))]
      (t/is (= (long (/ (* n-rows (dec n-rows)) 2)) total-sum)))))

;; ── I64 key + F64 values ──────────────────────────────────────────────
(t/deftest group-by-i64-key-f64-val
  (let [k-c (col/i64-column [1 2 1])
        v-c (col/f64-column [1.1 2.2 3.3])
        table (tbl/table [:k :v] [k-c v-c])
        result (g/group-by table :keys [:k]
                :aggs [{:agg :sum :col :v :out :s}
                       {:agg :min :col :v :out :mn}
                       {:agg :max :col :v :out :mx}])]
    (t/is (= 2 (tbl/nrows result)))
    (let [kc (tbl/col result :k)
          i1 (if (= 1 (col/-get-long kc 0)) 0 1)
          i2 (if (= 2 (col/-get-long kc 0)) 0 1)]
      (t/is (= 4.4 (col/-get-double (tbl/col result :s) i1)))
      (t/is (= 2.2 (col/-get-double (tbl/col result :s) i2)))
      (t/is (= 1.1 (col/-get-double (tbl/col result :mn) i1)))
      (t/is (= 3.3 (col/-get-double (tbl/col result :mx) i1))))))

;; ── Key distinctness ───────────────────────────────────────────────────
(t/deftest group-by-keeps-distinct-keys-separate
  (let [t (tbl/table [:k :v]
                     [(col/i64-column (vec (range 1000)))
                      (col/i64-column (vec (repeat 1000 1)))])
        r (g/group-by t :keys [:k] :aggs [{:agg :sum :col :v :out :total}])]
    (t/is (= 1000 (tbl/nrows r)))))
