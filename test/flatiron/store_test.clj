(ns flatiron.store-test
  "Specification tests for columnar binary storage — save-table and load-table."
  (:require [flatiron.store :as store]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [clojure.test :as t])
  (:import [java.io File]))

(defn- tmp-dir [name]
  (let [d (File. (str "/tmp/flatiron-test-" name))]
    (when (.exists d)
      (doseq [f (reverse (file-seq d))] (.delete f)))
    d))

(t/deftest save-load-i64-table
  (let [t (tbl/table [:a :b] [(col/i64-column [1 2 3]) (col/i64-column [10 20 30])])
        d (tmp-dir "i64")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= 2 (tbl/ncols loaded)))
      (t/is (= 3 (tbl/nrows loaded)))
      (t/is (= :i64 (col/-type-tag (tbl/col loaded :a))))
      (t/is (= 1 (col/-get-long (tbl/col loaded :a) 0)))
      (t/is (= 20 (col/-get-long (tbl/col loaded :b) 1))))))

(t/deftest save-load-f64-table
  (let [t (tbl/table [:val] [(col/f64-column [1.5 3.14 2.718])])
        d (tmp-dir "f64")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= :f64 (col/-type-tag (tbl/col loaded :val))))
      (t/is (= 1.5 (col/-get-double (tbl/col loaded :val) 0))))))

(t/deftest save-load-bool-table
  (let [t (tbl/table [:flag] [(col/bool-column [true false true])])
        d (tmp-dir "bool")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= :bool (col/-type-tag (tbl/col loaded :flag))))
      (t/is (true? (col/-get-obj (tbl/col loaded :flag) 0))))))

(t/deftest save-load-sym-table
  (let [t (tbl/table [:sym] [(col/sym-column [:buy :sell :buy])])
        d (tmp-dir "sym")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= :sym (col/-type-tag (tbl/col loaded :sym))))
      (t/is (= :buy (col/-get-obj (tbl/col loaded :sym) 0))))))

(t/deftest save-load-str-table
  (let [t (tbl/table [:s] [(col/str-column ["hello" "world" "!"])])
        d (tmp-dir "str")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= :str (col/-type-tag (tbl/col loaded :s))))
      (t/is (= "hello" (col/-get-obj (tbl/col loaded :s) 0))))))

(t/deftest save-load-mixed-table
  (let [t (tbl/table [:sym :qty :price :flag]
             [(col/sym-column [:a :b :a])
              (col/i64-column [10 20 30])
              (col/f64-column [1.5 2.5 3.5])
              (col/bool-column [true false true])])
        d (tmp-dir "mixed")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= 4 (tbl/ncols loaded)))
      (t/is (= 3 (tbl/nrows loaded)))
      (t/is (= :sym (col/-type-tag (tbl/col loaded :sym))))
      (t/is (= :i64 (col/-type-tag (tbl/col loaded :qty))))
      (t/is (= :f64 (col/-type-tag (tbl/col loaded :price))))
      (t/is (= :bool (col/-type-tag (tbl/col loaded :flag)))))))

(t/deftest save-load-empty-table
  (let [t (tbl/table [:a :b] [(col/i64-column []) (col/f64-column [])])
        d (tmp-dir "empty")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= 2 (tbl/ncols loaded)))
      (t/is (= 0 (tbl/nrows loaded))))))

(t/deftest save-load-large-table
  (let [n (* 3 1024)
        t (tbl/table [:val] [(col/i64-column (mapv long (range n)))])
        d (tmp-dir "large")]
    (store/save-table t (.getPath d))
    (let [loaded (store/load-table (.getPath d))]
      (t/is (= n (tbl/nrows loaded)))
      (t/is (= 0 (col/-get-long (tbl/col loaded :val) 0))))))

;; ════════════════════════════════════════════════════════════════════════
;; Null round-trips
;; ════════════════════════════════════════════════════════════════════════

(t/deftest store-roundtrips-i64-nulls
  (let [t (tbl/table [:n] [(col/i64-column [1 nil 3])])
        d (tmp-dir "i64-null")]
    (store/save-table t (.getPath d))
    (let [c (tbl/col (store/load-table (.getPath d)) :n)]
      (t/is (true? (col/-has-nulls? c)))
      (t/is (= 1 (col/-get-long c 0)))
      (t/is (nil? (col/-get-obj c 1)))
      (t/is (= 3 (col/-get-long c 2))))))

(t/deftest store-roundtrips-f64-nulls
  (let [t (tbl/table [:n] [(col/f64-column [1.5 nil 3.5])])
        d (tmp-dir "f64-null")]
    (store/save-table t (.getPath d))
    (let [c (tbl/col (store/load-table (.getPath d)) :n)]
      (t/is (true? (col/-has-nulls? c)))
      (t/is (= 1.5 (col/-get-double c 0)))
      (t/is (nil? (col/-get-obj c 1)))
      (t/is (= 3.5 (col/-get-double c 2))))))

(t/deftest store-distinguishes-empty-string-from-null
  (let [t (tbl/table [:s] [(col/str-column ["a" "" nil "c"])])
        d (tmp-dir "str-empty")]
    (store/save-table t (.getPath d))
    (let [c (tbl/col (store/load-table (.getPath d)) :s)]
      (t/is (= "a" (col/-get-obj c 0)))
      (t/is (= "" (col/-get-obj c 1)))
      (t/is (nil? (col/-get-obj c 2)))
      (t/is (= "c" (col/-get-obj c 3))))))
