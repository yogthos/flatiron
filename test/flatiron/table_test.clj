(ns flatiron.table-test
  "Specification tests for Table — correctness contract."
  (:require [flatiron.table :as tbl]
            [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; Construction
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-construction
  (let [sym-c (col/sym-column [:US :CA :MX])
        qty-c (col/i64-column [100 200 150])
        t     (tbl/table [:country :qty] [sym-c qty-c])]
    (t/is (= 2 (tbl/ncols t)))
    (t/is (= 3 (tbl/nrows t)))
    (t/is (= :country (tbl/col-name t 0)))
    (t/is (= :qty     (tbl/col-name t 1)))))

(t/deftest table-empty
  (let [t (tbl/table [] [])]
    (t/is (= 0 (tbl/ncols t)))
    (t/is (= 0 (tbl/nrows t)))))

;; ════════════════════════════════════════════════════════════════════════
;; Column lookup by keyword
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-col-by-keyword
  (let [sym-c (col/sym-column [:a :b :c])
        f64-c (col/f64-column [1.0 2.0 3.0])
        t     (tbl/table [:sym :val] [sym-c f64-c])]
    ;; col-idx
    (t/is (= 0  (tbl/col-idx t :sym)))
    (t/is (= 1  (tbl/col-idx t :val)))
    (t/is (= -1 (tbl/col-idx t :missing)))
    ;; col returns correct column
    (let [c (tbl/col t :sym)]
      (t/is (some? c))
      (t/is (= :sym (col/-type-tag c)))
      (t/is (= :a   (col/-get-obj c 0))))
    (let [c (tbl/col t :val)]
      (t/is (some? c))
      (t/is (= :f64 (col/-type-tag c))))
    ;; Missing column returns nil
    (t/is (nil? (tbl/col t :nope)))))

;; ════════════════════════════════════════════════════════════════════════
;; Column lookup by index
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-col-by-index
  (let [t (tbl/table [:x :y] [(col/i64-column [1 2])
                              (col/f64-column [1.0 2.0])])]
    (t/is (= :i64 (col/-type-tag (tbl/col-by-idx t 0))))
    (t/is (= :f64 (col/-type-tag (tbl/col-by-idx t 1))))
    ;; Out of range throws IndexOutOfBoundsException (native array access)
    (t/is (thrown? ArrayIndexOutOfBoundsException (tbl/col-by-idx t 2)))
    (t/is (thrown? ArrayIndexOutOfBoundsException (tbl/col-by-idx t -1)))))

(t/deftest table-col-name-out-of-range
  (let [t (tbl/table [:a] [(col/i64-column [1])])]
    (t/is (thrown? ArrayIndexOutOfBoundsException (tbl/col-name t 1)))
    (t/is (thrown? ArrayIndexOutOfBoundsException (tbl/col-name t -1)))))

;; ════════════════════════════════════════════════════════════════════════
;; Multiple column types (matches C test_table_multiple_cols)
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-multiple-column-types
  (let [i64-c  (col/i64-column [1 2 3])
        f64-c  (col/f64-column [1.1 2.2 3.3])
        bool-c (col/bool-column [true false true])
        t      (tbl/table [:a :b :c] [i64-c f64-c bool-c])]
    (t/is (= 3 (tbl/ncols t)))
    (t/is (= 3 (tbl/nrows t)))

    ;; Verify by keyword
    (t/is (= :i64  (col/-type-tag (tbl/col t :a))))
    (t/is (= :f64  (col/-type-tag (tbl/col t :b))))
    (t/is (= :bool (col/-type-tag (tbl/col t :c))))

    ;; Verify by index
    (t/is (= :i64  (col/-type-tag (tbl/col-by-idx t 0))))
    (t/is (= :f64  (col/-type-tag (tbl/col-by-idx t 1))))
    (t/is (= :bool (col/-type-tag (tbl/col-by-idx t 2))))

    ;; Verify column names
    (t/is (= :a (tbl/col-name t 0)))
    (t/is (= :b (tbl/col-name t 1)))
    (t/is (= :c (tbl/col-name t 2)))

    ;; Verify data integrity
    (t/is (= 1   (col/-get-long (tbl/col t :a) 0)))
    (t/is (= 3   (col/-get-long (tbl/col t :a) 2)))
    (t/is (= 2.2 (col/-get-double (tbl/col t :b) 1)))
    (t/is (true?  (col/-get-obj (tbl/col t :c) 0)))
    (t/is (false? (col/-get-obj (tbl/col t :c) 1)))))

;; ════════════════════════════════════════════════════════════════════════
;; Schema order preservation
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-schema-order
  "Column names are stored in insertion order."
  (let [t (tbl/table [:z :a :m] [(col/i64-column [10])
                                  (col/i64-column [20])
                                  (col/i64-column [30])])]
    (t/is (= :z (tbl/col-name t 0)))
    (t/is (= :a (tbl/col-name t 1)))
    (t/is (= :m (tbl/col-name t 2)))
    ;; Lookup still works regardless of order
    (t/is (= 0 (tbl/col-idx t :z)))
    (t/is (= 1 (tbl/col-idx t :a)))
    (t/is (= 2 (tbl/col-idx t :m)))))

;; ════════════════════════════════════════════════════════════════════════
;; Sym and Str columns in tables
;; ════════════════════════════════════════════════════════════════════════

(t/deftest table-with-sym-and-str
  (let [sym-c (col/sym-column [:buy :sell :buy])
        str-c (col/str-column ["AAPL" "GOOG" "MSFT"])
        t     (tbl/table [:side :ticker] [sym-c str-c])]
    (t/is (= 3 (tbl/nrows t)))
    (t/is (= :buy   (col/-get-obj (tbl/col t :side) 0)))
    (t/is (= "AAPL" (col/-get-obj (tbl/col t :ticker) 0)))
    (t/is (= "MSFT" (col/-get-obj (tbl/col t :ticker) 2)))))
