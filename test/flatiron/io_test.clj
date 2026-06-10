(ns flatiron.io-test
  "Specification tests for CSV I/O — read-csv and write-csv."
  (:require [flatiron.io :as io]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; read-csv — basic parsing with type inference
;; ════════════════════════════════════════════════════════════════════════

(t/deftest read-csv-header-and-types
  (let [csv "Symbol,Qty,Price\nAAPL,100,150.5\nGOOG,200,140.25\n"
        table (io/read-csv csv)]
    (t/is (= 2 (tbl/nrows table)))
    (t/is (= 3 (tbl/ncols table)))
    ;; Column names from header
    (t/is (= :Symbol (tbl/col-name table 0)))
    (t/is (= :Qty (tbl/col-name table 1)))
    (t/is (= :Price (tbl/col-name table 2)))
    ;; Symbol is str, Qty is i64, Price is f64
    (t/is (= :str (col/-type-tag (tbl/col table :Symbol))))
    (t/is (= :i64 (col/-type-tag (tbl/col table :Qty))))
    (t/is (= :f64 (col/-type-tag (tbl/col table :Price))))
    ;; Values
    (t/is (= "AAPL" (col/-get-obj (tbl/col table :Symbol) 0)))
    (t/is (= 100 (col/-get-long (tbl/col table :Qty) 0)))
    (t/is (= 150.5 (col/-get-double (tbl/col table :Price) 0)))))

(t/deftest read-csv-no-header
  (let [csv "100,200\n300,400\n"
        table (io/read-csv csv {:header? false})]
    (t/is (= 2 (tbl/nrows table)))
    (t/is (= 2 (tbl/ncols table)))
    ;; Default column names: col0, col1
    (t/is (= :col0 (tbl/col-name table 0)))
    (t/is (= :col1 (tbl/col-name table 1)))))

(t/deftest read-csv-empty
  (let [csv "A,B\n"
        table (io/read-csv csv)]
    (t/is (= 0 (tbl/nrows table)))
    (t/is (= 2 (tbl/ncols table)))))

(t/deftest read-csv-with-nulls
  (let [csv "Name,Score\nAlice,100\nBob,\n"
        table (io/read-csv csv)]
    (t/is (true? (col/-has-nulls? (tbl/col table :Score))))
    (t/is (= 100 (col/-get-long (tbl/col table :Score) 0)))
    (t/is (nil? (col/-get-obj (tbl/col table :Score) 1)))))

(t/deftest read-csv-booleans
  (let [csv "Flag\nTRUE\nFALSE\ntrue\nfalse\n"
        table (io/read-csv csv)]
    (t/is (= :bool (col/-type-tag (tbl/col table :Flag))))
    (t/is (true? (col/-get-obj (tbl/col table :Flag) 0)))
    (t/is (false? (col/-get-obj (tbl/col table :Flag) 1)))
    (t/is (true? (col/-get-obj (tbl/col table :Flag) 2)))))

(t/deftest read-csv-type-promotion
  "Mixed int/float column should promote to f64."
  (let [csv "Val\n100\n3.14\n200\n"
        table (io/read-csv csv)]
    (t/is (= :f64 (col/-type-tag (tbl/col table :Val))))))

;; ════════════════════════════════════════════════════════════════════════
;; write-csv
;; ════════════════════════════════════════════════════════════════════════

(t/deftest write-csv-round-trip
  (let [sym-c (col/sym-column [:a :b :a])
        qty-c (col/i64-column [10 20 30])
        table (tbl/table [:sym :qty] [sym-c qty-c])
        csv (io/write-csv table)
        ;; Parse back
        table2 (io/read-csv csv)]
    (t/is (= 3 (tbl/nrows table2)))
    (t/is (= 2 (tbl/ncols table2)))
    (t/is (= :sym (tbl/col-name table2 0)))
    (t/is (= :qty (tbl/col-name table2 1)))
    (t/is (= "a" (col/-get-obj (tbl/col table2 :sym) 0)))
    (t/is (= :str (col/-type-tag (tbl/col table2 :sym))))  ;; CSV doesn't preserve sym type
    (t/is (= 10 (col/-get-long (tbl/col table2 :qty) 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; CSV inference for special float values
;; ════════════════════════════════════════════════════════════════════════

(t/deftest csv-infers-and-parses-inf-and-nan
  (let [table (io/read-csv "x\nInf\n-Inf\n1.5\nnan\n")
        c     (tbl/col table :x)]
    (t/is (= :f64 (col/-type-tag c)))
    (t/is (Double/isInfinite (col/-get-double c 0)))
    (t/is (pos? (col/-get-double c 0)))
    (t/is (Double/isInfinite (col/-get-double c 1)))
    (t/is (neg? (col/-get-double c 1)))
    (t/is (= 1.5 (col/-get-double c 2)))
    (t/is (Double/isNaN (col/-get-double c 3)))))
