(ns flatiron.types-test
  "Logical (custom) column types: build, decode-on-read, and carry-through of
   the logical tag across sort, filter, group-by, and the binary store."
  (:require [clojure.test :refer [deftest testing is]]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.sort :as sort]
            [flatiron.filter :as filt]
            [flatiron.group :as group]
            [flatiron.parallel :as par]
            [flatiron.window :as win]
            [flatiron.store :as store]
            [flatiron.types :as types])
  (:import [java.time LocalDate Instant]
           [java.io File]))

(def d1 (LocalDate/of 2020 1 1))
(def d2 (LocalDate/of 2020 6 15))
(def d3 (LocalDate/of 2021 3 10))

(deftest build-and-read
  (testing "a date column is physically :i64 but logically :date"
    (let [c (col/date-column [d1 d2 d3])]
      (is (= :i64 (col/-type-tag c)))
      (is (= :date (col/-logical-tag c)))
      (testing "values read back as LocalDate"
        (is (= d1 (col/-get-obj c 0)))
        (is (= d3 (col/-get-obj c 2))))
      (testing "the backing array holds raw epoch days"
        (is (= (.toEpochDay d1) (col/-get-long c 0)))
        (is (= (.toEpochDay d3) (col/-get-long c 2))))))
  (testing "nil values round-trip as nil"
    (let [c (col/date-column [d1 nil d3])]
      (is (col/-has-nulls? c))
      (is (nil? (col/-get-obj c 1)))
      (is (= d1 (col/-get-obj c 0))))))

(deftest sort-carries-logical
  (testing "sorting on a date column orders chronologically and stays a date"
    (let [t      (tbl/table [:d :n] [(col/date-column [d3 d1 d2])
                                     (col/i64-column [3 1 2])])
          sorted (sort/sort-table t [[:d :asc]])
          dc     (tbl/col sorted :d)]
      (is (= :date (col/-logical-tag dc)))
      (is (= [d1 d2 d3] (mapv #(col/-get-obj dc %) (range 3))))
      (is (= [1 2 3] (mapv #(col/-get-long (tbl/col sorted :n) %) (range 3)))))))

(deftest filter-encodes-literal
  (testing "range and equality predicates encode the domain literal once"
    (let [c (col/date-column [d1 d2 d3])
          t (tbl/table [:d] [c])]
      (testing "> a date keeps later rows"
        (let [mask (filt/scalar-pred c :gt d2)
              out  (tbl/col (filt/filter-rows t mask) :d)]
          (is (= [d3] (mapv #(col/-get-obj out %) (range (col/-len out)))))))
      (testing "= a date keeps the matching row and preserves the logical type"
        (let [mask (filt/scalar-pred c :eq d1)
              out  (tbl/col (filt/filter-rows t mask) :d)]
          (is (= :date (col/-logical-tag out)))
          (is (= [d1] (mapv #(col/-get-obj out %) (range (col/-len out))))))))))

(deftest group-by-date
  (testing "group-by keyed on a date decodes keys; min/max stay dates, count is long"
    (let [t   (tbl/table [:d :q] [(col/date-column [d1 d1 d2])
                                  (col/i64-column [10 5 7])])
          res (group/group-by t
                              :keys [:d]
                              :aggs [{:agg :min :col :d :out :first}
                                     {:agg :max :col :d :out :last}
                                     {:agg :count :col :q :out :n}])
          key-col (tbl/col res :d)
          min-col (tbl/col res :first)
          n-col   (tbl/col res :n)
          rows    (sort-by first
                           (map (fn [i] [(col/-get-obj key-col i)
                                         (col/-get-obj min-col i)
                                         (col/-get-long n-col i)])
                                (range (col/-len key-col))))]
      (is (= :date (col/-logical-tag key-col)))
      (is (= :date (col/-logical-tag min-col)))
      (is (nil? (col/-logical-tag n-col)))
      (is (= [[d1 d1 2] [d2 d2 1]] rows)))))

(deftest store-round-trip
  (testing "save/load preserves logical types, including nulls"
    (let [dir (str (File. (System/getProperty "java.io.tmpdir")
                          (str "flatiron-types-test-" (System/nanoTime))))
          inst (Instant/ofEpochMilli 1700000000000)
          t    (tbl/table [:d :ts :n]
                          [(col/date-column [d1 nil d3])
                           (col/instant-column [inst inst nil])
                           (col/i64-column [1 2 3])])]
      (try
        (store/save-table t dir)
        (let [loaded (store/load-table dir)
              dc (tbl/col loaded :d)
              tc (tbl/col loaded :ts)
              nc (tbl/col loaded :n)]
          (is (= :date (col/-logical-tag dc)))
          (is (= :instant (col/-logical-tag tc)))
          (is (nil? (col/-logical-tag nc)) "plain column has no logical tag")
          (is (= [d1 nil d3] (mapv #(col/-get-obj dc %) (range 3))))
          (is (= [inst inst nil] (mapv #(col/-get-obj tc %) (range 3))))
          (is (= [1 2 3] (mapv #(col/-get-long nc %) (range 3)))))
        (finally
          (doseq [^File f (reverse (file-seq (File. dir)))] (.delete f)))))))

(deftest f64-backed-decode-keeps-fraction
  (testing "an :f64-backed codec round-trips fractional values without truncation"
    ;; regression: F64Column -get-obj used to (long v) before decoding.
    (types/register-type! ::scaled
      {:physical :f64
       :class    Double
       :encode   (fn ^double [x] (double x))
       :decode   (fn [^double v] v)})
    (let [c (col/typed-column ::scaled [1.5 2.25 nil])]
      (is (= :f64 (col/-type-tag c)))
      (is (= [1.5 2.25 nil] (mapv #(col/-get-obj c %) (range 3)))))))

(deftest lag-lead-encode-domain-default
  (testing "lag/lead accept a domain-object default on a logically-typed column"
    ;; regression: the default used to be cast with (to-long default) and threw.
    (let [c    (col/date-column [d1 d2 d3])
          dflt (LocalDate/of 1999 12 31)
          lagd (win/lag c 1 dflt)
          lead (win/lead c 1 dflt)]
      (is (= [dflt d1 d2] (mapv #(col/-get-obj lagd %) (range 3))))
      (is (= [d2 d3 dflt] (mapv #(col/-get-obj lead %) (range 3))))
      (testing "a nil default still reads back as nil"
        (is (nil? (col/-get-obj (win/lag c 1 nil) 0)))))))

(deftest parallel-min-max-all-null-group
  (testing "parallel-group-by min/max on an all-null group matches sequential (nil, no crash)"
    ;; regression: par-group-agg left empty groups at the fill sentinel, which
    ;; a logical tag then tried to decode as a date and crashed.
    (let [n  4000
          ks (vec (for [i (range n)] (if (even? i) :a :b)))
          ds (vec (for [i (range n)] (when (even? i) (LocalDate/ofEpochDay (mod i 500)))))
          t  (tbl/table [:k :d] [(col/sym-column ks) (col/date-column ds)])
          rows (fn [r out]
                 (into {} (map (fn [i] [(col/-get-obj (tbl/col r :k) i)
                                        (col/-get-obj (tbl/col r out) i)])
                               (range (col/-len (tbl/col r :k))))))
          seqr (group/group-by t :keys [:k] :aggs [{:agg :min :col :d :out :m}
                                                   {:agg :max :col :d :out :x}])
          parr (par/parallel-group-by t :keys [:k] :aggs [{:agg :min :col :d :out :m}
                                                          {:agg :max :col :d :out :x}])]
      (is (nil? (get (rows parr :m) :b)) "all-null group min is nil, not a sentinel")
      (is (nil? (get (rows parr :x) :b)))
      (is (= (rows seqr :m) (rows parr :m)) "parallel min matches sequential")
      (is (= (rows seqr :x) (rows parr :x)) "parallel max matches sequential"))))

(deftest custom-registered-type
  (testing "a user-registered codec round-trips through a column"
    ;; A trivial cents->BigDecimal money type backed by i64.
    (types/register-type! ::cents
      {:physical :i64
       :class    java.math.BigDecimal
       :encode   (fn ^long [^java.math.BigDecimal d]
                   (.longValueExact (.movePointRight d 2)))
       :decode   (fn [^long v] (.movePointLeft (java.math.BigDecimal/valueOf v) 2))})
    (let [c (col/typed-column ::cents [(java.math.BigDecimal. "1.50")
                                       (java.math.BigDecimal. "2.25")])]
      (is (= :i64 (col/-type-tag c)))
      (is (= 150 (col/-get-long c 0)))
      (is (= 0 (.compareTo (java.math.BigDecimal. "1.50") (col/-get-obj c 0))))
      (is (= 0 (.compareTo (java.math.BigDecimal. "2.25") (col/-get-obj c 1)))))))
