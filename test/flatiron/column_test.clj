(ns flatiron.column-test
  "Specification tests for column types — correctness contract."
  (:require [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; I64Column
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-construction
  (let [c (col/i64-column [1 2 3 5 8 13])]
    (t/is (= 6 (col/-len c)))
    (t/is (= :i64 (col/-type-tag c)))
    (t/is (false? (col/-has-nulls? c)))
    ;; Access via all three paths
    (t/is (= 1    (col/-get-long c 0)))
    (t/is (= 13   (col/-get-long c 5)))
    (t/is (= 3.0  (col/-get-double c 2)))
    (t/is (= 5    (col/-get-obj c 3)))))

(t/deftest i64-null-sentinel
  "Null sentinel is Long/MIN_VALUE — matches C NULL_I64 convention."
  (let [c (col/i64-column [1 nil 3 nil 5])]
    (t/is (= 5 (col/-len c)))
    (t/is (true? (col/-has-nulls? c)))
    (t/is (= 1 (col/-get-long c 0)))
    ;; Null rows return the sentinel via get-long, nil via get-obj
    (t/is (= col/NULL_I64 (col/-get-long c 1)))
    (t/is (nil? (col/-get-obj c 1)))
    (t/is (= 5 (col/-get-obj c 4)))
    ;; Non-null rows return boxed Long via get-obj
    (t/is (= 3 (col/-get-obj c 2)))))

(t/deftest i64-slice-zero-copy
  "Slice shares the backing array — zero copy like C ray_vec_slice."
  (let [c (col/i64-column [10 20 30 40 50])
        s (col/-slice c 1 3)]
    (t/is (= 3    (col/-len s)))
    (t/is (= :i64 (col/-type-tag s)))
    (t/is (= 20   (col/-get-long s 0)))
    (t/is (= 30   (col/-get-long s 1)))
    (t/is (= 40   (col/-get-long s 2)))))

(t/deftest i64-slice-of-slice
  "Nested slices should work — each adds its offset."
  (let [c  (col/i64-column [0 10 20 30 40 50 60])
        s1 (col/-slice c 2 4)    ;; [20 30 40 50]
        s2 (col/-slice s1 1 2)]  ;; [30 40]
    (t/is (= 2  (col/-len s2)))
    (t/is (= 30 (col/-get-long s2 0)))
    (t/is (= 40 (col/-get-long s2 1)))))

(t/deftest i64-slice-preserves-nulls
  "Null sentinel propagates through slice."
  (let [c (col/i64-column [10 nil 30 nil 50])
        s (col/-slice c 1 3)]
    (t/is (true? (col/-has-nulls? s)))
    (t/is (nil? (col/-get-obj s 0)))
    (t/is (= 30  (col/-get-obj s 1)))
    (t/is (nil? (col/-get-obj s 2)))))

(t/deftest i64-empty-column
  (let [c (col/i64-column [])]
    (t/is (= 0 (col/-len c)))
    (t/is (false? (col/-has-nulls? c)))))

(t/deftest i64-from-array
  "Raw long array constructor — useful for zero-copy from foreign sources."
  (let [arr (long-array [100 200 300])
        c   (col/i64-column-from-array arr)]
    (t/is (= 3   (col/-len c)))
    (t/is (= 100 (col/-get-long c 0)))
    (t/is (= 300 (col/-get-long c 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64Column
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-construction
  (let [c (col/f64-column [1.0 3.14 2.718])]
    (t/is (= 3    (col/-len c)))
    (t/is (= :f64 (col/-type-tag c)))
    (t/is (false? (col/-has-nulls? c)))
    (t/is (= 3.14  (col/-get-double c 1)))
    (t/is (= 2.718 (col/-get-obj c 2)))))

(t/deftest f64-null-nan-sentinel
  "NaN sentinel — nulls are detected via Double/isNaN (NaN != NaN)."
  (let [c (col/f64-column [1.0 nil 3.0])]
    (t/is (true? (col/-has-nulls? c)))
    ;; NaN sentinel self-inequality: NaN != NaN
    (t/is (Double/isNaN (col/-get-double c 1)))
    (t/is (not (= (col/-get-double c 1) (col/-get-double c 1))))
    (t/is (nil? (col/-get-obj c 1)))
    ;; Non-null rows still correct
    (t/is (= 1.0 (col/-get-double c 0)))
    (t/is (= 3.0 (col/-get-double c 2)))))

(t/deftest f64-slice
  (let [c (col/f64-column [0.5 1.0 1.5 2.0])
        s (col/-slice c 1 2)]
    (t/is (= 2   (col/-len s)))
    (t/is (= 1.0 (col/-get-double s 0)))
    (t/is (= 1.5 (col/-get-double s 1)))))

(t/deftest f64-slice-with-nulls
  (let [c (col/f64-column [1.0 nil 3.0 4.0])
        s (col/-slice c 0 3)]
    (t/is (true? (col/-has-nulls? s)))
    (t/is (nil? (col/-get-obj s 1)))
    (t/is (= 3.0 (col/-get-obj s 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; BoolColumn
;; ════════════════════════════════════════════════════════════════════════

(t/deftest bool-construction
  (let [c (col/bool-column [true false true true])]
    (t/is (= 4     (col/-len c)))
    (t/is (= :bool (col/-type-tag c)))
    (t/is (false? (col/-has-nulls? c)))
    ;; get-long: 1/0
    (t/is (= 1 (col/-get-long c 0)))
    (t/is (= 0 (col/-get-long c 1)))
    ;; get-double: 1.0/0.0
    (t/is (= 1.0 (col/-get-double c 0)))
    (t/is (= 0.0 (col/-get-double c 1)))
    ;; get-obj: Boolean true/false
    (t/is (true?  (col/-get-obj c 0)))
    (t/is (false? (col/-get-obj c 1)))))

(t/deftest bool-slice
  (let [c (col/bool-column [true false true false true])
        s (col/-slice c 2 3)]
    (t/is (= 3 (col/-len s)))
    (t/is (true?  (col/-get-obj s 0)))
    (t/is (false? (col/-get-obj s 1)))
    (t/is (true?  (col/-get-obj s 2)))))

(t/deftest bool-empty
  (let [c (col/bool-column [])]
    (t/is (= 0 (col/-len c)))))

;; ════════════════════════════════════════════════════════════════════════
;; SymColumn
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sym-construction
  (let [c (col/sym-column [:a :b :c])]
    (t/is (= 3    (col/-len c)))
    (t/is (= :sym (col/-type-tag c)))
    (t/is (false? (col/-has-nulls? c)))
    (t/is (= :a (col/-get-obj c 0)))
    (t/is (= :c (col/-get-obj c 2)))))

(t/deftest sym-with-nulls
  (let [c (col/sym-column [:a nil :b])]
    (t/is (true? (col/-has-nulls? c)))
    (t/is (= :a (col/-get-obj c 0)))
    (t/is (nil? (col/-get-obj c 1)))
    (t/is (= :b (col/-get-obj c 2)))))

(t/deftest sym-throws-on-numeric-access
  "SymColumn has no numeric representation — get-long/get-double must throw."
  (let [c (col/sym-column [:x])]
    (t/is (thrown? UnsupportedOperationException (col/-get-long c 0)))
    (t/is (thrown? UnsupportedOperationException (col/-get-double c 0)))))

(t/deftest sym-slice
  (let [c (col/sym-column [:x :y :z :w])
        s (col/-slice c 1 2)]
    (t/is (= 2 (col/-len s)))
    (t/is (= :y (col/-get-obj s 0)))
    (t/is (= :z (col/-get-obj s 1)))))

(t/deftest sym-slice-nulls
  (let [c (col/sym-column [:a nil :c])
        s (col/-slice c 0 2)]
    (t/is (true? (col/-has-nulls? s)))
    (t/is (nil? (col/-get-obj s 1)))))

;; ════════════════════════════════════════════════════════════════════════
;; StrColumn
;; ════════════════════════════════════════════════════════════════════════

(t/deftest str-construction
  (let [c (col/str-column ["hello" "world" "!"])]
    (t/is (= 3    (col/-len c)))
    (t/is (= :str (col/-type-tag c)))
    (t/is (false? (col/-has-nulls? c)))
    (t/is (= "hello" (col/-get-obj c 0)))
    (t/is (= "!"     (col/-get-obj c 2)))))

(t/deftest str-with-nulls
  (let [c (col/str-column ["a" nil "b"])]
    (t/is (true? (col/-has-nulls? c)))
    (t/is (= "a" (col/-get-obj c 0)))
    (t/is (nil? (col/-get-obj c 1)))
    (t/is (= "b" (col/-get-obj c 2)))))

(t/deftest str-throws-on-numeric-access
  (let [c (col/str-column ["x"])]
    (t/is (thrown? UnsupportedOperationException (col/-get-long c 0)))
    (t/is (thrown? UnsupportedOperationException (col/-get-double c 0)))))

(t/deftest str-slice
  (let [c (col/str-column ["foo" "bar" "baz"])
        s (col/-slice c 1 2)]
    (t/is (= 2 (col/-len s)))
    (t/is (= "bar" (col/-get-obj s 0)))
    (t/is (= "baz" (col/-get-obj s 1)))))
