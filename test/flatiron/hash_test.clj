(ns flatiron.hash-test
  "Specification tests for hash functions."
  (:require [flatiron.hash :as h]
            [clojure.test :as t]))

(t/deftest hash-i64-basic
  "hash-i64 should be deterministic and produce non-zero for non-zero input."
  (t/is (not= 0 (h/hash-i64 0)))
  (t/is (not= 0 (h/hash-i64 42)))
  (t/is (not= 0 (h/hash-i64 Long/MAX_VALUE)))
  (t/is (not= 0 (h/hash-i64 Long/MIN_VALUE)))
  ;; Same input → same hash
  (t/is (= (h/hash-i64 12345) (h/hash-i64 12345))))

(t/deftest hash-i64-distribution
  "Different inputs should produce different hashes."
  (t/is (not= (h/hash-i64 0) (h/hash-i64 1)))
  (t/is (not= (h/hash-i64 100) (h/hash-i64 200))))

(t/deftest hash-f64-basic
  "hash-f64 should be deterministic and handle special values."
  (t/is (not= 0 (h/hash-f64 0.0)))
  (t/is (not= 0 (h/hash-f64 1.0)))
  (t/is (not= 0 (h/hash-f64 3.14159)))
  ;; Same input → same hash
  (t/is (= (h/hash-f64 2.5) (h/hash-f64 2.5))))

(t/deftest hash-f64-neg-zero
  "Negative zero should hash same as positive zero."
  (t/is (= (h/hash-f64 0.0) (h/hash-f64 -0.0))))

(t/deftest hash-combine-order-dependent
  "hash-combine(a, b) != hash-combine(b, a)"
  (t/is (not= (h/hash-combine (h/hash-i64 1) (h/hash-i64 2))
              (h/hash-combine (h/hash-i64 2) (h/hash-i64 1)))))

(t/deftest hash-combine-deterministic
  "Same inputs → same combined hash."
  (let [a (h/hash-i64 10)
        b (h/hash-i64 20)]
    (t/is (= (h/hash-combine a b) (h/hash-combine a b)))))

(t/deftest next-power-of-two
  (t/is (= 1   (h/next-power-of-two 0)))
  (t/is (= 1   (h/next-power-of-two 1)))
  (t/is (= 2   (h/next-power-of-two 2)))
  (t/is (= 4   (h/next-power-of-two 3)))
  (t/is (= 4   (h/next-power-of-two 4)))
  (t/is (= 8   (h/next-power-of-two 5)))
  (t/is (= 16  (h/next-power-of-two 13)))
  (t/is (= 1024 (h/next-power-of-two 1000)))
  (t/is (= 65536 (h/next-power-of-two 50000))))

(t/deftest ht-capacity
  "Capacity should be power of two >= 2*n, minimum 64."
  (t/is (= 64 (h/ht-capacity 1)))
  (t/is (= 64 (h/ht-capacity 10)))
  (t/is (= 64 (h/ht-capacity 32)))
  (t/is (= 128 (h/ht-capacity 64)))
  (t/is (= 256 (h/ht-capacity 100)))
  (t/is (= 2048 (h/ht-capacity 1000))))

(t/deftest wymum-basic
  "128-bit multiply: verify lo and hi for known values."
  (let [ab (h/wymum 1 2)
        lo (aget ab 0)
        hi (aget ab 1)]
    (t/is (= 2 lo))
    (t/is (= 0 hi)))
  (let [ab (h/wymum -1 2) ;; 0xFFFFFFFFFFFFFFFF = -1 as signed long
        lo (aget ab 0)
        hi (aget ab 1)]
    ;; unsigned 2^64-1 * 2 = 2^65 - 2 → lo=2^64-2 = -2, hi=1
    (t/is (= -2 lo))
    (t/is (= 1 hi))))

(t/deftest wymix-basic
  "wymix should be deterministic."
  (t/is (= (h/wymix 1 2) (h/wymix 1 2)))
  (t/is (not= (h/wymix 1 2) (h/wymix 3 4))))
