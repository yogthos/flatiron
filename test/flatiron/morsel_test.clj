(ns flatiron.morsel-test
  "Specification tests for morsel iteration."
  (:require [flatiron.morsel :as m]
            [flatiron.column :as col]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; I64 morsel iteration
;; ════════════════════════════════════════════════════════════════════════

(t/deftest i64-morsel-single-chunk
  "Column smaller than morsel size → one chunk returns all elements."
  (let [col  (col/i64-column [1 3 5 7 11])
        ms   (m/i64-morsel-source col)
        buf  (long-array m/MORSEL-SIZE)]
    (t/is (= 5 (m/morsel-count ms)))
    (let [n (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
      (t/is (= 5 n))
      (t/is (= 1  (aget buf 0)))
      (t/is (= 11 (aget buf 4)))
      ;; No more elements
      (t/is (= 0 (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE))))))

(t/deftest i64-morsel-multi-chunk
  "Column larger than morsel size → multiple chunks."
  (let [n-rows (* 3 m/MORSEL-SIZE)
        col    (col/i64-column (mapv long (range n-rows)))
        ms     (m/i64-morsel-source col)
        buf    (long-array m/MORSEL-SIZE)
        chunks (loop [cs []]
                 (let [n (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)]
                   (if (zero? n)
                     cs
                     (let [slice (long-array n)]
                       (System/arraycopy buf 0 slice 0 n)
                       (recur (conj cs slice))))))]
    (t/is (= 3 (count chunks)))
    (t/is (= m/MORSEL-SIZE (alength ^longs (first chunks))))
    ;; First chunk: 0..1023
    (t/is (= 0    (aget ^longs (first chunks) 0)))
    (t/is (= 1023 (aget ^longs (first chunks) (dec m/MORSEL-SIZE))))
    ;; Second chunk: 1024..2047
    (t/is (= 1024 (aget ^longs (second chunks) 0)))
    ;; Third chunk: 2048..3071
    (t/is (= 2048 (aget ^longs (nth chunks 2) 0)))))

(t/deftest i64-morsel-partial-last-chunk
  "Last chunk shorter than MORSEL-SIZE."
  (let [n-rows (+ m/MORSEL-SIZE 7)
        col    (col/i64-column (mapv long (range n-rows)))
        ms     (m/i64-morsel-source col)
        buf    (long-array m/MORSEL-SIZE)]
    ;; First chunk: full
    (t/is (= m/MORSEL-SIZE (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)))
    ;; Second chunk: partial (7 elements)
    (t/is (= 7 (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= m/MORSEL-SIZE (aget buf 0)))
    ;; Done
    (t/is (= 0 (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)))))

(t/deftest i64-morsel-reset
  "morsel-reset! restarts from the beginning."
  (let [col (col/i64-column [10 20 30])
        ms  (m/i64-morsel-source col)
        buf (long-array 3)]
    (t/is (= 3 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 0 (m/morsel-next-i64! ms buf 0 3)))  ;; exhausted
    (m/morsel-reset! ms)
    (t/is (= 3 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 10 (aget buf 0)))))

(t/deftest i64-morsel-sliced-column
  "Morsel on a sliced column respects the slice offset."
  (let [col (col/i64-column [0 10 20 30 40 50])
        sliced (col/-slice col 2 3)   ;; [20 30 40]
        ms  (m/i64-morsel-source sliced)
        buf (long-array 3)]
    (t/is (= 3 (m/morsel-count ms)))
    (t/is (= 3 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 20 (aget buf 0)))
    (t/is (= 30 (aget buf 1)))
    (t/is (= 40 (aget buf 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; F64 morsel iteration
;; ════════════════════════════════════════════════════════════════════════

(t/deftest f64-morsel-basic
  (let [col (col/f64-column [1.5 2.5 3.5])
        ms  (m/f64-morsel-source col)
        buf (double-array m/MORSEL-SIZE)]
    (t/is (= 3 (m/morsel-count ms)))
    (t/is (= 3 (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= 1.5 (aget buf 0)))
    (t/is (= 3.5 (aget buf 2)))
    (t/is (= 0   (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)))))

(t/deftest f64-morsel-multi-chunk
  (let [n-rows (* 2 m/MORSEL-SIZE)
        col    (col/f64-column (mapv double (range n-rows)))
        ms     (m/f64-morsel-source col)
        buf    (double-array m/MORSEL-SIZE)]
    (t/is (= m/MORSEL-SIZE (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= m/MORSEL-SIZE (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= 0 (m/morsel-next-f64! ms buf 0 m/MORSEL-SIZE)))))

(t/deftest f64-morsel-reset
  (let [col (col/f64-column [1.0 2.0])
        ms  (m/f64-morsel-source col)
        buf (double-array 2)]
    (m/morsel-next-f64! ms buf 0 2)
    (t/is (= 0 (m/morsel-next-f64! ms buf 0 2)))
    (m/morsel-reset! ms)
    (t/is (= 2 (m/morsel-next-f64! ms buf 0 2)))
    (t/is (= 1.0 (aget buf 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; Bool morsel iteration
;; ════════════════════════════════════════════════════════════════════════

(t/deftest bool-morsel-basic
  (let [col (col/bool-column [true false true])
        ms  (m/bool-morsel-source col)
        buf (byte-array 3)]
    (t/is (= 3 (m/morsel-count ms)))
    (t/is (= 3 (m/morsel-next-bool! ms buf 0 3)))
    (t/is (= 1 (aget buf 0)))
    (t/is (= 0 (aget buf 1)))
    (t/is (= 1 (aget buf 2)))
    (t/is (= 0 (m/morsel-next-bool! ms buf 0 3)))))

(t/deftest bool-morsel-multi-chunk
  (let [n-rows (* 2 m/MORSEL-SIZE)
        col    (col/bool-column (repeat n-rows true))
        ms     (m/bool-morsel-source col)
        buf    (byte-array m/MORSEL-SIZE)]
    (t/is (= m/MORSEL-SIZE (m/morsel-next-bool! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= m/MORSEL-SIZE (m/morsel-next-bool! ms buf 0 m/MORSEL-SIZE)))
    (t/is (= 0 (m/morsel-next-bool! ms buf 0 m/MORSEL-SIZE)))))

;; ════════════════════════════════════════════════════════════════════════
;; Sym/Str object morsel iteration
;; ════════════════════════════════════════════════════════════════════════

(t/deftest sym-morsel-basic
  (let [col (col/sym-column [:a :b :c])
        ms  (m/obj-morsel-source col)
        buf (object-array 3)]
    (t/is (= 3 (m/morsel-count ms)))
    (t/is (= 3 (m/morsel-next-obj! ms buf 0 3)))
    (t/is (= :a (aget buf 0)))
    (t/is (= :c (aget buf 2)))))

(t/deftest str-morsel-basic
  (let [col (col/str-column ["hello" "world"])
        ms  (m/obj-morsel-source col)
        buf (object-array 2)]
    (t/is (= 2 (m/morsel-count ms)))
    (t/is (= 2 (m/morsel-next-obj! ms buf 0 2)))
    (t/is (= "hello" (aget buf 0)))
    (t/is (= "world" (aget buf 1)))))

;; ════════════════════════════════════════════════════════════════════════
;; Edge cases
;; ════════════════════════════════════════════════════════════════════════

(t/deftest morsel-empty-column
  "Empty column → count=0, next returns 0 immediately."
  (let [col (col/i64-column [])
        ms  (m/i64-morsel-source col)
        buf (long-array m/MORSEL-SIZE)]
    (t/is (= 0 (m/morsel-count ms)))
    (t/is (= 0 (m/morsel-next-i64! ms buf 0 m/MORSEL-SIZE)))))

(t/deftest morsel-buffer-offset
  "Elements are placed at buf-off position, not always position 0."
  (let [col (col/i64-column [10 20 30])
        ms  (m/i64-morsel-source col)
        buf (long-array 10)]
    (t/is (= 3 (m/morsel-next-i64! ms buf 5 5)))
    (t/is (= 10 (aget buf 5)))
    (t/is (= 20 (aget buf 6)))
    (t/is (= 30 (aget buf 7)))))

(t/deftest morsel-max-count-clamp
  "Next never returns more than max-count even if more rows are available."
  (let [col (col/i64-column [1 2 3 4 5 6 7 8])
        ms  (m/i64-morsel-source col)
        buf (long-array m/MORSEL-SIZE)]
    (t/is (= 3 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 3 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 2 (m/morsel-next-i64! ms buf 0 3)))
    (t/is (= 0 (m/morsel-next-i64! ms buf 0 3)))))
