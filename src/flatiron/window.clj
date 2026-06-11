(ns flatiron.window
  "Window functions over sorted columns."
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.types :as types])
  (:import [flatiron.column I64Column F64Column]))

(set! *warn-on-reflection* true)

;; Clojure 1.12.0 workaround: (long x) and (int x) fail in defn bodies
;; when x comes from an external function. Use RT methods directly.
(defn- to-long ^long [x] (clojure.lang.RT/longCast x))
(defn- to-int ^long [x] (clojure.lang.RT/intCast x))

;; ════════════════════════════════════════════════════════════════════════
;; Row number
;; ════════════════════════════════════════════════════════════════════════

(defn row-number [table]
  (let [n (to-int (tbl/nrows table))
        ^longs c (long-array n)]
    (dotimes [i n] (aset c i (to-long (inc i))))
    (I64Column. c n 0 false nil)))

;; ════════════════════════════════════════════════════════════════════════
;; Rank — gaps after ties: 1, 1, 3, 3, 5
;; ════════════════════════════════════════════════════════════════════════

(defn rank [rank-col order-col]
  (let [n    (to-int (col/-len rank-col))
        ^longs out (long-array n)]
    (loop [i 0, current-rank 1, rows-in-group 0]
      (when (< i n)
        (if (and (pos? i)
                 (not= (col/-get-obj rank-col i) (col/-get-obj rank-col (dec i))))
          (let [next-rank (+ current-rank rows-in-group)]
            (aset out i (to-long next-rank))
            (recur (inc i) next-rank 1))
          (do (aset out i (to-long current-rank))
              (recur (inc i) current-rank (inc rows-in-group))))))
    (I64Column. out n 0 false nil)))

;; ════════════════════════════════════════════════════════════════════════
;; Dense rank — no gaps: 1, 1, 2, 2, 3
;; ════════════════════════════════════════════════════════════════════════

(defn dense-rank [rank-col]
  (let [n    (to-int (col/-len rank-col))
        ^longs out (long-array n)]
    (loop [i 0, current-dense 1]
      (when (< i n)
        (if (and (pos? i)
                 (not= (col/-get-obj rank-col i) (col/-get-obj rank-col (dec i))))
          (let [next-dense (inc current-dense)]
            (aset out i (to-long next-dense))
            (recur (inc i) next-dense))
          (do (aset out i (to-long current-dense))
              (recur (inc i) current-dense)))))
    (I64Column. out n 0 false nil)))

;; ════════════════════════════════════════════════════════════════════════
;; Lag — value from previous row
;; ════════════════════════════════════════════════════════════════════════

(defn lag [col offset default]
  (case (col/-type-tag col)
    :i64
    (let [^flatiron.column.I64Column typed col
          n (to-int (.len typed))
          ^longs src (.data typed)
          off (.offset typed)
          ^longs dst (long-array n)
          lg  (.logical typed)
          def (to-long (cond (nil? default)                   col/NULL_I64
                             (and lg (not (number? default))) (types/encode lg default)
                             :else                            default))]
      (dotimes [i n]
        (let [idx (- i offset)]
          (aset dst i (if (< idx 0) def (aget src (+ off idx))))))
      ;; a nil default writes NULL_I64 into the shifted-in slots, so the result
      ;; is nullable even when the source was not — don't copy the source flag.
      (I64Column. dst n 0 (boolean (or (.has-nulls typed) (nil? default))) lg))
    :f64
    (let [^flatiron.column.F64Column typed col
          n (to-int (.len typed))
          ^doubles src (.data typed)
          off (.offset typed)
          dst (double-array n)
          lg  (.logical typed)
          def (double (cond (nil? default)                   Double/NaN
                            (and lg (not (number? default))) (types/encode lg default)
                            :else                            default))]
      (dotimes [i n]
        (let [idx (- i offset)]
          (aset dst i (if (< idx 0) def (aget src (+ off idx))))))
      (F64Column. dst n 0 (boolean (or (.has-nulls typed) (nil? default))) lg))
    (throw (IllegalArgumentException. (str "lag not supported for " (col/-type-tag col))))))

;; ════════════════════════════════════════════════════════════════════════
;; Lead — value from next row
;; ════════════════════════════════════════════════════════════════════════

(defn lead [col offset default]
  (case (col/-type-tag col)
    :i64
    (let [^flatiron.column.I64Column typed col
          n (to-int (.len typed))
          ^longs src (.data typed)
          off (.offset typed)
          ^longs dst (long-array n)
          lg  (.logical typed)
          def (to-long (cond (nil? default)                   col/NULL_I64
                             (and lg (not (number? default))) (types/encode lg default)
                             :else                            default))]
      (dotimes [i n]
        (let [idx (+ i offset)]
          (aset dst i (if (>= idx n) def (aget src (+ off idx))))))
      ;; a nil default writes NULL_I64 into the shifted-in slots, so the result
      ;; is nullable even when the source was not — don't copy the source flag.
      (I64Column. dst n 0 (boolean (or (.has-nulls typed) (nil? default))) lg))
    :f64
    (let [^flatiron.column.F64Column typed col
          n (to-int (.len typed))
          ^doubles src (.data typed)
          off (.offset typed)
          dst (double-array n)
          lg  (.logical typed)
          def (double (cond (nil? default)                   Double/NaN
                            (and lg (not (number? default))) (types/encode lg default)
                            :else                            default))]
      (dotimes [i n]
        (let [idx (+ i offset)]
          (aset dst i (if (>= idx n) def (aget src (+ off idx))))))
      (F64Column. dst n 0 (boolean (or (.has-nulls typed) (nil? default))) lg))
    (throw (IllegalArgumentException. (str "lead not supported for " (col/-type-tag col))))))
