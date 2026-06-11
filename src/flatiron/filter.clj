(ns flatiron.filter
  "Row filtering: build a boolean mask by comparing a column against a scalar,
   combine masks with boolean connectives, then materialize the surviving rows
   into a new table.

   Comparison operators dispatch on the column's runtime type, so the literal
   in a predicate does not need to match the column type (an integer literal
   compared against an F64 column is coerced to double, and so on)."
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl]
            [flatiron.types :as types])
  (:import [flatiron.column I64Column F64Column BoolColumn SymColumn StrColumn]))

(set! *warn-on-reflection* true)

;; Comparison ops resolve to an int code outside the row loop; the per-row
;; dispatch is a tableswitch on a primitive, not a keyword lookup with
;; boxed operands.
(defn- op-code ^long [op]
  (case op
    :gt 0 :lt 1 :ge 2 :le 3 :eq 4 :ne 5
    (throw (IllegalArgumentException. (str "Unknown comparison op: " op)))))

(defn scalar-pred
  "Compare every element of `col` against scalar `val` with `op`, one of
   :gt :lt :ge :le :eq :ne. Returns a BoolColumn (byte 1 = pass).
   Null and NaN elements always yield 0, matching the cmp conventions.
   Only :eq / :ne are valid for :sym, :str and :bool columns."
  ^BoolColumn [col op val]
  (let [n   (col/-len col)
        out (byte-array n)]
    (case (col/-type-tag col)
      :i64
      (let [^I64Column c col
            ^longs data (.data c)
            offset (.offset c)
            hn     (.has-nulls c)
            lg     (.logical c)
            ;; encode a domain literal (e.g. a LocalDate) to its raw long once,
            ;; outside the row loop, so the comparison stays a primitive op.
            v      (long (if (and lg (not (number? val))) (types/encode lg val) val))
            code   (op-code op)]
        (loop [i (int 0)]
          (when (< i n)
            (let [x (aget data (+ offset i))
                  pass (if (and hn (= x col/NULL_I64))
                         false
                         (case code
                           0 (> x v) 1 (< x v) 2 (>= x v)
                           3 (<= x v) 4 (== x v) 5 (not (== x v))))]
              (aset out i (byte (if pass 1 0))))
            (recur (unchecked-inc i)))))
      :f64
      (let [^F64Column c col
            ^doubles data (.data c)
            offset (.offset c)
            lg     (.logical c)
            v      (double (if (and lg (not (number? val))) (types/encode lg val) val))
            code   (op-code op)]
        (loop [i (int 0)]
          (when (< i n)
            (let [x (aget data (+ offset i))
                  pass (if (Double/isNaN x)
                         false
                         (case code
                           0 (> x v) 1 (< x v) 2 (>= x v)
                           3 (<= x v) 4 (== x v) 5 (not (== x v))))]
              (aset out i (byte (if pass 1 0))))
            (recur (unchecked-inc i)))))
      (:sym :str)
      (do
        (when-not (#{:eq :ne} op)
          (throw (IllegalArgumentException.
                  (str "Operator " op " not supported for " (col/-type-tag col) " columns"))))
        (let [eq? (= op :eq)]
          (loop [i (int 0)]
            (when (< i n)
              (let [x (col/-get-obj col i)
                    pass (if (nil? x)
                           false
                           (if eq? (= x val) (not= x val)))]
                (aset out i (byte (if pass 1 0))))
              (recur (unchecked-inc i))))))
      :bool
      (do
        (when-not (#{:eq :ne} op)
          (throw (IllegalArgumentException.
                  (str "Operator " op " not supported for :bool columns"))))
        (let [^BoolColumn c col
              ^bytes data (.data c)
              offset (.offset c)
              v (byte (if (boolean val) 1 0))
              eq? (= op :eq)]
          (loop [i (int 0)]
            (when (< i n)
              (let [x (aget data (+ offset i))]
                (aset out i (byte (if (if eq? (== x v) (not (== x v))) 1 0))))
              (recur (unchecked-inc i))))))
      (throw (IllegalArgumentException.
              (str "Unsupported column type for predicate: " (col/-type-tag col)))))
    (BoolColumn. out n 0)))

(defn- bool-binop ^BoolColumn [^BoolColumn a ^BoolColumn b and?]
  (let [n   (col/-len a)
        out (byte-array n)
        ^bytes ad (.data a) ao (.offset a)
        ^bytes bd (.data b) bo (.offset b)]
    (if and?
      (loop [i (int 0)]
        (when (< i n)
          (aset out i (byte (bit-and (aget ad (+ ao i)) (aget bd (+ bo i)))))
          (recur (unchecked-inc i))))
      (loop [i (int 0)]
        (when (< i n)
          (aset out i (byte (bit-or (aget ad (+ ao i)) (aget bd (+ bo i)))))
          (recur (unchecked-inc i)))))
    (BoolColumn. out n 0)))

(defn bool-and
  "Element-wise AND of one or more BoolColumns."
  ^BoolColumn [c & cs]
  (reduce #(bool-binop %1 %2 true) c cs))

(defn bool-or
  "Element-wise OR of one or more BoolColumns."
  ^BoolColumn [c & cs]
  (reduce #(bool-binop %1 %2 false) c cs))

(defn bool-not
  "Element-wise NOT of a BoolColumn."
  ^BoolColumn [^BoolColumn c]
  (let [n   (col/-len c)
        out (byte-array n)
        ^bytes d (.data c)
        o (.offset c)]
    (loop [i (int 0)]
      (when (< i n)
        (aset out i (byte (bit-xor 1 (aget d (+ o i)))))
        (recur (unchecked-inc i))))
    (BoolColumn. out n 0)))

(defn- gather-rows
  "Build a new column holding the rows of `col` named by `idx`. The result has
   length (alength idx), unlike sort's gather which permutes all rows."
  [col ^ints idx]
  (let [k (alength idx)]
    (case (col/-type-tag col)
      :i64
      (let [^I64Column c col
            ^longs src (.data c)
            offset (.offset c)
            dst (long-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (col/i64-like c dst k))
      :f64
      (let [^F64Column c col
            ^doubles src (.data c)
            offset (.offset c)
            dst (double-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (col/f64-like c dst k))
      :sym
      (let [^SymColumn c col
            ^objects src (.data c)
            offset (.offset c)
            dst (object-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (SymColumn. dst k 0 (.has-nulls c)))
      :str
      (let [^StrColumn c col
            ^objects src (.data c)
            offset (.offset c)
            dst (object-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (StrColumn. dst k 0 (.has-nulls c)))
      :bool
      (let [^BoolColumn c col
            ^bytes src (.data c)
            offset (.offset c)
            dst (byte-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (BoolColumn. dst k 0))
      (throw (IllegalArgumentException.
              (str "Unsupported column type for filter: " (col/-type-tag col)))))))

(defn filter-rows
  "Return a new table containing only the rows where `mask` (a BoolColumn) is 1.
   Column order and types are preserved."
  [table ^BoolColumn mask]
  (let [n  (col/-len mask)
        ^bytes md (.data mask)
        mo (.offset mask)
        k  (loop [i (int 0), c (int 0)]
             (if (< i n)
               (recur (unchecked-inc i)
                      (if (== 1 (aget md (+ mo i))) (unchecked-inc c) c))
               c))
        idx (int-array k)]
    (loop [i (int 0), j (int 0)]
      (when (< i n)
        (if (== 1 (aget md (+ mo i)))
          (do (aset idx j i) (recur (unchecked-inc i) (unchecked-inc j)))
          (recur (unchecked-inc i) j))))
    (let [ncols    (tbl/ncols table)
          schema   (mapv #(tbl/col-name table %) (range ncols))
          new-cols (mapv #(gather-rows (tbl/col-by-idx table %) idx) (range ncols))]
      (tbl/table schema new-cols))))
