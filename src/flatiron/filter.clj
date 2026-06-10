(ns flatiron.filter
  "Row filtering: build a boolean mask by comparing a column against a scalar,
   combine masks with boolean connectives, then materialize the surviving rows
   into a new table.

   Comparison operators dispatch on the column's runtime type, so the literal
   in a predicate does not need to match the column type (an integer literal
   compared against an F64 column is coerced to double, and so on)."
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column BoolColumn SymColumn StrColumn]))

(defn- num-op [op x y]
  (case op
    :gt (> x y)
    :lt (< x y)
    :ge (>= x y)
    :le (<= x y)
    :eq (== x y)
    :ne (not (== x y))
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
            data   (.data c)
            offset (.offset c)
            hn     (.has-nulls c)
            v      (long val)]
        (dotimes [i n]
          (let [x (aget data (+ offset i))]
            (aset out i (byte (if (and hn (= x col/NULL_I64))
                                0
                                (if (num-op op x v) 1 0)))))))
      :f64
      (let [^F64Column c col
            data   (.data c)
            offset (.offset c)
            v      (double val)]
        (dotimes [i n]
          (let [x (aget data (+ offset i))]
            (aset out i (byte (if (Double/isNaN x)
                                0
                                (if (num-op op x v) 1 0)))))))
      (:sym :str)
      (do
        (when-not (#{:eq :ne} op)
          (throw (IllegalArgumentException.
                  (str "Operator " op " not supported for " (col/-type-tag col) " columns"))))
        (dotimes [i n]
          (let [x (col/-get-obj col i)]
            (aset out i (byte (if (nil? x)
                                0
                                (if (case op :eq (= x val) :ne (not= x val)) 1 0)))))))
      :bool
      (do
        (when-not (#{:eq :ne} op)
          (throw (IllegalArgumentException.
                  (str "Operator " op " not supported for :bool columns"))))
        (let [v (boolean val)]
          (dotimes [i n]
            (let [x (col/-get-obj col i)]
              (aset out i (byte (if (case op :eq (= x v) :ne (not= x v)) 1 0)))))))
      (throw (IllegalArgumentException.
              (str "Unsupported column type for predicate: " (col/-type-tag col)))))
    (BoolColumn. out n 0)))

(defn- bool-binop ^BoolColumn [^BoolColumn a ^BoolColumn b f]
  (let [n   (col/-len a)
        out (byte-array n)
        ad  (.data a) ao (.offset a)
        bd  (.data b) bo (.offset b)]
    (dotimes [i n]
      (aset out i (byte (if (f (= 1 (aget ad (+ ao i)))
                              (= 1 (aget bd (+ bo i))))
                          1 0))))
    (BoolColumn. out n 0)))

(defn bool-and
  "Element-wise AND of one or more BoolColumns."
  ^BoolColumn [c & cs]
  (reduce #(bool-binop %1 %2 (fn [x y] (and x y))) c cs))

(defn bool-or
  "Element-wise OR of one or more BoolColumns."
  ^BoolColumn [c & cs]
  (reduce #(bool-binop %1 %2 (fn [x y] (or x y))) c cs))

(defn bool-not
  "Element-wise NOT of a BoolColumn."
  ^BoolColumn [^BoolColumn c]
  (let [n   (col/-len c)
        out (byte-array n)
        d   (.data c) o (.offset c)]
    (dotimes [i n]
      (aset out i (byte (if (= 1 (aget d (+ o i))) 0 1))))
    (BoolColumn. out n 0)))

(defn- gather-rows
  "Build a new column holding the rows of `col` named by `idx`. The result has
   length (alength idx), unlike sort's gather which permutes all rows."
  [col ^ints idx]
  (let [k (alength idx)]
    (case (col/-type-tag col)
      :i64
      (let [^I64Column c col, src (.data c), offset (.offset c), dst (long-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (I64Column. dst k 0 (.has-nulls c)))
      :f64
      (let [^F64Column c col, src (.data c), offset (.offset c), dst (double-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (F64Column. dst k 0 (.has-nulls c)))
      :sym
      (let [^SymColumn c col, src (.data c), offset (.offset c), dst (object-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (SymColumn. dst k 0 (.has-nulls c)))
      :str
      (let [^StrColumn c col, src (.data c), offset (.offset c), dst (object-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (StrColumn. dst k 0 (.has-nulls c)))
      :bool
      (let [^BoolColumn c col, src (.data c), offset (.offset c), dst (byte-array k)]
        (dotimes [i k] (aset dst i (aget src (+ offset (aget idx i)))))
        (BoolColumn. dst k 0))
      (throw (IllegalArgumentException.
              (str "Unsupported column type for filter: " (col/-type-tag col)))))))

(defn filter-rows
  "Return a new table containing only the rows where `mask` (a BoolColumn) is 1.
   Column order and types are preserved."
  [table ^BoolColumn mask]
  (let [n  (col/-len mask)
        md (.data mask)
        mo (.offset mask)
        k  (loop [i 0, c 0]
             (if (< i n)
               (recur (unchecked-inc i) (if (= 1 (aget md (+ mo i))) (unchecked-inc c) c))
               c))
        idx (int-array k)]
    (loop [i 0, j 0]
      (when (< i n)
        (if (= 1 (aget md (+ mo i)))
          (do (aset idx j i) (recur (unchecked-inc i) (unchecked-inc j)))
          (recur (unchecked-inc i) j))))
    (let [ncols    (tbl/ncols table)
          schema   (mapv #(tbl/col-name table %) (range ncols))
          new-cols (mapv #(gather-rows (tbl/col-by-idx table %) idx) (range ncols))]
      (tbl/table schema new-cols))))
