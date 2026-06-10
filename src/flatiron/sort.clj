(ns flatiron.sort
  "In-place sort on index arrays — stable TimSort over column data.
   Does not permute column data; produces a sorted index array instead."
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column BoolColumn SymColumn StrColumn]))

;; ════════════════════════════════════════════════════════════════════════
;; Comparator construction
;; ════════════════════════════════════════════════════════════════════════

(defn- column-comparator
  "Return a java.util.Comparator that compares rows by their values in `col`."
  ^java.util.Comparator [col direction]
  (let [mult (if (= direction :asc) 1 -1)]
    (case (col/-type-tag col)
      :i64
      (let [^flatiron.column.I64Column c col
            data (.data c)
            offset (.offset c)]
        (reify java.util.Comparator
          (compare [_ a b]
            (let [va (aget data (+ offset (int a)))
                  vb (aget data (+ offset (int b)))]
              (* mult (Long/compare va vb))))))
      :f64
      (let [^flatiron.column.F64Column c col
            data (.data c)
            offset (.offset c)]
        (reify java.util.Comparator
          (compare [_ a b]
            (let [va (aget data (+ offset (int a)))
                  vb (aget data (+ offset (int b)))]
              (* mult (Double/compare va vb))))))
      :sym
      (let [^flatiron.column.SymColumn c col
            data (.data c)
            offset (.offset c)]
        (reify java.util.Comparator
          (compare [_ a b]
            (let [va (aget data (+ offset (int a)))
                  vb (aget data (+ offset (int b)))]
              (if (nil? va)
                (if (nil? vb) 0 (* mult -1))
                (if (nil? vb) (* mult 1)
                  (* mult (.compareTo ^Comparable va vb))))))))
      :str
      (let [^flatiron.column.StrColumn c col
            data (.data c)
            offset (.offset c)]
        (reify java.util.Comparator
          (compare [_ a b]
            (let [va (aget data (+ offset (int a)))
                  vb (aget data (+ offset (int b)))]
              (if (nil? va)
                (if (nil? vb) 0 (* mult -1))
                (if (nil? vb) (* mult 1)
                  (* mult (.compareTo ^Comparable va vb))))))))
      :bool
      (let [^flatiron.column.BoolColumn c col
            data (.data c)
            offset (.offset c)]
        (reify java.util.Comparator
          (compare [_ a b]
            (let [va (aget data (+ offset (int a)))
                  vb (aget data (+ offset (int b)))]
              (* mult (Long/compare (long va) (long vb)))))))
      (throw (IllegalArgumentException.
              (str "Unsupported sort type: " (col/-type-tag col)))))))

;; ════════════════════════════════════════════════════════════════════════
;; Sort index
;; ════════════════════════════════════════════════════════════════════════

(defn sort-index
  "Return an int-array of row indices sorted by `col` in `direction` (:asc or :desc).
   Stable sort — preserves original order for equal keys."
  ^"[I" [col direction]
  (let [n (int (col/-len col))
        idx (make-array Integer n)]
    (dotimes [i n] (aset idx i (Integer/valueOf i)))
    (java.util.Arrays/sort ^"[Ljava.lang.Integer;" idx (column-comparator col direction))
    (let [out (int-array n)]
      (dotimes [i n] (aset out i (.intValue ^Integer (aget idx i))))
      out)))

;; ════════════════════════════════════════════════════════════════════════
;; Multi-key sort
;; ════════════════════════════════════════════════════════════════════════

(defn- chained-comparator
  "Chain multiple comparators — secondary comparator only fires on equal primary keys."
  ^java.util.Comparator [comparators]
  (reify java.util.Comparator
    (compare [_ a b]
      (loop [cs comparators]
        (if (seq cs)
          (let [c (.compare ^java.util.Comparator (first cs) a b)]
            (if (zero? c)
              (recur (rest cs))
              c))
          0)))))

(defn sort-index-by
  "Multi-key stable sort. `keys` is a vector of [column direction] pairs.
   Primary key first, then secondary, etc."
  ^"[I" [key-specs]
  (let [first-col  (ffirst key-specs)
        n          (int (col/-len first-col))
        idx        (make-array Integer n)
        comps      (mapv (fn [[col dir]] (column-comparator col dir)) key-specs)
        comparator (chained-comparator comps)]
    (dotimes [i n] (aset idx i (Integer/valueOf i)))
    (java.util.Arrays/sort ^"[Ljava.lang.Integer;" idx comparator)
    (let [out (int-array n)]
      (dotimes [i n] (aset out i (.intValue ^Integer (aget idx i))))
      out)))

;; ════════════════════════════════════════════════════════════════════════
;; Apply index to column — produce sorted column
;; ════════════════════════════════════════════════════════════════════════

(defn gather-column
  "Reorder a column according to an index array. Returns new column."
  [col ^ints idx]
  (let [n (int (col/-len col))]
    (case (col/-type-tag col)
      :i64
      (let [^flatiron.column.I64Column c col
            src (.data c)
            offset (.offset c)
            dst (long-array n)]
        (dotimes [i n]
          (aset dst i (aget src (+ offset (aget idx i)))))
        (I64Column. dst n 0 (.has-nulls c)))
      :f64
      (let [^flatiron.column.F64Column c col
            src (.data c)
            offset (.offset c)
            dst (double-array n)]
        (dotimes [i n]
          (aset dst i (aget src (+ offset (aget idx i)))))
        (F64Column. dst n 0 (.has-nulls c)))
      :sym
      (let [^flatiron.column.SymColumn c col
            src (.data c)
            offset (.offset c)
            dst (object-array n)]
        (dotimes [i n]
          (aset dst i (aget src (+ offset (aget idx i)))))
        (SymColumn. dst n 0 (.has-nulls c)))
      :str
      (let [^flatiron.column.StrColumn c col
            src (.data c)
            offset (.offset c)
            dst (object-array n)]
        (dotimes [i n]
          (aset dst i (aget src (+ offset (aget idx i)))))
        (StrColumn. dst n 0 (.has-nulls c)))
      :bool
      (let [^flatiron.column.BoolColumn c col
            src (.data c)
            offset (.offset c)
            dst (byte-array n)]
        (dotimes [i n]
          (aset dst i (aget src (+ offset (aget idx i)))))
        (BoolColumn. dst n 0))
      (throw (IllegalArgumentException.
              (str "Unsupported gather type: " (col/-type-tag col)))))))

(defn sort-table
  "Sort an entire table by key specs. Returns a new table with columns reordered.
   key-specs: [[col-kw direction] ...]
   Example: (sort-table t [[:date :asc] [:amount :desc]])"
  [table key-specs]
  (let [cols    (mapv (fn [[kw dir]] [(tbl/col table kw) dir]) key-specs)
        idx     (sort-index-by cols)
        schema  (mapv #(tbl/col-name table %) (range (tbl/ncols table)))
        new-cols (mapv #(gather-column (tbl/col-by-idx table %) idx)
                       (range (tbl/ncols table)))]
    (tbl/table schema new-cols)))
