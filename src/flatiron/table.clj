(ns flatiron.table
  (:require [flatiron.column :as col]))

(set! *warn-on-reflection* true)

;; ── Table = ordered schema (keyword array) + columns (PColumn array) ──
(defrecord Table [^objects schema     ;; array of keywords
                  ^objects columns    ;; array of PColumn
                  ^long    ncols])    ;; cached column count

(defn table
  "Create a Table from a schema vector and a columns vector."
  [schema-vec col-vec]
  (let [n (count schema-vec)]
    (assert (= n (count col-vec))
            (str "Schema length " n " != columns length " (count col-vec)))
    (Table. (object-array schema-vec)
            (object-array col-vec)
            n)))

(defn ^long ncols [^Table t]
  (.ncols t))

(defn ^long nrows [^Table t]
  (if (zero? (.ncols t))
    0
    (col/-len (aget ^objects (.columns t) 0))))

(defn col-idx
  "Find the index of a column by keyword. Returns -1 if not found."
  [^Table t kw]
  (let [^objects schema (.schema t)]
    (loop [i 0]
      (if (< i (alength schema))
        (if (= kw (aget schema i))
          i
          (recur (unchecked-inc i)))
        -1))))

(defn col
  "Get a column by keyword. Returns nil if not found."
  [^Table t kw]
  (let [idx (col-idx t kw)]
    (when (>= idx 0)
      (aget ^objects (.columns t) idx))))

(defn col-by-idx
  "Get a column by integer index."
  [^Table t ^long idx]
  (aget ^objects (.columns t) idx))

(defn col-name
  "Get the keyword name of a column by index."
  [^Table t ^long idx]
  (aget ^objects (.schema t) idx))
