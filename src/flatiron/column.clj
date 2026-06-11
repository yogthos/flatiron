(ns flatiron.column)

(set! *warn-on-reflection* true)

;; ── Protocol ──────────────────────────────────────────────────────────
(defprotocol PColumn
  (-len        [this]               "Number of rows")
  (-type-tag   [this]               "Keyword type tag: :i64, :f64, :bool, :sym, :str")
  (-has-nulls? [this]               "Whether column may contain null sentinels")
  (-get-long   [this idx]           "Access row as unboxed long")
  (-get-double [this idx]           "Access row as unboxed double")
  (-get-obj    [this idx]           "Access row as Object (keyword, String, Boolean)")
  (-slice      [this offset len]    "Zero-copy sub-view sharing the backing array"))

;; ── Null sentinels ────────────────────────────────────────────────────
(def ^:const NULL_I64 Long/MIN_VALUE)

;; ── I64Column ─────────────────────────────────────────────────────────
(deftype I64Column [^longs data ^long len ^long offset ^boolean has-nulls]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :i64)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (aget data (+ offset idx)))
  (-get-double [_ idx]   (double (aget data (+ offset idx))))
  (-get-obj    [_ idx]   (let [v (aget data (+ offset idx))]
                          (if (and has-nulls (= v NULL_I64)) nil v)))
  (-slice      [_ o l]   (I64Column. data l (+ offset o) has-nulls)))

(defn i64-column
  "Create an I64Column from a seq of longs. Nil values become NULL_I64 sentinel."
  [xs]
  (let [n  (count xs)
        arr (long-array n)
        has-nulls? (loop [i 0, s xs, hn false]
                     (if (seq s)
                       (let [x (first s)]
                         (if (nil? x)
                           (do (aset arr i NULL_I64)
                               (recur (unchecked-inc i) (rest s) true))
                           (do (aset arr i (long x))
                               (recur (unchecked-inc i) (rest s) hn))))
                       hn))]
    (I64Column. arr n 0 has-nulls?)))

(defn i64-column-from-array
  "Create an I64Column from a pre-built long array."
  [^longs arr]
  (let [n (alength arr)]
    (I64Column. arr n 0 false)))

;; ── F64Column ─────────────────────────────────────────────────────────
(deftype F64Column [^doubles data ^long len ^long offset ^boolean has-nulls]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :f64)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (long (aget data (+ offset idx))))
  (-get-double [_ idx]   (aget data (+ offset idx)))
  (-get-obj    [_ idx]   (let [v (aget data (+ offset idx))]
                          (if (and has-nulls (Double/isNaN v)) nil v)))
  (-slice      [_ o l]   (F64Column. data l (+ offset o) has-nulls)))

(defn f64-column
  "Create an F64Column from a seq of doubles. Nil values become NaN sentinel."
  [xs]
  (let [n  (count xs)
        arr (double-array n)
        has-nulls? (loop [i 0, s xs, hn false]
                     (if (seq s)
                       (let [x (first s)]
                         (if (nil? x)
                           (do (aset arr i Double/NaN)
                               (recur (unchecked-inc i) (rest s) true))
                           (do (aset arr i (double x))
                               (recur (unchecked-inc i) (rest s) hn))))
                       hn))]
    (F64Column. arr n 0 has-nulls?)))

;; ── BoolColumn ────────────────────────────────────────────────────────
;; Non-nullable. Backed by byte array: 0 = false, 1 = true.
(deftype BoolColumn [^bytes data ^long len ^long offset]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :bool)
  (-has-nulls? [_]       false)
  (-get-long   [_ idx]   (if (= (aget data (+ offset idx)) 1) 1 0))
  (-get-double [_ idx]   (if (= (aget data (+ offset idx)) 1) 1.0 0.0))
  (-get-obj    [_ idx]   (= (aget data (+ offset idx)) 1))
  (-slice      [_ o l]   (BoolColumn. data l (+ offset o))))

(defn bool-column
  "Create a BoolColumn from a seq of booleans."
  [xs]
  (let [n  (count xs)
        arr (byte-array n)]
    (loop [i 0, s xs]
      (when (seq s)
        (aset arr i (byte (if (first s) 1 0)))
        (recur (unchecked-inc i) (rest s))))
    (BoolColumn. arr n 0)))

;; ── SymColumn ─────────────────────────────────────────────────────────
;; Keyword column. nil = null sentinel.
(deftype SymColumn [^objects data ^long len ^long offset ^boolean has-nulls]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :sym)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (throw (UnsupportedOperationException. "SymColumn has no long representation")))
  (-get-double [_ idx]   (throw (UnsupportedOperationException. "SymColumn has no double representation")))
  (-get-obj    [_ idx]   (aget data (+ offset idx)))
  (-slice      [_ o l]   (SymColumn. data l (+ offset o) has-nulls)))

(defn sym-column
  "Create a SymColumn from a seq of keywords. Nil values are null."
  [xs]
  (let [n     (count xs)
        arr   (object-array n)
        has-nulls? (loop [i 0, s xs, hn false]
                     (if (seq s)
                       (let [x (first s)]
                         (aset arr i x)
                         (recur (unchecked-inc i) (rest s) (or hn (nil? x))))
                       hn))]
    (SymColumn. arr n 0 has-nulls?)))

;; ── StrColumn ─────────────────────────────────────────────────────────
;; String column. nil = null sentinel.
(deftype StrColumn [^objects data ^long len ^long offset ^boolean has-nulls]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :str)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (throw (UnsupportedOperationException. "StrColumn has no long representation")))
  (-get-double [_ idx]   (throw (UnsupportedOperationException. "StrColumn has no double representation")))
  (-get-obj    [_ idx]   (aget data (+ offset idx)))
  (-slice      [_ o l]   (StrColumn. data l (+ offset o) has-nulls)))

(defn str-column
  "Create a StrColumn from a seq of strings. Nil values are null."
  [xs]
  (let [n     (count xs)
        arr   (object-array n)
        has-nulls? (loop [i 0, s xs, hn false]
                     (if (seq s)
                       (let [x (first s)]
                         (aset arr i x)
                         (recur (unchecked-inc i) (rest s) (or hn (nil? x))))
                       hn))]
    (StrColumn. arr n 0 has-nulls?)))
