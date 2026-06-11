(ns flatiron.column
  (:require [flatiron.types :as types]))

(set! *warn-on-reflection* true)

;; ── Protocol ──────────────────────────────────────────────────────────
(defprotocol PColumn
  (-len        [this]               "Number of rows")
  (-type-tag   [this]               "Physical type tag: :i64, :f64, :bool, :sym, :str")
  (-logical-tag [this]              "Logical type tag (e.g. :date) or nil for a plain column")
  (-has-nulls? [this]               "Whether column may contain null sentinels")
  (-get-long   [this idx]           "Access row as unboxed long")
  (-get-double [this idx]           "Access row as unboxed double")
  (-get-obj    [this idx]           "Access row as Object (keyword, String, Boolean, or decoded logical value)")
  (-slice      [this offset len]    "Zero-copy sub-view sharing the backing array"))

;; ── Null sentinels ────────────────────────────────────────────────────
(def ^:const NULL_I64 Long/MIN_VALUE)

;; ── I64Column ─────────────────────────────────────────────────────────
;; `logical` is a logical-type keyword (e.g. :date) or nil. When set, -get-obj
;; decodes the raw long through the registered codec; the numeric kernels read
;; `data` directly and never look at it, so it adds no per-element cost.
(deftype I64Column [^longs data ^long len ^long offset ^boolean has-nulls logical]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :i64)
  (-logical-tag [_]      logical)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (aget data (+ offset idx)))
  (-get-double [_ idx]   (double (aget data (+ offset idx))))
  (-get-obj    [_ idx]   (let [v (aget data (+ offset idx))]
                          (cond
                            (and has-nulls (= v NULL_I64)) nil
                            logical                        (types/decode logical v)
                            :else                          v)))
  (-slice      [_ o l]   (I64Column. data l (+ offset o) has-nulls logical)))

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
    (I64Column. arr n 0 has-nulls? nil)))

(defn i64-column-from-array
  "Create an I64Column from a pre-built long array."
  [^longs arr]
  (let [n (alength arr)]
    (I64Column. arr n 0 false nil)))

;; ── F64Column ─────────────────────────────────────────────────────────
(deftype F64Column [^doubles data ^long len ^long offset ^boolean has-nulls logical]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :f64)
  (-logical-tag [_]      logical)
  (-has-nulls? [_]       has-nulls)
  (-get-long   [_ idx]   (long (aget data (+ offset idx))))
  (-get-double [_ idx]   (aget data (+ offset idx)))
  (-get-obj    [_ idx]   (let [v (aget data (+ offset idx))]
                          (cond
                            (and has-nulls (Double/isNaN v)) nil
                            ;; pass the raw double, not (long v) — an f64-backed
                            ;; codec needs the fractional part intact.
                            logical                          (types/decode logical v)
                            :else                            v)))
  (-slice      [_ o l]   (F64Column. data l (+ offset o) has-nulls logical)))

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
    (F64Column. arr n 0 has-nulls? nil)))

;; ── BoolColumn ────────────────────────────────────────────────────────
;; Non-nullable. Backed by byte array: 0 = false, 1 = true.
(deftype BoolColumn [^bytes data ^long len ^long offset]
  PColumn
  (-len        [_]       len)
  (-type-tag   [_]       :bool)
  (-logical-tag [_]      nil)
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
  (-logical-tag [_]      nil)
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
  (-logical-tag [_]      nil)
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

;; ── Logical (custom) typed columns ──────────────────────────────────────
;; A logically-typed column stores the codec-encoded primitive in the same
;; backing array a plain numeric column uses, and tags itself with the logical
;; keyword. All numeric operations treat it as its physical type; only -get-obj,
;; persistence and display decode it.

(defn typed-column
  "Create a logically-typed column from a seq of domain objects. The values are
   encoded to the physical backing type via the registered codec; nil values
   become the physical null sentinel. Supports :i64- and :f64-backed types."
  [logical-tag xs]
  (let [phys (types/physical logical-tag)
        enc  (:encode (types/codec logical-tag))
        n    (count xs)]
    (case phys
      :i64
      (let [arr (long-array n)
            has-nulls? (loop [i 0, s xs, hn false]
                         (if (seq s)
                           (let [x (first s)]
                             (if (nil? x)
                               (do (aset arr i NULL_I64)
                                   (recur (unchecked-inc i) (rest s) true))
                               (do (aset arr i (long (enc x)))
                                   (recur (unchecked-inc i) (rest s) hn))))
                           hn))]
        (I64Column. arr n 0 has-nulls? logical-tag))
      :f64
      (let [arr (double-array n)
            has-nulls? (loop [i 0, s xs, hn false]
                         (if (seq s)
                           (let [x (first s)]
                             (if (nil? x)
                               (do (aset arr i Double/NaN)
                                   (recur (unchecked-inc i) (rest s) true))
                               (do (aset arr i (double (enc x)))
                                   (recur (unchecked-inc i) (rest s) hn))))
                           hn))]
        (F64Column. arr n 0 has-nulls? logical-tag))
      (throw (IllegalArgumentException.
              (str "Unsupported physical backing " phys " for logical type " logical-tag))))))

(defn date-column     "LocalDate column."     [xs] (typed-column :date xs))
(defn instant-column  "Instant column."       [xs] (typed-column :instant xs))
(defn datetime-column "LocalDateTime column." [xs] (typed-column :datetime xs))
(defn duration-column "Duration column."      [xs] (typed-column :duration xs))

(defn with-logical
  "Re-tag an existing numeric column with `logical-tag` (or nil to clear),
   sharing the backing array. Only valid for :i64/:f64 columns."
  [col logical-tag]
  (case (-type-tag col)
    :i64 (let [^I64Column c col]
           (I64Column. (.data c) (.len c) (.offset c) (.has-nulls c) logical-tag))
    :f64 (let [^F64Column c col]
           (F64Column. (.data c) (.len c) (.offset c) (.has-nulls c) logical-tag))
    (throw (IllegalArgumentException.
            (str "Cannot attach a logical type to a " (-type-tag col) " column")))))

(defn i64-like
  "Build an I64Column over `data`/`len` carrying the null flag and logical tag
   of `src`. Used by gather/sort/filter to preserve a logical type."
  ^I64Column [^I64Column src ^longs data ^long len]
  (I64Column. data len 0 (.has-nulls src) (.logical src)))

(defn f64-like
  "Build an F64Column over `data`/`len` carrying the null flag and logical tag
   of `src`."
  ^F64Column [^F64Column src ^doubles data ^long len]
  (F64Column. data len 0 (.has-nulls src) (.logical src)))
