(ns flatiron.morsel
  "Morsel iterator protocol — batches rows into reusable primitive buffers
   so that inner loops avoid per-element protocol dispatch."
  (:require [flatiron.column :as col]))

(def ^:const MORSEL-SIZE 1024)

;; ── Protocol ──────────────────────────────────────────────────────────
(defprotocol IMorselSource
  (morsel-count       [_]                      "Total number of rows")
  (morsel-next-i64!   [_ ^longs buf buf-off max] "Fill buf with next chunk. Returns count (0 = done).")
  (morsel-next-f64!   [_ ^doubles buf buf-off max] "Fill buf with next chunk. Returns count.")
  (morsel-next-bool!  [_ ^bytes buf buf-off max] "Fill buf with next chunk. Returns count.")
  (morsel-next-obj!   [_ ^objects buf buf-off max] "Fill buf with next chunk. Returns count.")
  (morsel-reset!      [_]                      "Reset cursor to start."))

;; ── I64MorselSource ───────────────────────────────────────────────────
(deftype I64MorselSource [^longs data ^long len ^long offset
                          ^:volatile-mutable ^long cursor]
  IMorselSource
  (morsel-count [_] len)
  (morsel-next-i64! [_ buf buf-off max]
    (let [remaining (- len cursor)
          n (long (min remaining max))]
      (if (pos? n)
        (do (System/arraycopy data (+ offset cursor) buf buf-off n)
            (set! cursor (+ cursor n))
            n)
        0)))
  (morsel-next-f64!  [_ _ _ _]  0)
  (morsel-next-bool! [_ _ _ _]  0)
  (morsel-next-obj!  [_ _ _ _]  0)
  (morsel-reset! [_] (set! cursor 0)))

(defn i64-morsel-source [col]
  (I64MorselSource. (.data col) (.len col) (.offset col) 0))

;; ── F64MorselSource ───────────────────────────────────────────────────
(deftype F64MorselSource [^doubles data ^long len ^long offset
                          ^:volatile-mutable ^long cursor]
  IMorselSource
  (morsel-count [_] len)
  (morsel-next-i64!  [_ _ _ _]  0)
  (morsel-next-f64! [_ buf buf-off max]
    (let [remaining (- len cursor)
          n (long (min remaining max))]
      (if (pos? n)
        (do (System/arraycopy data (+ offset cursor) buf buf-off n)
            (set! cursor (+ cursor n))
            n)
        0)))
  (morsel-next-bool! [_ _ _ _]  0)
  (morsel-next-obj!  [_ _ _ _]  0)
  (morsel-reset! [_] (set! cursor 0)))

(defn f64-morsel-source [col]
  (F64MorselSource. (.data col) (.len col) (.offset col) 0))

;; ── BoolMorselSource ──────────────────────────────────────────────────
(deftype BoolMorselSource [^bytes data ^long len ^long offset
                           ^:volatile-mutable ^long cursor]
  IMorselSource
  (morsel-count [_] len)
  (morsel-next-i64!  [_ _ _ _]  0)
  (morsel-next-f64!  [_ _ _ _]  0)
  (morsel-next-bool! [_ buf buf-off max]
    (let [remaining (- len cursor)
          n (long (min remaining max))]
      (if (pos? n)
        (do (System/arraycopy data (+ offset cursor) buf buf-off n)
            (set! cursor (+ cursor n))
            n)
        0)))
  (morsel-next-obj! [_ _ _ _]  0)
  (morsel-reset! [_] (set! cursor 0)))

(defn bool-morsel-source [col]
  (BoolMorselSource. (.data col) (.len col) (.offset col) 0))

;; ── Sym/Str MorselSource (object array) ───────────────────────────────
(deftype ObjMorselSource [^objects data ^long len ^long offset
                          ^:volatile-mutable ^long cursor]
  IMorselSource
  (morsel-count [_] len)
  (morsel-next-i64!  [_ _ _ _]  0)
  (morsel-next-f64!  [_ _ _ _]  0)
  (morsel-next-bool! [_ _ _ _]  0)
  (morsel-next-obj! [_ buf buf-off max]
    (let [remaining (- len cursor)
          n (long (min remaining max))]
      (if (pos? n)
        (do (System/arraycopy data (+ offset cursor) buf buf-off n)
            (set! cursor (+ cursor n))
            n)
        0)))
  (morsel-reset! [_] (set! cursor 0)))

(defn obj-morsel-source [col]
  (let [tag    (col/-type-tag col)
        data   (if (#{:sym :str} tag)
                 (.data col)
                 (throw (IllegalArgumentException.
                         (str "ObjMorselSource requires :sym or :str column, got " tag))))
        offset (if (= :sym tag)
                 (.offset ^flatiron.column.SymColumn col)
                 (.offset ^flatiron.column.StrColumn col))]
    (ObjMorselSource. data (col/-len col) offset 0)))
