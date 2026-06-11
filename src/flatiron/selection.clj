(ns flatiron.selection)

(set! *warn-on-reflection* true)

;; ── Morsel constants ──────────────────────────────────────────────────
(def ^:const morsel-elems 1024)
(def ^:const bits-per-word 64)

;; ── Selection bitmap (three-level: segment flags + popcount + bits) ───
(deftype Selection [^bytes seg-flags      ;; 0=all-dead, 1=all-live, 2=mixed — byte array
                    ^longs seg-popcnt     ;; cumulative popcount per segment
                    ^longs bits           ;; actual bits for mixed segments
                    ^long  n-rows])

;; ── Segment helpers ───────────────────────────────────────────────────
(defn- n-segments ^long [^long n]
  (long (/ (+ n morsel-elems -1) morsel-elems)))

(defn- n-bitmap-words ^long [^long n]
  (long (/ (+ n bits-per-word -1) bits-per-word)))

(defn- seg-start ^long [^long seg-idx]
  (* seg-idx morsel-elems))

(defn- seg-end ^long [^long seg-idx ^long n]
  (min n (+ (seg-start seg-idx) morsel-elems)))

;; ── Core accessors ────────────────────────────────────────────────────
(defn selection-n-rows ^long [^Selection sel]
  (.n-rows sel))

(defn seg-flag ^long [^Selection sel ^long seg-idx]
  (long (aget ^bytes (.seg-flags sel) seg-idx)))

(defn seg-popcount ^long [^Selection sel ^long seg-idx]
  (aget ^longs (.seg-popcnt sel) seg-idx))

(defn selection-popcount ^long [^Selection sel]
  (let [s (n-segments (.n-rows sel))]
    (if (zero? s) 0 (seg-popcount sel (dec s)))))

(defn row-live?
  "Check if a single row is live."
  [^Selection sel ^long idx]
  (let [seg-idx  (quot idx morsel-elems)
        flag     (aget ^bytes (.seg-flags sel) seg-idx)]
    (case flag
      0 false
      1 true
      2 (let [word-idx (quot idx bits-per-word)
              bit-idx  (rem idx bits-per-word)]
          (not (zero? (bit-and (aget ^longs (.bits sel) word-idx)
                               (bit-shift-left 1 bit-idx))))))))

;; ── Mutation (for building filtering) ──────────────────────────────────
(defn set-row
  "Set a single row as live (bit=1) or dead (bit=0). Converts segment flag to mixed if needed."
  [^Selection sel ^long idx live?]
  (let [seg-idx   (quot idx morsel-elems)
        word-idx  (quot idx bits-per-word)
        bit-idx   (rem idx bits-per-word)
        bit-mask  (bit-shift-left 1 bit-idx)
        ^bytes seg-flags (.seg-flags sel)
        ^longs bits      (.bits sel)
        curr-flag (aget seg-flags seg-idx)]
    (when (zero? curr-flag)
      (if live?
        ;; First write to dead segment: go to mixed, init bits to 0, set this bit
        (do
          (aset seg-flags seg-idx (byte 2))
          ;; Init all bits in segment to 0 then set this row's bit
          (let [seg-wstart (quot (seg-start seg-idx) bits-per-word)
                seg-wend   (quot (dec (seg-end seg-idx (.n-rows sel))) bits-per-word)]
            (loop [w seg-wstart]
              (when (<= w seg-wend)
                (aset bits w 0)
                (recur (unchecked-inc w)))))
          (let [w (aget bits word-idx)]
            (aset bits word-idx (bit-or w bit-mask))))
        ;; Setting a row dead in an all-dead segment: nothing to do
        ))
    (when (= 1 curr-flag)
      ;; Was all-live, transitioning to mixed
      (when (not live?)
        (aset seg-flags seg-idx (byte 2))
        ;; Set all rows in segment to 1, then clear this specific bit
        (let [seg-wstart (quot (seg-start seg-idx) bits-per-word)
              seg-wend   (quot (dec (seg-end seg-idx (.n-rows sel))) bits-per-word)]
          (loop [w seg-wstart]
            (when (<= w seg-wend)
              (aset bits w -1)
              (recur (unchecked-inc w))))
          (let [w (aget bits word-idx)]
            (aset bits word-idx (bit-and-not w bit-mask))))))
    (when (= 2 curr-flag)
      ;; Already mixed: just update the bit
      (let [w (aget bits word-idx)]
        (aset bits word-idx
              (if live?
                (bit-or  w bit-mask)
                (bit-and-not w bit-mask)))))))

;; ── Recompute popcounts after mutations ───────────────────────────────
(defn recompute-popcounts!
  "Rebuild cumulative popcounts from segment flags and bits."
  [^Selection sel]
  (let [^bytes seg-flags (.seg-flags sel)
        ^longs pop       (.seg-popcnt sel)
        ^longs bits      (.bits sel)
        n-segs'   (n-segments (.n-rows sel))
        n         (.n-rows sel)]
    (loop [seg-idx 0, cum 0]
      (when (< seg-idx n-segs')
        (let [s-end (seg-end seg-idx n)
              s-n   (- s-end (seg-start seg-idx))
              cnt   (long
                     (case (aget seg-flags seg-idx)
                       0 0
                       1 s-n
                       2 (let [wstart (quot (seg-start seg-idx) bits-per-word)
                               wend   (quot (dec s-end) bits-per-word)]
                          (loop [wi wstart, sum 0]
                            (if (<= wi wend)
                              (recur (unchecked-inc wi)
                                     (+ sum (Long/bitCount (aget bits wi))))
                              sum)))))]
          (aset pop seg-idx (long (+ cum cnt)))
          (recur (unchecked-inc seg-idx) (+ cum cnt)))))))

;; ── Factory constructors ───────────────────────────────────────────────
(defn selection-none
  "Create a selection bitmap for n rows, none selected (all dead)."
  [^long n]
  (let [n-segs' (n-segments n)
        n-words (n-bitmap-words n)]
    (Selection. (byte-array n-segs') (long-array n-segs') (long-array n-words) n)))

(defn selection-all
  "Create a selection bitmap for n rows, all selected (all live)."
  [^long n]
  (let [n-segs'    (n-segments n)
        n-words    (n-bitmap-words n)
        seg-flags  (byte-array n-segs')
        seg-pop    (long-array n-segs')]
    (dotimes [i n-segs']
      (aset seg-flags i (byte 1)))
    ;; Popcounts: cumulative
    (dotimes [i n-segs']
      (aset seg-pop i (long (- (seg-end i n) (seg-start i)))))
    (loop [i 1]
      (when (< i n-segs')
        (aset seg-pop i (long (+ (aget seg-pop (dec i)) (aget seg-pop i))))
        (recur (unchecked-inc i))))
    (Selection. seg-flags seg-pop (long-array n-words) n)))
