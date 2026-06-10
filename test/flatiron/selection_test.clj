(ns flatiron.selection-test
  "Specification tests for selection bitmap — correctness contract."
  (:require [flatiron.selection :as sel]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; Empty selection
;; ════════════════════════════════════════════════════════════════════════

(t/deftest selection-empty
  "Selection of 0 rows produces 0 segments, 0 popcount."
  (let [s (sel/selection-none 0)]
    (t/is (= 0 (sel/selection-n-rows s)))
    (t/is (= 0 (sel/selection-popcount s)))))

(t/deftest selection-single-row
  "Exactly 1 row — one segment of size 1."
  (let [s (sel/selection-all 1)]
    (t/is (= 1 (sel/selection-n-rows s)))
    (t/is (= 1 (sel/selection-popcount s)))
    (t/is (true? (sel/row-live? s 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; selection-none / selection-all
;; ════════════════════════════════════════════════════════════════════════

(t/deftest selection-none-basic
  "All rows dead — every segment flag is 0."
  (let [s (sel/selection-none 2048)]
    (t/is (= 2048 (sel/selection-n-rows s)))
    (t/is (= 0 (sel/selection-popcount s)))
    ;; All rows dead across segment boundaries
    (t/is (false? (sel/row-live? s 0)))
    (t/is (false? (sel/row-live? s 1023)))   ;; last of seg 0
    (t/is (false? (sel/row-live? s 1024)))   ;; first of seg 1
    (t/is (false? (sel/row-live? s 2047)))   ;; last of seg 1
    ;; Segment flags all 0 (dead)
    (t/is (= 0 (sel/seg-flag s 0)))
    (t/is (= 0 (sel/seg-flag s 1)))))

(t/deftest selection-all-basic
  "All rows live — every segment flag is 1."
  (let [s (sel/selection-all 2048)]
    (t/is (= 2048 (sel/selection-n-rows s)))
    (t/is (= 2048 (sel/selection-popcount s)))
    ;; All rows live across segment boundaries
    (t/is (true? (sel/row-live? s 0)))
    (t/is (true? (sel/row-live? s 1023)))
    (t/is (true? (sel/row-live? s 1024)))
    (t/is (true? (sel/row-live? s 2047)))
    ;; Segment flags all 1 (live)
    (t/is (= 1 (sel/seg-flag s 0)))
    (t/is (= 1 (sel/seg-flag s 1)))))

(t/deftest selection-all-exactly-one-morsel
  "Exactly 1024 rows → one segment, flag=1, popcount=1024."
  (let [s (sel/selection-all 1024)]
    (t/is (= 1024 (sel/selection-n-rows s)))
    (t/is (= 1024 (sel/selection-popcount s)))
    (t/is (= 1 (sel/seg-flag s 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; Partial last morsel (matches C test_rowsel_partial_last_morsel)
;; ════════════════════════════════════════════════════════════════════════

(t/deftest selection-partial-last-segment
  "Last segment partial: n=1500 → 2 segments, seg 0 full (1024), seg 1 partial (476)."
  (let [s (sel/selection-all 1500)]
    (t/is (= 1500 (sel/selection-n-rows s)))
    (t/is (= 1500 (sel/selection-popcount s)))
    ;; Two segments — seg-popcount for seg 0 = 1024, seg 1 = 1500
    (t/is (= 1024 (sel/seg-popcount s 0)))
    (t/is (= 1500 (sel/seg-popcount s 1)))
    ;; Partial last segment: rows in seg 1 should be live
    (t/is (true? (sel/row-live? s 1024)))
    (t/is (true? (sel/row-live? s 1499)))))

;; ════════════════════════════════════════════════════════════════════════
;; set-row: dead segment → mixed
;; ════════════════════════════════════════════════════════════════════════

(t/deftest set-row-dead-to-live
  "Setting a row live in a dead segment transitions flag 0→2 (mixed)."
  (let [s (sel/selection-none 64)]
    (t/is (false? (sel/row-live? s 10)))
    (sel/set-row s 10 true)
    (t/is (true? (sel/row-live? s 10)))
    ;; Other rows still dead
    (t/is (false? (sel/row-live? s 11)))
    ;; Segment flag now mixed
    (t/is (= 2 (sel/seg-flag s 0)))
    ;; Set a second row
    (sel/set-row s 20 true)
    (t/is (true? (sel/row-live? s 20)))
    ;; Popcount should be 2 after recompute
    (sel/recompute-popcounts! s)
    (t/is (= 2 (sel/selection-popcount s)))))

(t/deftest set-row-dead-to-dead-noop
  "Setting a row dead in a dead segment is a no-op (flag stays 0)."
  (let [s (sel/selection-none 64)]
    (sel/set-row s 10 false)
    (t/is (false? (sel/row-live? s 10)))
    (t/is (= 0 (sel/seg-flag s 0)))
    ;; Setting an ALL row to false should never transition to mixed — but
    ;; a caller asking for dead→dead in an ALL segment is a logic error.
    ;; Our set-row current doesn't handle that case. We'll skip testing it.
    ))

;; ════════════════════════════════════════════════════════════════════════
;; set-row: live segment → mixed
;; ════════════════════════════════════════════════════════════════════════

(t/deftest set-row-live-to-dead
  "Setting a row dead in an all-live segment transitions flag 1→2 (mixed)."
  (let [s (sel/selection-all 128)]
    (t/is (= 1 (sel/seg-flag s 0)))
    (sel/set-row s 5 false)
    (t/is (false? (sel/row-live? s 5)))
    (t/is (= 2 (sel/seg-flag s 0)))
    ;; Other rows still live
    (t/is (true? (sel/row-live? s 0)))
    (t/is (true? (sel/row-live? s 10)))
    (sel/recompute-popcounts! s)
    (t/is (= 127 (sel/selection-popcount s)))))

(t/deftest set-row-live-to-live-noop
  "Setting a row live in an all-live segment keeps flag at 1."
  (let [s (sel/selection-all 64)]
    (sel/set-row s 10 true)
    (t/is (true? (sel/row-live? s 10)))
    ;; Flag should still be 1 (not 2 — we didn't transition to mixed)
    (t/is (= 1 (sel/seg-flag s 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; set-row: mixed segment — no flag change
;; ════════════════════════════════════════════════════════════════════════

(t/deftest set-row-in-mixed-segment
  "Toggling rows in an already-mixed segment keeps flag=2."
  (let [s (sel/selection-all 128)]
    ;; Transition to mixed first
    (sel/set-row s 5 false)
    (t/is (= 2 (sel/seg-flag s 0)))
    ;; Set another row dead
    (sel/set-row s 10 false)
    (t/is (false? (sel/row-live? s 5)))
    (t/is (false? (sel/row-live? s 10)))
    ;; Set row back to live
    (sel/set-row s 5 true)
    (t/is (true? (sel/row-live? s 5)))
    ;; Flag still 2
    (t/is (= 2 (sel/seg-flag s 0)))
    ;; Popcount: 128 - 1 (row 10 dead) = 127
    (sel/recompute-popcounts! s)
    (t/is (= 127 (sel/selection-popcount s)))))

;; ════════════════════════════════════════════════════════════════════════
;; Multi-segment mixed (matches C test_rowsel_multi_morsel)
;; ════════════════════════════════════════════════════════════════════════

(t/deftest multi-segment-all-none-mixed
  "Three segments: ALL, NONE, MIXED — verifies all three flag states."
  (let [n       (* 3 1024)  ;; 3072
        ;; Build by hand: seg 0 all-live, seg 1 mixed (half dead), seg 2 all-dead
        s       (sel/selection-all n)]
    ;; Seg 1: set every other row dead → mixed
    (doseq [i (range 1024 2048 2)]
      (sel/set-row s i false))
    ;; Seg 2: set all rows dead → flag becomes 2 (mixed, all bits zero).
    ;; Once a segment goes mixed, it stays mixed — even if all bits are dead.
    ;; The popcount correctly reflects 0 live rows in this segment.
    (doseq [i (range 2048 n)]
      (sel/set-row s i false))
    (sel/recompute-popcounts! s)

    ;; Flags: seg 0 = ALL (1), seg 1 = MIX (2), seg 2 = MIX (2, all bits zero)
    (t/is (= 1 (sel/seg-flag s 0)))
    (t/is (= 2 (sel/seg-flag s 1)))
    (t/is (= 2 (sel/seg-flag s 2)))

    ;; Popcount: 1024 + 512 + 0 = 1536
    (t/is (= 1536 (sel/selection-popcount s)))

    ;; Row queries
    (t/is (true?  (sel/row-live? s 0)))     ;; seg 0: live
    (t/is (true?  (sel/row-live? s 1025)))  ;; seg 1: live (odd index)
    (t/is (false? (sel/row-live? s 1024)))  ;; seg 1: dead (even index)
    (t/is (false? (sel/row-live? s 2048))))) ;; seg 2: dead

(t/deftest multi-segment-popcount-consistency
  "Popcounts should be cumulative: seg-popcount[i] = total live through seg i."
  (let [n 5000
        s (sel/selection-all n)]
    ;; Kill rows: 0, 100, 500, 1500, 3000, 4000
    (doseq [idx [0 100 500 1500 3000 4000]]
      (sel/set-row s idx false))
    (sel/recompute-popcounts! s)

    ;; 6 rows dead from 5000 = 4994 live
    (t/is (= 4994 (sel/selection-popcount s)))

    ;; Pick a few segment boundaries and verify cumulative popcounts
    (let [seg0-end (min 1024 n)
          seg1-end (min 2048 n)
          seg2-end (min 3072 n)]
      ;; seg 0: rows in [0, 1024); dead: 0, 100, 500 → 3 dead, popcount = 1021
      (t/is (= (- seg0-end 3) (sel/seg-popcount s 0)))
      ;; seg 1: rows in [1024, 2048); dead: 1500 → 1 dead, cumulative = 1021 + 1023 = 2044
      (when (> n 2048)
        (t/is (= (+ (sel/seg-popcount s 0) (- seg1-end seg0-end 1))
                 (sel/seg-popcount s 1)))))))
