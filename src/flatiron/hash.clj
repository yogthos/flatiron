(ns flatiron.hash
  "Fast wyhash-based hashing — ported from src/ops/hash.h.

   Based on wyhash final version 4.2 by Wang Yi.
   Deterministic (seed=0) for repeatable hashing within a process."
  (:refer-clojure :exclude [hash-combine]))

(set! *warn-on-reflection* true)

;; ── Secret constants (from wyhash final4.2) ────────────────────────────
;; Force to primitive long — hex literals with high bit set overflow to BigInt
(def ^:const wyp0 (unchecked-long 0x2d358dccaa6c78a5))
(def ^:const wyp1 (unchecked-long 0x8bb84b93962eacc9))
(def ^:const wyp2 (unchecked-long 0x4b33a62ed433d4a3))
(def ^:const wyp3 (unchecked-long 0x4d5a2da51de1aa47))

;; ── 128-bit multiply ───────────────────────────────────────────────────
;; Math/unsignedMultiplyHigh (JDK 18+, intrinsified) gives the high 64 bits
;; of the unsigned product; unchecked-multiply gives the low 64 bits.

(defn wymum
  "128-bit multiply: returns [lo hi] as a vector of two longs.
   (A * B) → [(low 64 bits) (high 64 bits)]"
  ^longs [^long a ^long b]
  (doto (long-array 2)
    (aset 0 (unchecked-multiply a b))
    (aset 1 (Math/unsignedMultiplyHigh a b))))

(defn wymix
  "Mix two 64-bit values via multiply-then-xor. Returns uint64 as long."
  ^long [^long a ^long b]
  (bit-xor (unchecked-multiply a b)
           (Math/unsignedMultiplyHigh a b)))

;; ── Hash functions ─────────────────────────────────────────────────────

(defn hash-i64
  "Hash a 64-bit integer. Fast two-round mixing.
   Matches ray_hash_i64 in hash.h."
  ^long [^long val]
  (let [A  (bit-xor val wyp0)
        B  (bit-xor val wyp1)
        lo (unchecked-multiply A B)
        hi (Math/unsignedMultiplyHigh A B)]
    (wymix (bit-xor lo wyp0)
           (bit-xor hi wyp1))))

(defn hash-f64
  "Hash a 64-bit float by its bit pattern.
   Normalizes -0.0 to +0.0 so they hash identically.
   Matches ray_hash_f64 in hash.h."
  ^long [^double val]
  (let [v    (if (== val 0.0) 0.0 val)  ;; normalize -0.0 → +0.0
        bits (Double/doubleToLongBits v)
        A    (bit-xor bits wyp0)
        B    (bit-xor bits wyp1)
        lo   (unchecked-multiply A B)
        hi   (Math/unsignedMultiplyHigh A B)]
    (wymix (bit-xor lo wyp0)
           (bit-xor hi wyp1))))

(defn hash-combine
  "Combine two hash values into one. Order-dependent:
   (hash-combine a b) != (hash-combine b a).
   Matches ray_hash_combine in hash.h."
  ^long [^long h1 ^long h2]
  (let [A  (bit-xor h1 wyp0)
        B  (bit-xor h2 wyp1)
        lo (unchecked-multiply A B)
        hi (Math/unsignedMultiplyHigh A B)]
    (wymix (bit-xor lo wyp0)
           (bit-xor hi wyp1))))

;; ── Multi-key hashing ──────────────────────────────────────────────────

(defn hash-row-i64
  "Hash a row across one or more I64 key columns.
   Returns combined uint64 hash as long."
  ^long [^longs key-bufs  ;; flat array of [col0-val, col1-val, ...] per row
         ^long row-idx
         ^long n-keys]
  (loop [k 0, h 0]
    (if (< k n-keys)
      (let [kh (hash-i64 (aget key-bufs (+ (* row-idx n-keys) k)))]
        (recur (unchecked-inc k)
               (if (zero? k) kh (hash-combine h kh))))
      h)))

;; ── Hash table helpers ─────────────────────────────────────────────────

(defn next-power-of-two
  "Smallest power of two >= n."
  ^long [^long n]
  (if (<= n 1)
    1
    (let [n' (dec n)]
      (loop [v n']
        (let [v' (bit-or v (unsigned-bit-shift-right v 1))
              v'' (bit-or v' (unsigned-bit-shift-right v' 2))
              v3 (bit-or v'' (unsigned-bit-shift-right v'' 4))
              v4 (bit-or v3 (unsigned-bit-shift-right v3 8))
              v5 (bit-or v4 (unsigned-bit-shift-right v4 16))
              v6 (bit-or v5 (unsigned-bit-shift-right v5 32))]
          (unchecked-inc v6))))))

(defn ht-capacity
  "Hash table capacity: smallest power of two >= 2 * n-rows, minimum 64."
  ^long [^long n-rows]
  (max 64 (next-power-of-two (* 2 n-rows))))

;; ── HT sentinel ────────────────────────────────────────────────────────
(def ^:const HT-EMPTY -1)
