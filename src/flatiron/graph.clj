(ns flatiron.graph
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl])
  (:import [flatiron.column I64Column F64Column]))

(defrecord Graph [^longs fwd-offsets ^longs fwd-targets
                  ^longs rev-offsets ^longs rev-targets
                  ^long n-nodes ^long n-edges ^doubles weights])

;; ── Build helpers ─────────────────────────────────────────────────────
(defn- build-csr-w
  "Build a CSR keyed by `key-arr` (one entry per edge). `tgt-arr` holds the
   adjacency target per edge, and `wt-arr` (optional) the matching weights.
   Targets and weights are scattered together so they stay aligned.
   Returns [offsets targets weights-or-nil]."
  [^longs key-arr ^longs tgt-arr ^doubles wt-arr n-edges n-nodes]
  (let [n-edges (long n-edges)
        n-nodes (long n-nodes)
        deg (long-array n-nodes)]
    (dotimes [i n-edges]
      (let [k (aget key-arr i)]
        (aset deg k (unchecked-inc (aget deg k)))))
    (let [off (long-array (unchecked-inc n-nodes))
          pos (long-array n-nodes)
          tgt (long-array n-edges)
          ow  (when wt-arr (double-array n-edges))]
      (loop [i 0 cum 0]
        (when (< i n-nodes)
          (aset off i cum) (aset pos i cum)
          (recur (unchecked-inc i) (unchecked-add cum (aget deg i)))))
      (aset off n-nodes n-edges)
      (dotimes [i n-edges]
        (let [k (aget key-arr i)
              p (aget pos k)]
          (aset tgt p (aget tgt-arr i))
          (when wt-arr (aset ^doubles ow p (aget wt-arr i)))
          (aset pos k (unchecked-inc p))))
      [off tgt ow])))

;; ── graph ─────────────────────────────────────────────────────────────
(defn graph
  "Build a graph from a source and a target I64 column (one edge per row).
   An optional F64 weight column gives per-edge weights. `n-nodes` overrides
   the inferred node count (otherwise it is max node id + 1)."
  ([src-col dst-col] (graph src-col dst-col nil nil))
  ([src-col dst-col weight-col] (graph src-col dst-col weight-col nil))
  ([src-col dst-col weight-col n-nodes]
   (let [^I64Column sc src-col
         ^I64Column dc dst-col
         n-edges (.len sc)
         sd (.data sc) so (.offset sc)
         dd (.data dc) do (.offset dc)
         src (long-array n-edges) dst (long-array n-edges)]
     (dotimes [i n-edges]
       (aset src i (aget sd (+ so i)))
       (aset dst i (aget dd (+ do i))))
     (let [nn (long (or n-nodes
                        (if (zero? n-edges)
                          0
                          (let [mx (volatile! 0)]
                            (dotimes [i n-edges] (vswap! mx max (aget src i) (aget dst i)))
                            (unchecked-inc @mx)))))
           wts (when weight-col
                 (let [^F64Column wc weight-col
                       wd (.data wc) wo (.offset wc)
                       out (double-array n-edges)]
                   (dotimes [i n-edges] (aset out i (aget wd (+ wo i))))
                   out))
           [fo ft fw] (build-csr-w src dst wts n-edges nn)
           [ro rt _]  (build-csr-w dst src nil n-edges nn)]
       (Graph. fo ft ro rt nn n-edges fw)))))

;; ── build-graph ───────────────────────────────────────────────────────
(defn build-graph
  "Build a graph from a table with :src and :dst columns (and an optional
   :weight column). See `graph` for the column-based constructor."
  [table & {:keys [n-nodes]}]
  (graph (tbl/col table :src) (tbl/col table :dst) (tbl/col table :weight) n-nodes))

(defn degree [^Graph g ^long n]
  (let [^longs off (.fwd-offsets g)]
    (unchecked-subtract (aget off (unchecked-inc n)) (aget off n))))

(defn in-degree [^Graph g ^long n]
  (let [^longs off (.rev-offsets g)]
    (unchecked-subtract (aget off (unchecked-inc n)) (aget off n))))

(defn neighbors [^Graph g ^long n]
  (let [^longs off (.fwd-offsets g) ^longs tgt (.fwd-targets g)
        wts (.weights g) start (aget off n) end (aget off (unchecked-inc n))]
    (for [i (range start end)]
      [(aget tgt i) (if wts (aget wts i) 1.0)])))

(defn pagerank [^Graph g & {:keys [damping iters] :or {damping 0.85 iters 20}}]
  (let [nn (.n-nodes g)
        ^longs fo (.fwd-offsets g) ^longs ro (.rev-offsets g) ^longs rt (.rev-targets g)
        rank (double-array nn) rank2 (double-array nn)
        init (/ 1.0 (double nn))]
    (dotimes [i nn] (aset rank i init))
    (dotimes [_iter iters]
      (let [ds (loop [u 0 ds 0.0]
                 (if (< u nn)
                   (if (= (aget fo (unchecked-inc u)) (aget fo u))
                     (recur (unchecked-inc u) (+ ds (aget rank u)))
                     (recur (unchecked-inc u) ds))
                   ds))
            base (+ (/ (- 1.0 damping) (double nn)) (* damping (/ ds (double nn))))]
        (dotimes [v nn]
          (let [sum (loop [j (aget ro v) s 0.0]
                      (if (< j (aget ro (unchecked-inc v)))
                        (let [u (aget rt j)
                              od (unchecked-subtract (aget fo (unchecked-inc u)) (aget fo u))]
                          (if (pos? od)
                            (recur (unchecked-inc j) (+ s (/ (aget rank u) (double od))))
                            (recur (unchecked-inc j) s)))
                        s))]
            (aset rank2 v (+ base (* damping sum)))))
        (dotimes [i nn] (aset rank i (aget rank2 i)))))
    (let [nc (long-array nn) rc (double-array nn)]
      (dotimes [i nn] (aset nc i i) (aset rc i (aget rank i)))
      (tbl/table [:node :rank] [(I64Column. nc nn 0 false) (F64Column. rc nn 0 false)]))))

(defn page-rank
  "PageRank with positional arguments, matching the README:
     (page-rank graph)             ;; 20 iterations, damping 0.85
     (page-rank graph iters)
     (page-rank graph iters damping)"
  ([g] (pagerank g))
  ([g iters] (pagerank g :iters iters))
  ([g iters damping] (pagerank g :iters iters :damping damping)))

;; ── Traversal ──────────────────────────────────────────────────────────
(defn- i64-result-table
  "Build a [:node value-name] table from a long-array of per-node values.
   Entries equal to `missing` become null."
  [value-name ^longs vals ^long nn ^long missing]
  (let [nc (long-array nn)
        vc (long-array nn)
        hn (volatile! false)]
    (dotimes [i nn]
      (aset nc i i)
      (let [v (aget vals i)]
        (if (== v missing)
          (do (aset vc i col/NULL_I64) (vreset! hn true))
          (aset vc i v))))
    (tbl/table [:node value-name]
               [(I64Column. nc nn 0 false)
                (I64Column. vc nn 0 @hn)])))

(defn bfs
  "Breadth-first search from `start`. Returns a table of [:node :distance]
   where distance is the number of hops from `start`; unreachable nodes are
   null."
  [^Graph g ^long start]
  (let [nn (.n-nodes g)
        ^longs fo (.fwd-offsets g)
        ^longs ft (.fwd-targets g)
        dist (long-array nn)]
    (java.util.Arrays/fill dist (long -1))
    (when (and (>= start 0) (< start nn))
      (aset dist start 0)
      (let [q (java.util.ArrayDeque.)]
        (.add q (Long/valueOf start))
        (loop []
          (when-not (.isEmpty q)
            (let [u  (long (.poll q))
                  du (aget dist u)]
              (loop [j (aget fo u)]
                (when (< j (aget fo (unchecked-inc u)))
                  (let [v (aget ft j)]
                    (when (== -1 (aget dist v))
                      (aset dist v (unchecked-inc du))
                      (.add q (Long/valueOf v))))
                  (recur (unchecked-inc j)))))
            (recur)))))
    (i64-result-table :distance dist nn -1)))

(defn dfs
  "Iterative depth-first search from `start`. Returns a table of [:node :order]
   giving the preorder visitation index of each reachable node; unvisited
   nodes are null."
  [^Graph g ^long start]
  (let [nn (.n-nodes g)
        ^longs fo (.fwd-offsets g)
        ^longs ft (.fwd-targets g)
        order   (long-array nn)
        visited (boolean-array nn)
        cnt     (long-array 1)]
    (java.util.Arrays/fill order (long -1))
    (when (and (>= start 0) (< start nn))
      (let [stack (java.util.ArrayDeque.)]
        (.push stack (Long/valueOf start))
        (loop []
          (when-not (.isEmpty stack)
            (let [u (long (.pop stack))]
              (when-not (aget visited u)
                (aset visited u true)
                (aset order u (aget cnt 0))
                (aset cnt 0 (unchecked-inc (aget cnt 0)))
                ;; Push neighbors in reverse so the lowest-id neighbor is
                ;; visited first.
                (loop [j (unchecked-dec (aget fo (unchecked-inc u)))]
                  (when (>= j (aget fo u))
                    (let [v (aget ft j)]
                      (when-not (aget visited v)
                        (.push stack (Long/valueOf v))))
                    (recur (unchecked-dec j))))))
            (recur)))))
    (i64-result-table :order order nn -1)))

(defn dijkstra
  "Single-source shortest paths from `start` over edge weights. Edges without
   weights count as weight 1.0. Returns a table of [:node :distance]; nodes
   that are unreachable have null distance."
  [^Graph g ^long start]
  (let [nn (.n-nodes g)
        ^longs fo (.fwd-offsets g)
        ^longs ft (.fwd-targets g)
        ^doubles wts (.weights g)
        dist (double-array nn)]
    (java.util.Arrays/fill dist Double/POSITIVE_INFINITY)
    (when (and (>= start 0) (< start nn))
      (aset dist start 0.0)
      (let [pq (java.util.PriorityQueue.
                 (reify java.util.Comparator
                   (compare [_ a b]
                     (Double/compare (double (nth a 0)) (double (nth b 0))))))]
        (.add pq [0.0 start])
        (loop []
          (when-not (.isEmpty pq)
            (let [top (.poll pq)
                  d   (double (nth top 0))
                  u   (long (nth top 1))]
              ;; Skip stale heap entries left behind by a relaxation.
              (when (<= d (aget dist u))
                (loop [j (aget fo u)]
                  (when (< j (aget fo (unchecked-inc u)))
                    (let [v  (aget ft j)
                          w  (if wts (aget wts j) 1.0)
                          nd (+ d w)]
                      (when (< nd (aget dist v))
                        (aset dist v nd)
                        (.add pq [nd v])))
                    (recur (unchecked-inc j))))))
            (recur)))))
    (let [nc (long-array nn)
          dc (double-array nn)
          hn (volatile! false)]
      (dotimes [i nn]
        (aset nc i i)
        (let [d (aget dist i)]
          (if (Double/isInfinite d)
            (do (aset dc i Double/NaN) (vreset! hn true))
            (aset dc i d))))
      (tbl/table [:node :distance]
                 [(I64Column. nc nn 0 false)
                  (F64Column. dc nn 0 @hn)]))))

(defn connected-components [^Graph g]
  (let [nn (.n-nodes g)
        ^longs fo (.fwd-offsets g) ^longs ft (.fwd-targets g)
        ^longs ro (.rev-offsets g) ^longs rt (.rev-targets g)
        label (long-array nn)]
    (dotimes [i nn] (aset label i i))
    (loop [changed true]
      (when changed
        (let [ch (volatile! false)]
          (dotimes [v nn]
            (let [ml (volatile! (aget label v))]
              (loop [j (aget fo v)]
                (when (< j (aget fo (unchecked-inc v)))
                  (vswap! ml min (aget label (aget ft j)))
                  (recur (unchecked-inc j))))
              (loop [j (aget ro v)]
                (when (< j (aget ro (unchecked-inc v)))
                  (vswap! ml min (aget label (aget rt j)))
                  (recur (unchecked-inc j))))
              (when (< @ml (aget label v))
                (aset label v @ml) (vreset! ch true))))
          (recur @ch))))
    (let [nc (long-array nn) cc (long-array nn)]
      (dotimes [i nn] (aset nc i i) (aset cc i (aget label i)))
      (tbl/table [:node :component] [(I64Column. nc nn 0 false) (I64Column. cc nn 0 false)]))))
