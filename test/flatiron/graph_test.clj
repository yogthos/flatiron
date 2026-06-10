(ns flatiron.graph-test
  "Specification tests for CSR graph operations."
  (:require [flatiron.graph :as g]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [clojure.test :as t]))

;; ════════════════════════════════════════════════════════════════════════
;; Graph construction and accessors
;; ════════════════════════════════════════════════════════════════════════

(t/deftest build-simple-graph
  (let [src-c (col/i64-column [0 1 2 0])
        dst-c (col/i64-column [1 2 3 2])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)]
    (t/is (= 4 (.n-nodes graph)))
    (t/is (= 4 (.n-edges graph)))
    ;; Degrees: node 0 → [1,2], node 1 → [2], node 2 → [3], node 3 → []
    (t/is (= 2 (g/degree graph 0)))
    (t/is (= 1 (g/degree graph 1)))
    (t/is (= 1 (g/degree graph 2)))
    (t/is (= 0 (g/degree graph 3)))
    ;; In-degrees
    (t/is (= 0 (g/in-degree graph 0)))
    (t/is (= 1 (g/in-degree graph 1)))   ;; 0→1
    (t/is (= 2 (g/in-degree graph 2)))   ;; 1→2, 0→2
    (t/is (= 1 (g/in-degree graph 3))))) ;; 2→3

(t/deftest build-graph-with-weights
  (let [src-c (col/i64-column [0 1])
        dst-c (col/i64-column [1 0])
        wgt-c (col/f64-column [2.5 3.5])
        table (tbl/table [:src :dst :weight] [src-c dst-c wgt-c])
        graph (g/build-graph table)]
    (t/is (= 2 (.n-nodes graph)))
    (t/is (some? (.weights graph)))
    (let [nbrs (g/neighbors graph 0)]
      (t/is (= 1 (count nbrs)))
      (t/is (= [1 2.5] (first nbrs))))))

(t/deftest neighbors-no-weights
  (let [src-c (col/i64-column [0 0])
        dst-c (col/i64-column [1 2])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        nbrs (g/neighbors graph 0)]
    (t/is (= 2 (count nbrs)))
    (t/is (= [1 1.0] (first nbrs)))))

;; ════════════════════════════════════════════════════════════════════════
;; PageRank
;; ════════════════════════════════════════════════════════════════════════

(t/deftest pagerank-converges
  "PageRank should produce non-zero scores that sum to approximately 1.0."
  (let [src-c (col/i64-column [0 1 2 0 1])
        dst-c (col/i64-column [1 2 0 2 0])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        result (g/pagerank graph :iters 30)]
    (t/is (= 3 (tbl/nrows result)))
    (t/is (some? (tbl/col result :node)))
    (t/is (some? (tbl/col result :rank)))
    ;; Check rank sum is close to 1.0
    (let [rc (tbl/col result :rank)
          total (loop [i 0 s 0.0]
                  (if (< i 3)
                    (recur (unchecked-inc i) (+ s (col/-get-double rc i)))
                    s))]
      (t/is (< 0.9 total 1.1) (str "Rank sum = " total)))))

(t/deftest pagerank-single-node
  "Single-node graph: rank = 1.0"
  (let [src-c (col/i64-column [0])
        dst-c (col/i64-column [0])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        result (g/pagerank graph :iters 10)]
    (t/is (= 1 (tbl/nrows result)))
    (t/is (= 1.0 (col/-get-double (tbl/col result :rank) 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; Connected components
;; ════════════════════════════════════════════════════════════════════════

(t/deftest connected-components-two-groups
  "Two disconnected subgraphs should produce two components."
  (let [src-c (col/i64-column [0 1 3])
        dst-c (col/i64-column [1 2 4])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        result (g/connected-components graph)]
    (t/is (= 5 (tbl/nrows result)))
    (let [cc (tbl/col result :component)]
      ;; Nodes 0,1,2 should share one component
      (t/is (= (col/-get-long cc 0) (col/-get-long cc 1)))
      (t/is (= (col/-get-long cc 0) (col/-get-long cc 2)))
      ;; Nodes 3,4 should share another component
      (t/is (= (col/-get-long cc 3) (col/-get-long cc 4)))
      ;; And the two groups should be different
      (t/is (not= (col/-get-long cc 0) (col/-get-long cc 3))))))

(t/deftest connected-components-single-node
  "Two nodes connected by one edge should be in the same component."
  (let [src-c (col/i64-column [0])
        dst-c (col/i64-column [1])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        result (g/connected-components graph)]
    (t/is (= 2 (tbl/nrows result)))
    (let [cc (tbl/col result :component)]
      ;; Edge 0→1 connects both in undirected sense
      (t/is (= (col/-get-long cc 0) (col/-get-long cc 1))))))

(t/deftest connected-components-fully-connected
  "Fully connected graph → all nodes same component"
  (let [src-c (col/i64-column [0 0 1 1 2 2])
        dst-c (col/i64-column [1 2 0 2 0 1])
        table (tbl/table [:src :dst] [src-c dst-c])
        graph (g/build-graph table)
        result (g/connected-components graph)]
    (let [cc (tbl/col result :component)
          comp (col/-get-long cc 0)]
      (t/is (every? #(= comp %) (for [i (range 3)] (col/-get-long cc i)))))))

;; ════════════════════════════════════════════════════════════════════════
;; Larger graph
;; ════════════════════════════════════════════════════════════════════════

(t/deftest build-large-graph
  "Build a graph with 10K edges — structural verification only."
  (let [n 10000
        src (col/i64-column (mapv #(long (rem % 100)) (range n)))
        dst (col/i64-column (mapv #(long (rem (+ % 1) 100)) (range n)))
        table (tbl/table [:src :dst] [src dst])
        graph (g/build-graph table)]
    (t/is (= 100 (.n-nodes graph)))
    (t/is (= n (.n-edges graph)))))

;; ════════════════════════════════════════════════════════════════════════
;; Graph weight alignment
;; ════════════════════════════════════════════════════════════════════════

(t/deftest graph-weights-align-with-targets
  (let [src (col/i64-column [0 0 0])
        dst (col/i64-column [2 1 3])
        w   (col/f64-column [20.0 10.0 30.0])
        gr  (g/graph src dst w)
        nbrs (into {} (g/neighbors gr 0))]
    (t/is (= 10.0 (nbrs 1)))
    (t/is (= 20.0 (nbrs 2)))
    (t/is (= 30.0 (nbrs 3)))))

;; ════════════════════════════════════════════════════════════════════════
;; BFS
;; ════════════════════════════════════════════════════════════════════════

(t/deftest bfs-distances
  (let [gr (g/graph (col/i64-column [0 1 2 0])
                    (col/i64-column [1 2 3 2]))
        r  (g/bfs gr 0)
        d  (tbl/col r :distance)]
    (t/is (= 0 (col/-get-long d 0)))
    (t/is (= 1 (col/-get-long d 1)))
    (t/is (= 1 (col/-get-long d 2)))
    (t/is (= 2 (col/-get-long d 3)))))

(t/deftest bfs-unreachable-is-null
  (let [gr (g/graph (col/i64-column [0]) (col/i64-column [1]) nil 4)
        r  (g/bfs gr 0)
        d  (tbl/col r :distance)]
    (t/is (= 0 (col/-get-long d 0)))
    (t/is (= 1 (col/-get-long d 1)))
    (t/is (nil? (col/-get-obj d 2)))
    (t/is (nil? (col/-get-obj d 3)))))

;; ════════════════════════════════════════════════════════════════════════
;; DFS
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dfs-visits-all-reachable
  (let [gr (g/graph (col/i64-column [0 1 2 0])
                    (col/i64-column [1 2 3 2]))
        r  (g/dfs gr 0)
        o  (tbl/col r :order)]
    (t/is (= #{0 1 2 3}
             (set (for [i (range 4)] (col/-get-long o i)))))
    (t/is (= 0 (col/-get-long o 0)))))

;; ════════════════════════════════════════════════════════════════════════
;; Dijkstra
;; ════════════════════════════════════════════════════════════════════════

(t/deftest dijkstra-shortest-paths
  (let [gr (g/graph (col/i64-column [0 1 0])
                    (col/i64-column [1 2 2])
                    (col/f64-column [1.0 1.0 5.0]))
        r  (g/dijkstra gr 0)
        d  (tbl/col r :distance)]
    (t/is (= 0.0 (col/-get-double d 0)))
    (t/is (= 1.0 (col/-get-double d 1)))
    (t/is (= 2.0 (col/-get-double d 2)))))

(t/deftest dijkstra-unreachable-is-null
  (let [gr (g/graph (col/i64-column [0]) (col/i64-column [1])
                    (col/f64-column [1.0]) 3)
        r  (g/dijkstra gr 0)
        d  (tbl/col r :distance)]
    (t/is (nil? (col/-get-obj d 2)))))

;; ════════════════════════════════════════════════════════════════════════
;; PageRank — positional vs kwargs
;; ════════════════════════════════════════════════════════════════════════

(t/deftest page-rank-positional-matches-kwargs
  (let [gr (g/graph (col/i64-column [0 1 2 0 1])
                    (col/i64-column [1 2 0 2 0]))
        r1 (g/page-rank gr 30 0.85)
        r2 (g/pagerank gr :iters 30 :damping 0.85)]
    (doseq [i (range 3)]
      (t/is (= (col/-get-double (tbl/col r1 :rank) i)
               (col/-get-double (tbl/col r2 :rank) i))))))
