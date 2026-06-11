(ns flatiron.rayforce-bench
  "Benchmark flatiron against the original C implementation (Rayforce).

   Generates one dataset, hands it to both engines, runs the same queries
   in each and prints a side-by-side table. Rayforce timings come from its
   Rayfall `timeit` builtin (milliseconds); flatiron timings are measured
   around the equivalent calls. Both sides get warmup runs; we report the
   minimum and median of 10 timed runs.

   Usage:
     clojure -M:bench -m flatiron.rayforce-bench [path-to-rayforce-binary]

   The binary defaults to ~/src/rayforce/rayforce (build with `make release`)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [flatiron.agg :as agg]
            [flatiron.column :as col]
            [flatiron.filter :as filt]
            [flatiron.group :as g]
            [flatiron.table :as tbl]))

(set! *warn-on-reflection* true)

(def ^:private ^:const N 1000000)
(def ^:private csv-path "/tmp/flatiron_rayforce_bench.csv")
(def ^:private rfl-path "/tmp/flatiron_rayforce_bench.rfl")

;; ── Data ──────────────────────────────────────────────────────────────
;; Deterministic, no RNG: Sym cycles 100 symbols, Id has ~100K distinct
;; values, Qty in [0,1000), Price in [0.0,100.0).

(defn- gen-table []
  (let [syms (object-array (map #(keyword (str "s" %)) (range 100)))
        sym-arr (object-array N)
        id-arr (long-array N)
        qty-arr (long-array N)
        price-arr (double-array N)]
    (dotimes [i N]
      (aset sym-arr i (aget syms (mod i 100)))
      (aset id-arr i (long (mod (* i 2654435761) 100000)))
      (aset qty-arr i (long (mod (* (inc i) 40503) 1000)))
      (aset price-arr i (/ (double (mod (* (inc i) 69069) 10000)) 100.0)))
    {:sym sym-arr :id id-arr :qty qty-arr :price price-arr}))

(defn- write-csv! [{:keys [^objects sym ^longs id ^longs qty ^doubles price]}]
  (with-open [w (io/writer csv-path)]
    (.write w "Sym,Id,Qty,Price\n")
    (dotimes [i N]
      (.write w (name ^clojure.lang.Keyword (aget sym i)))
      (.write w ",")
      (.write w (Long/toString (aget id i)))
      (.write w ",")
      (.write w (Long/toString (aget qty i)))
      (.write w ",")
      (.write w (Double/toString (aget price i)))
      (.write w "\n"))))

(defn- flatiron-table [{:keys [^objects sym ^longs id ^longs qty ^doubles price]}]
  (tbl/table [:Sym :Id :Qty :Price]
             [(flatiron.column.SymColumn. sym N 0 false)
              (col/i64-column-from-array id)
              (col/i64-column-from-array qty)
              (flatiron.column.F64Column. price N 0 false)]))

;; ── Timing ────────────────────────────────────────────────────────────

(defn- time-ms ^double [f]
  (let [t0 (System/nanoTime)]
    (f)
    (/ (- (System/nanoTime) t0) 1e6)))

(defn- run-timed
  "3 warmup + 10 timed runs (plus JIT warmup beforehand); [min median] ms."
  [f]
  (dotimes [_ 13] (f))
  (let [ts (sort (repeatedly 10 #(time-ms f)))]
    [(first ts) (nth ts 5)]))

;; ── Rayforce side ─────────────────────────────────────────────────────

(def ^:private rayfall-queries
  [["Q1 group-by Sym, sum"
    "(select {from: df total: (sum Qty) by: {Sym: Sym}})"]
   ["Q2 group-by Sym, sum+count+avg"
    "(select {from: df s: (sum Qty) c: (count Qty) a: (avg Qty) by: {Sym: Sym}})"]
   ["Q3 where Qty>500, group-by Sym sum"
    "(select {from: df total: (sum Qty) by: {Sym: Sym} where: (> Qty 500)})"]
   ["Q4 group-by Id (100K groups), sum"
    "(select {from: df total: (sum Qty) by: {Id: Id}})"]
   ["Q5 scalar sum(Qty)"
    "(select {from: df total: (sum Qty)})"]])

(defn- write-rfl! []
  (with-open [w (io/writer rfl-path)]
    (.write w (str "(set df (read-csv [SYMBOL I64 I64 F64] \"" csv-path "\"))\n"))
    (doseq [[label q] rayfall-queries
            :let [marker (first (str/split label #" "))]]
      (.write w (str "(count (map (fn [_] (count " q ")) (til 3)))\n"))
      (.write w (str "(println \"##" marker "\")\n"))
      (.write w (str "(map (fn [_] (println (timeit " q "))) (til 10))\n")))
    (.write w "(exit 0)\n")))

(defn- parse-rayforce-output
  "Pull the 10 timeit millisecond values printed after each ##Qn marker."
  [out]
  (let [lines (str/split-lines out)]
    (loop [ls lines, cur nil, acc {}]
      (if (empty? ls)
        acc
        (let [l (str/trim (first ls))]
          (cond
            (str/starts-with? l "##") (recur (rest ls) (subs l 2) acc)
            (and cur (re-matches #"\d+(\.\d+)?" l))
            (recur (rest ls) cur (update acc cur (fnil conj []) (Double/parseDouble l)))
            :else (recur (rest ls) cur acc)))))))

(defn- run-rayforce [binary]
  (let [{:keys [exit out err]} (sh/sh binary :in (slurp rfl-path))]
    (when-not (zero? exit)
      (throw (ex-info "rayforce run failed" {:exit exit :err err})))
    (into {}
          (map (fn [[marker ts]]
                 (let [ts (sort ts)]
                   [marker [(first ts) (nth ts (quot (count ts) 2))]])))
          (parse-rayforce-output out))))

;; ── Flatiron side ─────────────────────────────────────────────────────

(defn- flatiron-queries [table]
  (let [qty-col (tbl/col table :Qty)
        mask (delay (filt/scalar-pred qty-col :gt 500))]
    [["Q1" #(g/group-by table :keys [:Sym]
                        :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q1p" #(g/parallel-group-by table :keys [:Sym] :n-threads 8
                                  :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q2" #(g/group-by table :keys [:Sym]
                        :aggs [{:agg :sum :col :Qty :out :s}
                               {:agg :count :col :Qty :out :c}
                               {:agg :avg :col :Qty :out :a}])]
     ["Q2p" #(g/parallel-group-by table :keys [:Sym] :n-threads 8
                                  :aggs [{:agg :sum :col :Qty :out :s}
                                         {:agg :count :col :Qty :out :c}
                                         {:agg :avg :col :Qty :out :a}])]
     ["Q3" #(g/group-by table :keys [:Sym] :where @mask
                        :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q3p" #(g/parallel-group-by table :keys [:Sym] :where @mask :n-threads 8
                                  :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q4" #(g/group-by table :keys [:Id]
                        :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q4p" #(g/parallel-group-by table :keys [:Id] :n-threads 8
                                  :aggs [{:agg :sum :col :Qty :out :total}])]
     ["Q5" #(agg/i64-sum qty-col)]]))

;; ── Main ──────────────────────────────────────────────────────────────

(defn- fmt ^String [[mn md]]
  (if mn (format "%8.2f %8.2f" (double mn) (double md)) (format "%8s %8s" "-" "-")))

(defn -main [& [binary]]
  (let [binary (or binary (str (System/getProperty "user.home") "/src/rayforce/rayforce"))
        _ (when-not (.canExecute (io/file binary))
            (binding [*out* *err*]
              (println "rayforce binary not found/executable:" binary)
              (println "build it with: cd ~/src/rayforce && make release"))
            (System/exit 1))
        _ (println (format "Generating %,d rows..." N))
        data (gen-table)
        _ (write-csv! data)
        _ (write-rfl!)
        table (flatiron-table data)
        _ (println "Running rayforce (C)...")
        rf (run-rayforce binary)
        _ (println "Running flatiron (JVM)...")
        fl (into {} (map (fn [[label f]] [label (run-timed f)]))
                 (flatiron-queries table))]
    (println)
    (println (format "%-38s %17s %17s %17s" "" "rayforce (C)" "flatiron" "flatiron par8"))
    (println (format "%-38s %8s %8s %8s %8s %8s %8s"
                     (format "query (%,d rows)" N) "min" "median" "min" "median" "min" "median"))
    (println (apply str (repeat 92 "-")))
    (doseq [[label _] rayfall-queries
            :let [marker (first (str/split label #" "))]]
      (println (format "%-38s %s %s %s"
                       label
                       (fmt (get rf marker))
                       (fmt (get fl marker))
                       (fmt (get fl (str marker "p"))))))
    (println)
    (println "times in ms; rayforce timed via its timeit builtin, flatiron via System/nanoTime")
    (shutdown-agents)))
