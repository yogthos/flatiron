(ns flatiron.io
  "I/O bridge — CSV parsing into Tables with type inference,
   and CSV writing from Tables."
  (:require [flatiron.csv :as csv]
            [flatiron.column :as col]
            [flatiron.table :as tbl]
            [clojure.string :as str]))

;; ════════════════════════════════════════════════════════════════════════
;; Type inference
;; ════════════════════════════════════════════════════════════════════════

(def ^:const ^long SAMPLE-ROWS 100)

(defn- ^boolean blank? [^String s]
  (or (nil? s) (str/blank? s)))

(defn- ^boolean bool-str? [^String s]
  (contains? #{"true" "TRUE" "false" "FALSE"} s))

(def ^:private nan-tokens   #{"NaN" "nan"})
(def ^:private posinf-tokens #{"Inf" "inf" "+Inf" "+inf" "Infinity" "+Infinity"})
(def ^:private neginf-tokens #{"-Inf" "-inf" "-Infinity"})

(defn- ^boolean nan-str? [^String s]
  (or (contains? nan-tokens s)
      (contains? posinf-tokens s)
      (contains? neginf-tokens s)))

(defn- ^boolean int-str? [^String s]
  (re-matches #"^[+-]?\d+$" s))

(defn- ^boolean float-str? [^String s]
  (re-matches #"^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$" s))

(defn- ^boolean date-str? [^String s]
  (and (= 10 (count s))
       (re-matches #"^\d{4}-\d{2}-\d{2}$" s)))

(defn- ^boolean datetime-str? [^String s]
  (and (>= (count s) 19)
       (re-matches #"^\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}$"
                   (subs s 0 19))))

(defn- ^boolean time-str? [^String s]
  (and (>= (count s) 8)
       (re-matches #"^\d{2}:\d{2}:\d{2}(\.\d+)?$" s)))

;; ── Type labels ───────────────────────────────────────────────────────
(def ^:private type-order {:null 0 :bool 1 :i64 2 :f64 3 :str 4})

(defn- promote-type [a b]
  (let [oa (type-order a 99) ob (type-order b 99)]
    (if (< oa ob) b a)))

(defn- infer-cell-type [^String s]
  (cond
    (blank? s)      :null
    (nan-str? s)    :f64
    (bool-str? s)   :bool
    (int-str? s)    :i64
    (float-str? s)  :f64
    (date-str? s)   :str   ;; dates stored as strings for now
    (datetime-str? s) :str
    (time-str? s)   :str
    :else           :str))

(defn- infer-column-type [^long n-cols rows]
  (let [types (object-array (repeat n-cols :null))]
    (doseq [row rows]
      (dotimes [c (min n-cols (count row))]
        (let [cell (nth row c)
              inferred (infer-cell-type cell)]
          (when (not= :null inferred)
            (aset types c (promote-type (aget types c) inferred))))))
    (mapv #(if (= :null %) :str %) (vec types))))

;; ════════════════════════════════════════════════════════════════════════
;; Cell parsing
;; ════════════════════════════════════════════════════════════════════════

(defn- parse-i64-cell [^String s]
  (if (blank? s) nil (Long/parseLong s)))

(defn- parse-f64-cell [^String s]
  (if (blank? s)
    nil
    ;; Normalize the float spellings that infer-cell-type accepts but that
    ;; Double/parseDouble rejects (e.g. "Inf", "nan", "+Inf").
    (cond
      (contains? nan-tokens s)    Double/NaN
      (contains? posinf-tokens s) Double/POSITIVE_INFINITY
      (contains? neginf-tokens s) Double/NEGATIVE_INFINITY
      :else                       (Double/parseDouble s))))

(defn- parse-bool-cell [^String s]
  (if (blank? s) nil (contains? #{"true" "TRUE"} s)))

(defn- parse-str-cell [^String s]
  (if (blank? s) nil s))

;; ════════════════════════════════════════════════════════════════════════
;; CSV → Table
;; ════════════════════════════════════════════════════════════════════════

(defn read-csv
  "Read a CSV string or Reader into a flatiron Table.
   
   Options:
     :header?    — first row is column names (default true)
     :types      — map of column-index → :i64|:f64|:bool|:str (overrides inference)
     :delimiter  — field separator (default \\,)
     :quote-char — quote character (default \\\")
     :strict     — strict parsing mode (default false)
     :sample     — number of rows to sample for type inference (default 100)"
  ([csv-str]
   (read-csv csv-str {:header? true}))
  ([csv-str {:keys [header? types delimiter quote-char strict sample]
             :or {header? true strict false sample SAMPLE-ROWS}
             :as opts}]
   (let [rows    (csv/parse-csv csv-str (merge {:strict strict} (select-keys opts [:delimiter :quote-char])))
         rows    (doall rows)
         header  (if header?
                   (mapv keyword (first rows))
                   (let [n (apply max (map count rows))]
                     (mapv #(keyword (str "col" %)) (range n))))
         data    (if header? (rest rows) rows)
         n-cols  (count header)
         n-rows  (count data)
         ;; Type inference from sample
         sample-rows (take sample data)
         col-types   (or types (infer-column-type n-cols sample-rows))]
     ;; Build columns
     (let [columns (for [c (range n-cols)]
                     (let [t (get col-types c :str)
                           vals (for [row data] (nth row c nil))]
                       (case t
                         :i64  (col/i64-column  (map parse-i64-cell vals))
                         :f64  (col/f64-column  (map parse-f64-cell vals))
                         :bool (col/bool-column (map parse-bool-cell vals))
                         :str  (col/str-column  (map parse-str-cell vals))
                         (col/str-column (map parse-str-cell vals)))))]
       (tbl/table (vec header) (vec columns))))))

;; ════════════════════════════════════════════════════════════════════════
;; Table → CSV
;; ════════════════════════════════════════════════════════════════════════

(defn write-csv
  "Write a flatiron Table as a CSV string.
   
   Options:
     :delimiter  — field separator (default \\,)
     :quote-char — quote character (default \\\")
     :end-of-line — line ending (default \\n)"
  [table & {:as opts}]
  (let [n-cols (tbl/ncols table)
        n-rows (tbl/nrows table)
        header (for [i (range n-cols)] (name (tbl/col-name table i)))
        rows   (for [r (range n-rows)]
                 (for [c (range n-cols)]
                    (let [col (tbl/col-by-idx table c)
                          v (col/-get-obj col r)
                        s (cond
                            (nil? v) ""
                            (keyword? v) (name v)
                            :else (str v))]
                      s)))]
    (csv/write-csv (cons header rows) opts)))
