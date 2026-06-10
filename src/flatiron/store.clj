(ns flatiron.store
  "Columnar binary file format — save and load tables to/from disk."
  (:require [flatiron.column :as col]
            [flatiron.table :as tbl])
  (:import [java.io DataOutputStream DataInputStream
            BufferedOutputStream BufferedInputStream
            FileOutputStream FileInputStream File]
           [flatiron.column I64Column F64Column BoolColumn SymColumn StrColumn]))

(defn- column-tag [c]
  (case (col/-type-tag c)
    :i64 1 :f64 2 :bool 3 :sym 4 :str 5
    (throw (IllegalArgumentException. (str "Bad type: " (col/-type-tag c))))))

;; ── Save ──────────────────────────────────────────────────────────────

(def save-table
  (fn [table dir]
    (let [d (File. (str dir))
          schema (.schema table)
          nc (count (.schema table))]
      (.mkdirs d)
      (when (pos? nc)
        (let [nr (col/-len (aget (.columns table) 0))]
          (loop [c 0]
            (when (< c nc)
              (let [col (aget (.columns table) c)
                    col-name (name (aget schema c))
                    stream (FileOutputStream. (File. d (str "col_" col-name)))
                    buf (BufferedOutputStream. stream)
                    out (DataOutputStream. buf)]
                (try
                  (.writeInt out (column-tag col))
                  (.writeLong out (.longValue (Long/valueOf nr)))
                  ;; Persist the null flag so it round-trips. For i64/f64 the
                  ;; sentinel value (NULL_I64 / NaN) is stored as-is and this
                  ;; flag restores null awareness. For sym/str each value gets
                  ;; a presence byte so nil is distinguished from "".
                  (.writeBoolean out (boolean (col/-has-nulls? col)))
                  (loop [i 0]
                    (when (< i nr)
                      (case (col/-type-tag col)
                        :i64  (.writeLong out (col/-get-long col i))
                        :f64  (.writeDouble out (col/-get-double col i))
                        :bool (.writeByte out (byte (if (col/-get-obj col i) 1 0)))
                        :sym  (let [kw (col/-get-obj col i)]
                                (if (nil? kw)
                                  (.writeBoolean out false)
                                  (do (.writeBoolean out true)
                                      (.writeUTF out (name kw)))))
                        :str  (let [s (col/-get-obj col i)]
                                (if (nil? s)
                                  (.writeBoolean out false)
                                  (do (.writeBoolean out true)
                                      (.writeUTF out (str s))))))
                      (recur (unchecked-inc i))))
                  (finally (.close out)))
                (recur (unchecked-inc c))))))
        (let [col-names (mapv #(aget schema %) (range nc))]
          (spit (str dir "/_meta.edn")
                (pr-str {:schema col-names
                         :nrows (if (pos? nc) (col/-len (aget (.columns table) 0)) 0)}))))
      dir)))

;; ── Load ──────────────────────────────────────────────────────────────

(def ^:private read-column
  (fn [f]
    (let [in (DataInputStream. (BufferedInputStream. (FileInputStream. f)))
          tag (.readInt in)
          nr (.intValue (Long/valueOf (.readLong in)))
          has-nulls (.readBoolean in)]
      (try
        (if (pos? nr)
          (case tag
            1 (let [data (long-array nr)]
                (loop [i 0]
                  (when (< i nr)
                    (aset data i (.readLong in))
                    (recur (unchecked-inc i))))
                (I64Column. data nr 0 has-nulls))
            2 (let [data (double-array nr)]
                (loop [i 0]
                  (when (< i nr)
                    (aset data i (.readDouble in))
                    (recur (unchecked-inc i))))
                (F64Column. data nr 0 has-nulls))
            3 (let [data (byte-array nr)]
                (loop [i 0]
                  (when (< i nr)
                    (aset data i (.readByte in))
                    (recur (unchecked-inc i))))
                (BoolColumn. data nr 0))
            4 (let [data (object-array nr)]
                (loop [i 0]
                  (when (< i nr)
                    (when (.readBoolean in)
                      (aset data i (keyword (.readUTF in))))
                    (recur (unchecked-inc i))))
                (SymColumn. data nr 0 has-nulls))
            5 (let [data (object-array nr)]
                (loop [i 0]
                  (when (< i nr)
                    (when (.readBoolean in)
                      (aset data i (.readUTF in)))
                    (recur (unchecked-inc i))))
                (StrColumn. data nr 0 has-nulls))
            (throw (IllegalArgumentException. (str "Unknown tag: " tag))))
          ;; nr=0 → empty column of appropriate type
          (case tag
            1 (I64Column. (long-array 0) 0 0 false)
            2 (F64Column. (double-array 0) 0 0 false)
            3 (BoolColumn. (byte-array 0) 0 0)
            4 (SymColumn. (object-array 0) 0 0 false)
            5 (StrColumn. (object-array 0) 0 0 false)
            (throw (IllegalArgumentException. (str "Unknown tag: " tag)))))
        (finally (.close in))))))

(def load-table
  (fn [dir]
    (let [d (str dir)
          meta (read-string (slurp (str d "/_meta.edn")))
          schema (:schema meta)
          nc (count schema)]
      (if (zero? nc)
        (tbl/table [] [])
        (let [cols
              (loop [c 0 acc (transient [])]
                (if (< c nc)
                  (let [col-name (name (nth schema c))
                        f (File. d (str "col_" col-name))]
                    (if (.exists f)
                      (recur (unchecked-inc c) (conj! acc (read-column f)))
                      (throw (Exception. (str "Missing column file: " f)))))
                  (persistent! acc)))]
          (tbl/table (vec schema) (vec cols)))))))
