(ns flatiron.types
  "Logical types for numeric columns.

   A logical type is an order-preserving bijection between a domain object and a
   primitive long (or double). The column stores the encoded primitive in its
   backing array, so every numeric kernel — comparison, sort, group-by, hash,
   min/max — runs unchanged on the raw values. Only the boundaries (building a
   column, reading a value back, persisting) consult the codec.

   A codec is a map:

     {:physical :i64            ;; backing physical type, :i64 or :f64
      :class    LocalDate       ;; Java class the codec produces
      :encode   (fn ^long [x])  ;; object -> primitive
      :decode   (fn [^long v])} ;; primitive -> object

   Built-in logical types (all :i64-backed):

     :date         java.time.LocalDate      epoch day
     :instant      java.time.Instant        epoch milli
     :datetime     java.time.LocalDateTime  epoch milli at UTC
     :date-millis  java.util.Date           getTime epoch milli (#inst literal)
     :duration     java.time.Duration       nanos"
  (:import [java.time LocalDate Instant LocalDateTime Duration ZoneOffset]
           [java.util Date]))

(set! *warn-on-reflection* true)

;; ── Registry ──────────────────────────────────────────────────────────
(defonce ^:private registry (atom {}))

(defn register-type!
  "Register (or override) a logical type. `codec` is a map with :physical,
   :class, :encode and :decode. Returns the logical tag."
  [logical-tag codec]
  (swap! registry assoc logical-tag codec)
  logical-tag)

(defn lookup
  "Return the codec for a logical tag, or nil if unregistered."
  [logical-tag]
  (get @registry logical-tag))

(defn codec
  "Return the codec for a logical tag, throwing if unregistered."
  [logical-tag]
  (or (lookup logical-tag)
      (throw (IllegalArgumentException.
              (str "Unknown logical type: " logical-tag)))))

(defn physical
  "Physical backing type (:i64 / :f64) for a logical tag."
  [logical-tag]
  (:physical (codec logical-tag)))

(defn encode
  "Encode object `x` to the physical primitive representation for `logical-tag`.
   Returns a long for :i64-backed types and a double for :f64-backed types;
   callers cast to the primitive they need."
  [logical-tag x]
  ((:encode (codec logical-tag)) x))

(defn decode
  "Decode the physical primitive `v` back to the domain object for
   `logical-tag`. `v` is a long for :i64-backed types and a double for
   :f64-backed types; the codec's :decode is hinted for the matching primitive."
  [logical-tag v]
  ((:decode (codec logical-tag)) v))

;; ── Built-in codecs ─────────────────────────────────────────────────────

(register-type! :date
  {:physical :i64
   :class    LocalDate
   :encode   (fn ^long [^LocalDate d] (.toEpochDay d))
   :decode   (fn [^long v] (LocalDate/ofEpochDay v))})

(register-type! :instant
  {:physical :i64
   :class    Instant
   :encode   (fn ^long [^Instant i] (.toEpochMilli i))
   :decode   (fn [^long v] (Instant/ofEpochMilli v))})

(register-type! :datetime
  {:physical :i64
   :class    LocalDateTime
   :encode   (fn ^long [^LocalDateTime dt]
               (.toEpochMilli (.toInstant dt ZoneOffset/UTC)))
   :decode   (fn [^long v]
               (LocalDateTime/ofInstant (Instant/ofEpochMilli v) ZoneOffset/UTC))})

(register-type! :date-millis
  {:physical :i64
   :class    Date
   :encode   (fn ^long [^Date d] (.getTime d))
   :decode   (fn [^long v] (Date. v))})

(register-type! :duration
  {:physical :i64
   :class    Duration
   :encode   (fn ^long [^Duration d] (.toNanos d))
   :decode   (fn [^long v] (Duration/ofNanos v))})
