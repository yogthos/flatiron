(ns flatiron.reflection-test
  "Gate: every source namespace must compile without reflection or
   auto-boxing warnings. Compiles them in a fresh JVM so the check is
   order-independent. CI enforces the same invariant by grepping the
   test-runner output."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn- src-namespaces []
  (->> (file-seq (io/file "src/flatiron"))
       (map #(.getName ^java.io.File %))
       (filter #(str/ends-with? % ".clj"))
       (map #(str "flatiron." (str/replace (subs % 0 (- (count %) 4)) "_" "-")))
       sort))

(t/deftest ^:slow no-reflection-in-source-namespaces
  (let [{:keys [exit err]}
        (sh/sh "clojure" "-M" "-e"
               (str "(binding [*warn-on-reflection* true]"
                    "  (doseq [n '[" (str/join " " (src-namespaces)) "]] (require n)))"))]
    (t/is (zero? exit) err)
    (t/is (not (re-find #"Reflection warning" err)) err)
    (t/is (not (re-find #"Auto-boxing|not matching primitive" err)) err)))
