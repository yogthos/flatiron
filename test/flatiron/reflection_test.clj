(ns flatiron.reflection-test
  "Gate: hot-path namespaces must compile without reflection or auto-boxing
   warnings. Compiles them in a fresh JVM so the check is order-independent."
  (:require [clojure.java.shell :as sh]
            [clojure.test :as t]))

(def ^:private hot-namespaces
  "flatiron.column flatiron.morsel flatiron.hash flatiron.table flatiron.agg flatiron.group flatiron.filter")

(t/deftest ^:slow no-reflection-in-hot-namespaces
  (let [{:keys [exit err]}
        (sh/sh "clojure" "-M" "-e"
               (str "(binding [*warn-on-reflection* true]"
                    "  (doseq [n '[" hot-namespaces "]] (require n)))"))]
    (t/is (zero? exit) err)
    (t/is (not (re-find #"Reflection warning" err)) err)
    (t/is (not (re-find #"Auto-boxing|not matching primitive" err)) err)))
