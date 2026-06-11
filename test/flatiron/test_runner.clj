(ns flatiron.test-runner
  "CI entry point: compiles all source namespaces with *warn-on-reflection*
   enabled (warnings land on stderr where CI greps them — see Selmer's CI
   for the pattern), then runs every test namespace. Exits non-zero on any
   failure or error."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn- ns-syms [dir prefix]
  (->> (file-seq (io/file dir))
       (map #(.getName ^java.io.File %))
       (filter #(str/ends-with? % ".clj"))
       (map #(symbol (str prefix (str/replace (subs % 0 (- (count %) 4)) "_" "-"))))
       sort))

(defn -main [& _]
  ;; Compile sources with reflection warnings on. Test namespaces are
  ;; compiled outside the binding — only src regressions should fail CI.
  (binding [*warn-on-reflection* true]
    (doseq [n (ns-syms "src/flatiron" "flatiron.")]
      (require n)))
  (let [test-nses (->> (ns-syms "test/flatiron" "flatiron.")
                       (remove #{'flatiron.bench-test 'flatiron.test-runner}))]
    (apply require test-nses)
    (let [{:keys [fail error]} (apply t/run-tests test-nses)]
      (System/exit (if (zero? (+ (long fail) (long error))) 0 1)))))
