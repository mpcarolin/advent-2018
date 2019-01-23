(defproject advent "0.1.0-SNAPSHOT"
  :description "Advent of Code Solutions"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/tools.trace "0.7.10"]]}
             :uberjar {:aot :all}}
  :main ^:skip-aot advent.core
  :target-path "target/%s")
