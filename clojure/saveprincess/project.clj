(defproject saveprincess "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot saveprincess.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
