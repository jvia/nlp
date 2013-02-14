(defproject nlp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.4.1"]
                 [net.sourceforge.jtransforms/jtransforms "2.4.0"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :javac-options ["-target" "1.7" "-source" "1.7" "-Xlint:-options"]
  :profiles {:dev {:plugins [[lein-midje "2.0.4"]]}})
