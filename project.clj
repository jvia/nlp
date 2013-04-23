(defproject nlp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.4.1"]
                 [org.clojure/core.logic "0.8.3"]
                 [org.clojure/core.unify "0.5.5"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [net.sourceforge.jtransforms/jtransforms "2.4.0"]
                 [edu.cmu.sphinx/sphinx4 "1.0-beta6"]
                 [com.taoensso/timbre "1.5.2"]
                 [midje "1.5.1"]]
  :repositories [["nexus" {:url "http://repository.ow2.org/nexus/content/groups/public"}]
                 ["sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"}]]
  :source-paths ["src/main/clojure" "jars/*"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :profiles {:dev {:plugins [[lein-midje "2.0.4"]]}})
