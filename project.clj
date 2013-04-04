(defproject nlp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.4.1"]
                 [net.sourceforge.jtransforms/jtransforms "2.4.0"]
                 [edu.cmu.sphinx/sphinx4 "1.0-beta6"]
                 [com.taoensso/timbre "1.5.2"]
                 [com.google.guava/guava "14.0.1"]
                 [org.apache.commons/commons-math3 "3.1.1"]]
  :repositories [["nexus" {:url "http://repository.ow2.org/nexus/content/groups/public"}]
                 ["sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"}]]
  :source-paths ["src/main/clojure" "jars/*"]
  :java-source-paths ["src/main/java" "jars/*"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :profiles {:dev {:plugins [[lein-midje "2.0.4"]]}})
