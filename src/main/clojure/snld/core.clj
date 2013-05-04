(ns snld.core
  (:require [snld.speech :as speech]
            [snld.parser :as parser])
  (:gen-class :main true))

(defn -main
  "Run a demonstration of each component."
    [& args]
    (println "+------------------------------------------------+")
    (println "|              HMM Speech Recognizer             |")
    (println "+------------------------------------------------+")   
    (speech/gordon)
    (println "\n\n")
    (println "+------------------------------------------------+")
    (println "|          Ruthless Reduce-First CCG Parser      |")
    (println "+------------------------------------------------+")
    (parser/gordon)
    (System/exit 0))


