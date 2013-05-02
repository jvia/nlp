(ns snld.core
  (:require [snld.speech   :as speech]
            [snld.parser   :as parser]
            [snld.dialogue :as dialogue]))


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
    (parser/gordon))


