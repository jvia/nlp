(ns snld.parser-test
  (:use [clojure test]
        [snld parser]))

;; Right composition
(is (= (complex :right :NP :S)
       (compose (complex :right (complex :left :NP :S) :S)
                (complex :right :NP (complex :left :NP :S)))))

;; Left composition
(is (= (complex :left :Z :X)
       (compose (complex :left :Z :Y)
                (complex :left :Y :X))))
