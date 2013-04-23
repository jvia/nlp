(ns snld.parser-test
  (:use midje.sweet snld.parser))

(def the1 (complex :right :N :NP))
(def the2 (complex :right :N (complex :right (complex :left :NP :S) :S)))
(def doctor1 :N)
(def doctor2 (complex :right (complex :left :N :N) :N))
(def patient1 :N)
(def patient2 (complex :right (complex :left :N :N) :N))
(def sent1 (complex :right :PP (complex :left :NP :S)))
(def sent2 (complex :right :PP (complex :left :N :N)))
(def arrived (complex :left :NP :S))
(def for_w (complex :right :NP :PP))
(def lex
  {;; determiners
   :the     [(complex :right :N :NP) (complex :right :N (complex :right (complex :left :NP :S) :S))]
   ;; Nouns
   :doctor  [:N (complex :right (complex :left :N :N) :N)]
   :flowers [:N (complex :right (complex :left :N :N) :N)]
   :patient [:N (complex :right (complex :left :N :N) :N)]
   ;; Verbs
   :sent    [(complex :right :PP (complex :left :NP :S))
             (complex :right :PP (complex :left :N :N))]
   :arrived [(complex :left :NP :S)]
   ;; Preposistions
   :for     [(complex :right :NP :PP)]})

(defn same-stacks? [stacks1 stacks2]
  (empty? (filter false? (map (fn [y] (some (fn [x] (= y x)) stacks1)) stacks2))))

(fact "Application requires a functor type which is applied to another lexical item."
      (appli the1 doctor1) => :NP
      (appli the1 doctor2) => nil
      (appli the2 (appli doctor2 (complex :left :N :N))) => (complex :right (complex :left :NP :S) :S))


(fact "Composition works like it does in math."
      (compose sent1 for_w) => (complex :right :NP :PP)
      (compose sent2 for_w) => (complex :right :NP :PP))

(facts "About shift"
       (fact "Shifting when there are no stacks creates a new stack
              for each lexical entry with the associcated key."
             (shift :the nil lex)     => [[the1] [the2]]
             (shift :arrived nil lex) => [[arrived]])
       (facts "Shifting will create m * n number of stacks."
              (let [stacks [[the1] [the2]]]
                (shift :doctor stacks lex) => [[doctor1 the1] [doctor1 the2]
                                               [doctor2 the2] [doctor2 the2]])))

