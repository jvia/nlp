(ns snld.parser-test
  (:use midje.sweet snld.parser))

(def lex
  {;; determiners
   :the     [{:syn (complex :right :N :NP)  :sem (lambda :x [:def :x])}
             {:syn (complex :right :N (complex :right (complex :left :NP :S) :S))   :sem (lambda :P (lambda :Q [:Q :det :P]))}]
   ;; Nouns
   :doctor  [{:syn :N :sem (lambda :x [:doctor :x])}
             {:syn (complex :right (complex :left :N :N) :N) :sem (lambda :Q (lambda :x [:Q :doctor :x]))}]
   :flowers [{:syn :N :sem (lambda :x [:flowers :x])}
             {:syn (complex :right (complex :left :N :N) :N) :sem (lambda :Q (lambda :x [:Q :flowers :x]))}]
   :patient [{:syn :N :sem (lambda :x [:patient :x])}
             {:syn (complex :right (complex :left :N :N) :N) :sem (lambda :Q (lambda :x [:Q :patient :x]))}]
   ;; Verbs
   :sent    [{:syn (complex :right :PP (complex :left :NP :S)) :sem (lambda :x (lambda :y [:summon :x :y]))}
             {:syn (complex :right :PP (complex :left :N :N)) :sem (lambda :z (lambda :P (lambda :y [:P :y :and :send :z :y :sb])))}]
   :arrived [{:syn (complex :left :NP :S) :sem (lambda :x [:arrive :x])}]
   ;; Preporistions
   :for     [{:syn (complex :right :NP :PP) :sem (lambda :x [:x])}]})


(facts "about CCG"
       (facts "about forward application"
              (fact (appli (-> lex :the first) (-> lex :doctor first))   => {:syn :NP :sem [:def (lambda :x [:doctor :x])]}))
       (facts "about backward application")
       (facts "about forward composition")
       (facts "about backward composition")
       (fact "right composition"
             (compose {:syn (complex :right (complex :left :NP :S) :S)}
                      {:syn (complex :right :NP (complex :left :NP :S))}) => (complex :right :NP :S))
       (fact "left composition"
             (compose {:syn (complex :left :Z :Y)} {:syn (complex :left :Y :X)}) => (complex :left :Z :X)))



