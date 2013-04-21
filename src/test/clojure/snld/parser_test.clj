(ns snld.parser-test
  (:use midje.sweet snld.parser))


(facts "about CCG"
       (fact "right composition"
             (compose (complex :right (complex :left :NP :S) :S)
                      (complex :right :NP (complex :left :NP :S))) => (complex :right :NP :S))
       (fact "left composition"
             (compose (complex :left :Z :Y) (complex :left :Y :X)) => (complex :left :Z :X)))


(facts "about application")

(facts "about type-raising")


(facts "about lambda-calculus"
       (fact "lambda function creates simple λ data structure"
             (lambda :x [:def :x]) => {:var :x :body [:def :x]}
             (lambda :x (lambda :y [:pred :x :y])) => {:var :x :body {:var :y :body [:pred :x :y]}})
       (fact "A lambda term is the basic unit. Simply some variable. We use keywords as terms."
             (term? :x) => true
             (term? 'x) => false
             (term? (lambda :x [:det :x])) => true)
       (fact "An abstraction is a lambda term with a functional symbol"
             (abstraction? :x)  => false
             (abstraction? (lambda :x [:det :x])) => true)
       (fact "Beta reduction applies an abstraction alpha to a term beta"
             (beta-reduce (lambda :x [:x]) :y) => [:y]
             (beta-reduce (lambda :x [(lambda :y [:y])]) :a) => [(lambda :y [:y])]
             (beta-reduce (lambda :x [:x (lambda :x [:x])]) :Q) => [:Q (lambda :x [:x])])
       (fact "String representations are much easier to read"
             (lambda->str (lambda :x [:x]))  => "(λx.x)" 
             (lambda->str (lambda :x [:x (lambda :x [:x])])) => "(λx.x (λx.x))" ))
