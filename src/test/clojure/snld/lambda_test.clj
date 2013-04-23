(ns snld.lambda-test
  (:use snld.lambda midje.sweet))


(def I (lambda :x :x))
(def one (lambda :f [(lambda :x [:f :x])]))
(def two (lambda :f [(lambda :x [:f [:f :x]])]))
(def succ (lambda :n [(lambda :f [(lambda :x [:f [:n :f :x]])])]))

(def free1 (lambda :x , :y))
(def free2 (lambda :x , :x (lambda :z , :x :z)))


(fact "lambda function creates simple λ data structure"
      (lambda :x , :def :x)                   => {:var :x :body [:def :x]}
      (lambda :x , :x)                        => {:var :x :body [:x]}
      (lambda :x , (lambda :y , :pred :x :y)) => {:var :x :body [{:var :y :body [:pred :x :y]}]})


(fact "A lambda term is the basic unit. Simply some variable. We use keywords as terms."
      (term? :x)  => true
      (term? 'x)  => false
      (term? one) => true
      (term? 3)   => false)


(fact "An abstraction is a lambda term with a functional symbol"
      (abstraction? :x)                    => false
      (abstraction? (lambda :x [:det :x])) => true
      (abstraction? one)                   => true
      (abstraction? two)                   => true
      (abstraction? succ)                  => true)


(fact "We may ony manipulate free-variables under and abstraction."
      (let [context [:x :y]]
        (free? :x context) => false
        (free? :y context) => false
        (free? :z context) => true))


(fact "Not all terms can be beta-reduced"
      (reducible? I I)  => true
      (reducible? I :x) => true
      (reducible? :x I) => false)


(fact "String representations are much easier to read"
      (lambda->str (lambda :x :x))                    => "(λx.x)" 
      (lambda->str (lambda :x [:x (lambda :x [:x])])) => "(λx.x (λx.x))")


(fact "Substitution replaces all free occurrences of a variabe V in
      expression E with expression R"
      (substitute :x free1 :P) => free1)


(fact "Beta reduction applies an abstraction alpha to a term beta"
      (beta-reduce (lambda :x [:x]) :y)                  => [:y]
      (beta-reduce (lambda :x [(lambda :y [:y])]) :a)    => [(lambda :y [:y])]
      (beta-reduce (lambda :x [:x (lambda :x [:x])]) :Q) => [:Q (lambda :x [:x])]
      (beta-reduce succ one)                             => two
      (beta-reduce I I)                                  => I)

