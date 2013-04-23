;; Variables are in the form :?x
;; Terms are in the form of :x
;; Abstraction are in the form of {:var :?x :body [:?x]}
(ns ^{:doc "A simple lambda calculus interpreter."
      :author "Jeremiah Via"}
  snld.lambda
  (:use [clojure.core.match :only (match)]))


(defn lambda
  "Create a lambda data structure. Use this to represent "
  [var & body]
  (if (not (coll? body))
    {:var var :body [body]}
    {:var var :body body}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^{:private true}
  I (lambda :?x :?x))
(def ^{:private true}
  EYE (lambda :?y :?y))
(def ^{:private true}
  one (lambda :?f , (lambda :?x , :?f :?x)))
(def ^{:private true}
  juan (lambda :?g [(lambda :?x , :?g :?x)]))
(def ^{:private true}
  two (lambda :?f [(lambda :?x , :?f [:?f :?x])]))
(def ^{:private true}
  succ (lambda :?n , (lambda :?f , (lambda :?x , :?f [:?n :?f :?x]))))
(def ^{:private true}
  free1 (lambda :?x , :?y))
(def ^{:private true}
  free2 (lambda :?x , :?x (lambda :?z , :?x :?z)))


(defn variable?
  "We assume variables starts with :?"
  [term]
  (let [[f s _] (str term)]
    (and (= f \:) (= s \?))))


(defn ground?
  "Ground terms that are not variables are not eligible to replaced by
  substitution. This isn't part of lambda calculus but it is useful
  for the parser."
  [term]
  (let [[f s _] (str term)]
    (and (= f \:) (not (= s \?)))))


(defn abstraction?
  "Check if the given lambda-term is an abstraction."
  [lambda-term]
  (and (map? lambda-term)
       (contains? lambda-term :var)
       (contains? lambda-term :body)))

(defn term?
  "Using keywords as term."
  [term]
  (or (variable? term)
      (ground? term)
      (abstraction? term)))

(defn reducible?
  "Can the two lambda terms be reduced."
  [alpha beta]
  (and (abstraction? alpha) (term? beta)))


(defn free?
  "Determine if the given var is free in this term. "
  [var context]
  (not (some #(= % var) context)))


(defn flatten-term
  "Walk over a lambda abstraction, putting all the ground & variables
  into a list."
  [term]
  (cond (ground? term)   [term]
        (variable? term) [term]
        (abstraction? term) (into [(:var term)] (flatten-term (:body term)))
        :else (mapcat flatten-term term)))


(defn alpha-equiv?
  "Determine if two lambda abstractions are alpha-equivalent."
  [A B]
  (if (not (and (abstraction? A) (abstraction? B))) false
      (let [{varA :var} A {varB :var} B
            alpha=  (fn [termA termB]
                      (or (= termA termB)
                          (and (= termA varA) (= termB varB))))]
        (loop [[termA & restA] (flatten-term A)
               [termB & restB] (flatten-term B)]
          (cond (and (nil? termA) (nil? termB)) true
                (not (alpha= termA termB)) false
                :else (recur restA restB))))))



(defn substitute
  ([var val terms]
     (substitute var val terms nil))
  ([var val terms bounded]
     (letfn [(substitutible? [term]
               (and (= var term) (not (some #(= % var) bounded))))]
       (for [term terms]
         (cond (substitutible? term) val
               (abstraction? term) (lambda (:var term)
                                           (substitute var val (:body term)
                                                       (cons (:var term) bounded)))
               :else term)))))


(defn beta-reduce
  "Assume we have alpha = (λx. x λx.x) and beta is z.

   The basic process of beta-reduction is as follows:
   1. Remove the abstraction, making the variable bound by the abstraction free.
   2. Replace all free instances of the variable with beta through substitution
   3. "
  [alpha beta]
  (when (reducible? alpha beta)
    (let [{var :var body :body} alpha]
      (substitute var beta body))))


(defn lambda->str [lambda]
  (letfn [(key->str [key] (subs (str key) 1))]
    (cond (abstraction? lambda)
          (str "(λ" (key->str (:var lambda)) "." (lambda->str (:body lambda)) ")")
          (term? lambda) (key->str lambda)
          :else (apply str (interpose " " (map lambda->str lambda))))))