;;; Assignment 2
;; Write an incremental parser that can parse English language
;; sentences.  Feel free to be as creative as you would like to be in
;; devising the parser (there are no requirements for a particular
;; parsing algorithm other than that the parser be incremental and
;; take one word at a time).  You are allowed to use whatever
;; supporting software you'd like to use short of using an existing
;; parser.  You will have access to at least two corpora for training.
;; When you write the parser, allowing for dialogue-based and
;; context-based biasing of the parse (e.g., prepositional attachment)
;; and demonstrate how this bias changes the parsing result.
(ns ^{:doc "An incremental parser."
      :author "Jeremiah Via <jeremiah.via@gmail.com>"}
  snld.parser)

(def ^:dynamic *parse*)


(defn complex [dir take yield]
  {:dir dir  :take take :yield yield})

(defn atom? [token]
  (= (type token) clojure.lang.Keyword))

(defn complex? [token]
  (or
   (= (type token) clojure.lang.PersistentHashMap)
   (= (type token) clojure.lang.PersistentArrayMap)))

(def mappings
  {:ADJ (complex :left  :N :N)
   :DET (complex :right :N :NP)
   :NP :NP})

(def lexicon
  {;; Determiners
   :the    (complex :right :N :NP)
   ;; Adjectives
   :bad    (complex :right :N :N)
   ;; Verbs
   :loves  (complex :right :NP (complex :left :NP :S))
   :made   (complex :right :NP (complex :left :NP :S))
   :bit    (complex :right :NP (complex :left :NP :S))
   ;; Pronoun
   :that   (complex :right :N :NP)
   ;; Nouns
   :boy    :N
   :mess   :N
   :dog    :N
   ;; Proper nouns
   :andie  :NP
   :stevie :NP
   :john   :NP})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application
(defn- appli-right?
  "Determine if alpha can be right-applied to beta. This require that
   alpha is a complex type and whose argument type matches beta's
   type."
  [alpha beta]
  (and (complex? alpha)
       (= (:dir alpha) :right)
       (= (:take alpha) beta)))


(defn- appli-left?
  "Determine if beta can be left-applied to alpha. This require that
   beta is a complex type and whose argument type matches alpha's
   type."
  [alpha beta]
  (and (complex? beta)
       (= (:dir beta) :left)
       (= (:take beta) alpha)))


(defn appli
  "Apply a lexical item with a functor type to an argument with an
  appropriate type."
  [alpha beta]
  (cond (appli-left? alpha beta)  (:yield beta)
        (appli-right? alpha beta) (:yield alpha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition 
(defn- compose-right?
  "Determine if right composition can occur. This requires: 
   1) alpha and beta are functor types
   2) both are right applied
   3) beta yields the argument alpha accepts."
  [alpha beta]
  (and (complex? alpha) (complex? beta)
       (= :right (:dir alpha) (:dir beta))
       (= (:take alpha) (:yield beta))))


(defn- compose-left?
    "Determine if left composition can occur. This requires: 
     1) alpha and beta are functor types
     2) both are left applied
     3) alpha yields the argument beta accepts."
  [alpha beta]
  (and (complex? alpha) (complex? beta)
       (= :left (:dir alpha) (:dir beta))
       (= (:take beta) (:yield alpha))))


(defn compose
  "Compose two functors together, either using left or right composition."
  [alpha beta]
  (cond (compose-right? alpha beta) (complex :right (:take beta) (:yield alpha))
        (compose-left? alpha beta)  (complex :left (:take alpha) (:yield beta))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Raise

(defn type-raise
  "Performs right type-raising (succifient for incremental,
  left-branching derivation?)"
  [alpha]
  (complex :right (complex :left alpha :T) :T))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;