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
  snld.parser
  (:require [clojure.string :as str]))

(def ^:dynamic *parse*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data representation

(defn complex [dir take yield]
  {:dir dir  :take take :yield yield})


(defn atom? [token]
  (= (type token) clojure.lang.Keyword))


(defn complex? [token]
  (or
   (= (type token) clojure.lang.PersistentHashMap)
   (= (type token) clojure.lang.PersistentArrayMap)))


(defn get-syntax [lexicon lex-entry]
  (get lexicon lex-entry))


(defn ccg->str [ccg]
  (if (atom? ccg) (subs (str ccg) 1)
      (let [{take :take yield :yield dir :dir} ccg
            ret (if (atom? yield) (ccg->str yield) (str "(" (ccg->str yield) ")"))
            arg (if (atom? take)  (ccg->str take)  (str "(" (ccg->str take) ")"))]
        (str ret
             (if (= :right dir) "/" "\\")
             arg))))


(defn pprint-ccg [ccg]
  (println (ccg->str ccg)))


(defn pprint-lexicon [lexicon]
  (doall (map #(println (key %) "=>" (ccg->str (val %))) lexicon))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexicon

(def mappings
  {:ADJ (complex :left  :N :N)
   :DET (complex :right :N :NP)
   :NP :NP})

(def lexicon
  {;; Determiners
   :the    (complex :right :N :NP)
   ;; {:word "the" :syntax (complex :right :N :NP) :semantics (fn [X] [:det x])}
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

(defn- applicable? [alpha beta]
  (or (appli-left? alpha beta) (appli-right? alpha beta)))

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


(defn- composable? [alpha beta]
  (or (compose-left? alpha beta) (compose-right? alpha beta)))


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
  ;; TODO move from automatic sentence construction to abstract types
  [alpha]
  (complex :right (complex :left alpha :S) :S))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser

#_(defn shift [key parse]
  (cons (get-syntax lexicon key) parse))


#_(defn redüc [parse]
  (let [[alpha beta & rest] parse]
    (filter (complement nil?)
            [(cons (appli alpha beta) rest)
             (cons (compose alpha beta) rest)
             (cons (type-raise alpha) (cons beta rest))])))


#_(defn parse [token parse]
  (let [parse' (map #(shift token %) parse)]
    (mapcat redüc parse')))

#_(defn batch-parse [str]
  (let [token-list (map #(keyword %) (str/split str #" "))]
    (println "Tokens:" token-list)
    (loop [[token & rest] token-list parz nil]
      (println parz)
      (println "--------------")
        (if (nil? token) parz
          (recur rest (parse token parz))))))