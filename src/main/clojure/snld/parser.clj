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



(defn ccg->str [ccg]
  (if (nil? ccg) nil)
  
  (if (atom? ccg) (subs (str ccg) 1)
      (let [{take :take yield :yield dir :dir} ccg
            ret (if (atom? yield) (ccg->str yield) (str "(" (ccg->str yield) ")"))
            arg (if (atom? take)  (ccg->str take)  (str "(" (ccg->str take) ")"))]
        (str ret
             (if (= :right dir) "/" "\\")
             arg))))


(defn pprint-ccg [ccg]
  (println (ccg->str ccg)))

(defn sentence? [S]
  (if (atom? S)
    (= :S S)
    (and (= 1 (count S))
         (= :S (first S)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexicon

(def ^{:private true}
  the1 (complex :right :N :NP))
(def ^{:private true}
  the2 (complex :right :N (complex :right (complex :left :NP :S) :S)))
(def ^{:private true}
  doctor1 :N)
(def ^{:private true}
  doctor2 (complex :right (complex :left :N :N) :N))
(def ^{:private true}
  patient1 :N)
(def ^{:private true}
  patient2 (complex :right (complex :left :N :N) :N))
(def ^{:private true}
  sent1 (complex :right :PP (complex :left :NP :S)))
(def ^{:private true}
  sent2 (complex :right :PP (complex :left :N :N)))
(def ^{:private true}
  arrived (complex :left :NP :S))
(def ^{:private true}
  for_w (complex :right :NP :PP))

(def lexicon
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
  (cond (appli-right? alpha beta)(:yield alpha)
        (appli-left? alpha beta) (:yield beta)))

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
  (cond (compose-left? alpha beta)  (complex :left (:take alpha) (:yield alpha))
        (compose-right? alpha beta) (complex :right (:take beta) (:yield beta))))


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
(defn stacks->str [stacks]
  (map #(map ccg->str %) stacks))

(defn pprint-stacks [stacks]
  (doall (println (stacks->str stacks))))

(defn good-stack? [stack]
  (not (some nil? stack)))

(defn shift [key stacks lexicon]
  (let [entries (get lexicon key)]
    (if (nil? stacks)
      (for [entry entries] [entry])
      (for [entry entries
            stack stacks]
        (cons entry stack)))))


(defn ccg-reduce
  "Perform reduction unless all the stacks are of size 1, meaning only
  one token has been seen."
  [stacks]
  (if (= (count stacks) (reduce + (map count stacks)))
    stacks
    (filter good-stack?
            (concat (map (fn [stack]
                           [(appli   (second stack) (first stack))]) stacks)
                    (map (fn [stack]
                           [(compose (second stack) (first stack))]) stacks)))))


(defn parse [token stacks lexicon]
  (ccg-reduce (shift token stacks lexicon)))


(defn batch-parse
  ([str] (batch-parse str lexicon))
  ([str lexicon]
     (let [token-list (map #(keyword %) (str/split str #" "))]
       (println "Tokens:" token-list)
       (loop [[token & rest] token-list stacks nil]
         (if (nil? token)
           (do (println "-----------------------------")
               (println "Final stacks")
               (pprint-stacks stacks)
               stacks)
           (do (let [shift (shift token stacks lexicon)
                     reduce (ccg-reduce shift)]
                 (do (println "-----------------------------")
                     (println "Shift")
                     (pprint-stacks shift)
                     (println "-----------------------------")
                     (println "Reduce")
                     (pprint-stacks reduce))
                 (recur rest reduce))))))))