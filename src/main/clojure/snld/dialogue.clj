;;;
;; Will be a dialogue system in which a restuarant robot puts dry
;; dishes away.
;;
;; - 2 agents: 1 human to wash & dry dishes, 1 robot to put them away
;;
;; - Set number of dishes.
;;
;; - Goal is to be able to answers questions and ue utterances from
;;   the human to update beliefs about the world and itself.
;;
;; - Should be able to answer simple wh-questions about this domain
;;   and of itself in general.
(ns ^{:doc "A dialogue "
      :author "Jeremiah Via"}
  snld.dialogue
  (:refer-clojure :exclude [==])
  (:require [snld.sapa :as sapa])
  (:use [clojure.core.match :only [match]]
        clojure.core.logic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
(defn predicate [name & vars]
  {:name name :vars (into {} (for [var vars] {(keyword var) nil}))})

(defn bind
  "Attempt to bind the given value to the specified var in the
  predicate. Return a new predicate with the bound var or nil."
  [predicate var val]
  (when (and (contains? (:vars predicate) var)
             (nil? (get-in predicate [:vars var])))
    (assoc-in predicate [:vars var] val)))

(defn predicate->str [predicate]
  (str "(" (:name predicate) " "
       (apply str (interpose ", " (for [var (:vars predicate)]
                                    (str "?" (key var) "-"
                                         (if (nil? (val var))
                                           "nil" (val var)))))) ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDDL-ish stuff
(defrel type type)


;; agent(robby).
;; agent(human).
(defrel agent agent)
(agent :robby)
(agent :human)

;; goal(put_away, dishes).
(defrel goal action object)
(goal :put-away, :dishes)

(def script
  [{:human {:str "Robby, put away the clean dishes."
            :sem [:forall :x [[[:dish :x] :& [:clean :x]] :-> [:put_away :x]]]}}
   {:robot {:str "Okay."
            :sem :ack}}
   {:human {:str  "How many dishes are dirty?"
            :sem [:query :x [[:dishes :x] :& [:dirty :x]]]}}
   {:robot {:str "Four."
            :sem [:number 4]}}
   {:human {:str "Okay. I will clean the plates, then you put them away."
            :sem [:ack [:forall :x :y :z [[[:human :y] :-> [[:plate :x] :-> [:clean :x]]] :&
                                          [[:robot :z] :-> [:put_away :x]]]]]}}
   {:robot {:str "Okay."
            :sem :ack}}
   {:human {:str "The first plate is clean."
            :sem [:exists :x [[:plate :x] :-> [:clean :x]]]}}
   {:robot {:str "Okay."
            :sem :ack}}
   {:human {:str "WOAH."
            :sem :exclaim}}
   {:human {:str "You just broke the plate."
            :sem [:exists :x :y [[:robot :y] :& [:plate :x] :& [:broke :x]]]}}
   {:human {:str "Why did you do that?"
            :sem [:query :x [[:plate :x] :& [:broke :x]]]}}
   {:robot {:str "I don't know."
            :sem [:unknown]}}
   {:human {:str "Go slower and that won't happen."
            :sem [[:slower :action] :-> [:not [:exists :x [:plate :x] :& [:broke :x]]]]}}
   {:robot {:str "Okay."
            :sem :ack}}
   {:human {:str "Now put the rest away."
            :sem [:forall :x [[:plate :x] :-> [:put_away :x]]]}}
   {:robot {:str "Okay."
            :sem :ack}}
   {:human {:str "How many do you need to put away?"
            :sem [:query :x [[:plate :x] :& [:number :x]]]}}
   {:robot {:str "Three."
            :sem [:number 3]}}])


(def statuses [:not_started :started :in_progress :complete :unknown])



(def task-kb
  {:status :unknown
   :dry_dishes 1})

(def human-kb
  {:mood :unknown
   :identity :human})

(def self-db
  {:name :robby
   :identity :robot
   :speed :fast
   :confidence :high})

(def kb {:task task-kb :human human-kb :self self-db})


(defn update-status [utterance kb]
  (assoc-in kb [:status] :in_progress))

(defn dialogue [utterance kb]
  (update-status utterance kb))

(defn batch-dialogue [script]
  (loop [[line & lines] script kb kb]
    (if (nil? line) kb
        (recur lines (dialogue line kb)))))

;; - need this information to start training
;; - need a way to represent the pomdp, the
;; - MPD needs to learn a policy


;; pomdp has:
;; - set of actions
;; - immediate reward function
;; - probabilistic stte-transition function
