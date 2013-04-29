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


(def human-script
  '({:str "Robby, put away the clean dishes.",
     :sem [:forall :x [[[:dish :x] :& [:clean :x]] :-> [:put_away :x]]]}
    {:str "How many dishes are dirty?",
     :sem [:query :x [[:dishes :x] :& [:dirty :x]]]}
    {:str "Okay. I will clean the plates, then you put them away.",
     :sem [:ack [:forall :x :y :z [[[:human :y] :-> [[:plate :x] :-> [:clean :x]]] :& [[:robot :z] :-> [:put_away :x]]]]]}
    {:str "The first plate is clean.",
     :sem [:exists :x [[:plate :x] :-> [:clean :x]]]}
    {:str "WOAH.", :sem :exclaim}
    {:str "You just broke the plate.",
     :sem [:exists :x :y [[:robot :y] :& [:plate :x] :& [:broke :x]]]}
    {:str "Why did you do that?",
     :sem [:query :x [[:plate :x] :& [:broke :x]]]}
    {:str "Go slower and that won't happen.",
     :sem [[:slower :action] :-> [:not [:exists :x [:plate :x] :& [:broke :x]]]]}
    {:str "Now put the rest away.",
     :sem [:forall :x [[:plate :x] :-> [:put_away :x]]]}
    {:str "How many do you need to put away?",
     :sem [:query :x [[:plate :x] :& [:number :x]]]}))


(def task-kb
  {:status :unknown})

(def human-kb
  {:mood :unknown
   :identity :human})

(def self-db
  {:name :robby
   :identity :robot
   :speed :fast
   :confidence :high})

(def world-db
  {:dishes {:dirty 4 :clean 0}})

(def kb {:task task-kb :human human-kb :self self-db :world world-db})


(defn act
  "Perform an action based on the KB."
  [kb old-kb]
  (let [cur-goal (first (get-in kb [:task :goal]))]
    (match [cur-goal]
           [[:query :x [[:dishes :x] :& [:dirty :x]]]] (assoc-in kb [:self :communicate] (get-in kb [:world :dishes :dirty])))))

(defn update-task [utterance kb]
  #_(println "UTTERANCE:" utterance)
  (match
   [utterance]
   [[:forall :x [[[:dish :x] :& [:clean :x]] :-> [:put_away :x]]]] (assoc-in (assoc-in kb [:task :status] [:started]) [:task :goal] [utterance])
   [[:query :x [[:dishes :x] :& [:dirty :x]]]]                     (update-in (update-in kb [:task :status] #(cons :started %)) [:task :goal] #(cons utterance %))
   [:done]                                                         (assoc-in (assoc-in kb [:task :status] :done) [:task :goal] nil)
   :else kb))

(defn update-status [utterance kb]
  kb)

(defn generate-utterance [utterance kb old-kb]
  (cond
   (not (nil? (get-in kb [:self :communicate]))) (get-in kb [:self :communicate])
   (and (nil? (-> old-kb :task :goal)) (not (nil? (-> kb :task :goal)))) "okay"
        :else "bleep bloop"))

(defn dialogue
  "Given the logical representation of an utterance, update the knowledge base "
  [utterance kb]
  (let [task      (update-task utterance kb)
        status    (update-status utterance task)
        post-act  (act status kb)
        utterance (generate-utterance utterance post-act kb)]
    (println "POST-ACT" post-act)
    {:kb post-act :out utterance}))

(defn batch-dialogue [script]
  (loop [[line & lines] script kb kb]
    (println "HUMAN:"  (:str line))
    (if (nil? line) kb
        (let [{kb' :kb out :out} (dialogue (:sem line) kb)]
          (println "ROBOT:" out)
          (recur lines kb')))))

(defn gordon []
  (let [kb (batch-dialogue human-script)]
    (println kb)))
