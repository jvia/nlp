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
  (:require [snld.sapa :as sapa])
  (:use [clojure.core.match :only [match]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic stuff
(defn predicate [name & vars]
  {:name name :vars (into {} (for [var vars] {(keyword var) nil}))})

(defn predicate->str [predicate]
  (str "(" (:name predicate) " "
       (apply str (interpose ", " (for [var (:vars predicate)]
                                    (str "?" (key var) "-"
                                         (if (nil? (val var))
                                           "nil" (val var)))))) ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDDL-ish stuff



(def script
  [{:human {:str "Robby, help me with the dishes."
            :parse '(:S
                     (:NP (:NNP :robby))
                     (:VP (:VB :help)
                          (:NP (:PRP :me))
                          (:PP (:IN :with)
                               (:NP (:DT :the) (:NNS :dishes)))))}}
   {:robot {:str "Okay."
            :parse '(:INTJ (:UH :okay))}}
   {:human {:str  "How many dishes are dirty?"
            :parse '(:SBARQ
                     (:WHNP
                      (:WHADJP (:WRB :how) (:JJ :many))
                      (:NNS :dishes))
                     (:SQ
                      (:VP (:VBP :are)
                           (:ADJP (:JJ :dirty)))))}}
   {:robot {:str "Four."
            :parse '(:FRAG
                     (:NP (:CD :four)))}}
   {:human {:str "Okay. I will clean the plates, then you put them away."
            :parse '(:S
                     (:S
                      (:NP (:PRP :i))
                      (:VP (:MD :will)
                           (:VP (:VB :wash)
                                (:CC :and)
                                (:VB :dry)
                                (:NP (:DT :the) (:NNS :dishes)))))
                     (:RB :then)
                     (:S
                      (:NP (:PRP :you))
                      (:VP (:VBD :put)
                           (:NP (:PRP :them))
                           (:ADVP (:RB :away)))))}}
   {:robot {:str "Okay."
            :parse '(:INTJ (:UH :okay))}}
   {:human {:str "The first plate is clean."
            :parse '(:S
                     (:NP (:DT :the) (:JJ :first) (:NN :dish))
                     (:VP (:VBZ :is)
                          (:ADJP (:JJ :clean))))}}
   {:robot {:str "Okay."
            :parse '(:INTJ (:UH :okay))}}
   {:human {:str "WOAH."
            :parse '(:FRAG (:NP (:NNP :whoa)))}}
   {:human {:str "You just broke the plate."
            :parse '(:S (:NP (:PRP :you))
                        (:ADVP (:RB :just))
                        (:VP (:VBD :broke)
                             (:NP (:DT :the) (:NN :plate))))}}
   {:human {:str "Why did you do that?"
            :parse '(:SBARQ
                     (:WHADVP (:WRB :why))
                     (:SQ (:VBD :did)
                          (:NP (:PRP :you))
                          (:VP (:VB :do)
                               (:NP (:DT :that)))))}}
   {:robot {:str "I don't know."
            :parse '(:S (:NP (:PRP :i))
                        (:VP (:VBP :do) (:RB :nt)
                             (:VP (:VB :know))))}}
   {:human {:str "Go slower and that won't happen."
            :parse '(:S (:S
                         (:VP (:VB :go)
                              (:ADJP (:JJR :slower))))
                        (:CC :and)
                        (:S
                         (:NP (:DT :that))
                         (:VP (:MD :wo) (:RB :nt)
                              (:VP (:VB :happen)))))}}
   {:robot {:str "Okay."
            :parse '(:INTJ (:UH :okay))}}
   {:human {:str "Now put the rest away."
            :parse '(:S
                     (:ADVP (:RB :now))
                     (:VP (:VB :put)
                          (:NP (:DT :the) (:NN :rest))
                          (:ADVP (:RB :away))))}}
   {:robot {:str "Okay."
            :parse '(:INTJ (:UH :okay))}}
   {:human {:str "How many do you need to put away?"
            :parse '(:SBARQ
                     (:WHADJP (:WRB :how) (:JJ :many))
                     (:SQ (:VBP :do)
                          (:NP (:PRP :you))
                          (:VP (:VB :need)
                               (:S
                                (:VP (:TO :to)
                                     (:VP (:VB :put)
                                          (:PRT (:RP :away))))))))}}
   {:robot {:str "Three."
            :parse '(:FRAG
                     (:NP (:CD :three)))}}])


(def statuses [:not_started :started :in_progress :complete :unknown])



(def task-kb
  {:status :unknown
   :dry_dishes 1})

(def human-kb
  {:mood :unknown})

(def self-db
  {:name "Robby"
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
