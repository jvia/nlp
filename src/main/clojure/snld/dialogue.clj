;;; Assignment 3
;; Write a simple spoken dialogue component in ADE that can perform
;; simple task-based dialogues (you are free to use your previous SR
;; and parser, or any of the existing ADE components).  The system
;; should be able to answer simple wh-questions and carry out simple
;; commands.
;;
;; http://www.pomdp.org/pomdp/index.shtml
(ns ^{:doc "A dialogue "
      :author "Jeremiah Via"}
  snld.dialogue)


;;; TODO:
;; - think of domain and come up with a dialogue.
;; - need this information to start training
;; - need a way to represent the pomdp, the
;; - MPD needs to learn a policy


;; pomdp has:
;; - set of actions
;; - immediate reward function
;; - probabilistic stte-transition function