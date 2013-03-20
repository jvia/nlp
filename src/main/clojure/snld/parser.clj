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

;; Implement the CCG parser from class
;; http://www.aclweb.org/aclwiki/index.php?title=Combinatory_Categorial_Grammar