(ns snld.speech-test
  (:use clojure.test snld.speech))

(is (= "/Users/jxv911/tufts/snld") (where-am-i?))