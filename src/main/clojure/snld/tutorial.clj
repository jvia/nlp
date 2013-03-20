(ns ^{:doc "A re-implementation of the Sphinx tutorial in Clojure"
      :author "Jeremiah Via <jeremiah.via@gmailc.com>"}
  snld.tutorial
  (:import edu.cmu.sphinx.frontend.util.Microphone
           edu.cmu.sphinx.recognizer.Recognizer
           edu.cmu.sphinx.result.Result
           edu.cmu.sphinx.util.props.ConfigurationManager)
  (:use [taoensso.timbre :as log :only (trace debug info warn error fatal spy)]
        snld.sphinx snld.speech))



(def root "src/main/resources/")

(defn -main []
  (let [mic (microphone)
        frontend (make-pipline)]
    (log/debug "Initializing microphone")
    (add-data-source frontend mic)
    (start-recording mic)
    (loop []
      (log/info (data mic))
      (recur))
    #_(loop [val (data frontend)]
      (cond (start? val) (do
                           (log/info "Data start signal")
                           (recur (data frontend)))
            (end? val) (log/info "Data End Signal")
            :else (do
                    (log/info (.toString val))
                    (recur (data frontend)))))))
