(ns ^{:doc "A re-implementation of the Sphinx tutorial in Clojure"
      :author "Jeremiah Via <jeremiah.via@gmailc.com>"}
  snld.tutorial
  (:import edu.cmu.sphinx.frontend.util.Microphone
           edu.cmu.sphinx.recognizer.Recognizer
           edu.cmu.sphinx.result.Result
           edu.cmu.sphinx.util.props.ConfigurationManager)
  (:use [taoensso.timbre :as log :only (trace debug info warn error fatal spy)]
        snld.sphinx snld.speech))


(def absolute-beam-width 5000)
(def relative-beam-width 1e-120)

(def root "src/main/resources/")

(defn init-recognzer [frontend]
  (let [scorer (threaded-acoustic-scorer frontend)
        pruner (simple-pruner)
        linguist nil
        logmath (log-math)
        alf (partition-active-list-factory absolute-beam-width relative-beam-width logmath)
        search (bfs-manager logmath linguist pruner scorer alf false 0.0 0 false)
        decoder (decoder search false false [] 10000)
        ;;monitors [(best-path-accuracy-tracker re-implementation)]
        reconizer (recognizer decoder [])]))

(defn -main []
  (let [audio-source (microphone)
        frontend (make-pipline)]
    (log/debug "Initializing microphone")
    (add-data-source frontend audio-source)
    (start-recording audio-source)
    (loop []
      (log/info (data audio-source))
      (recur))
    #_(loop [val (data frontend)]
      (cond (start? val) (do
                           (log/info "Data start signal")
                           (recur (data frontend)))
            (end? val) (log/info "Data End Signal")
            :else (do
                    (log/info (.toString val))
                    (recur (data frontend)))))))

  