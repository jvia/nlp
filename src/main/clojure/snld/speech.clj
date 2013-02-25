;; Papers on HMM for computing a statistical representation of an
;; acoustic signal
;; - Baum, 1972
;; - Baker, 1975
;; - Jelinek, 1976
(ns ^{:doc "Convert acoustic signals to text."
      :author "Jeremiah Via"}
  snld.speech
  (:use ;;[incanter core charts stats]
   [snld sphinx])
  (:import [java.io File FileInputStream InputStream BufferedInputStream] 
           [javax.sound.sampled AudioFormat AudioInputStream AudioSystem]
           [edu.emory.mathcs.jtransforms.fft DoubleFFT_1D]
           edu.cmu.sphinx.frontend.filter.Preemphasizer
           edu.cmu.sphinx.frontend.window.RaisedCosineWindower
           edu.cmu.sphinx.frontend.transform.DiscreteFourierTransform
           edu.cmu.sphinx.frontend.frequencywarp.MelFrequencyFilterBank
           edu.cmu.sphinx.frontend.transform.DiscreteCosineTransform
           edu.cmu.sphinx.frontend.feature.BatchCMN
           edu.cmu.sphinx.frontend.feature.DeltasFeatureExtractor
           edu.cmu.sphinx.frontend.FrontEnd
           edu.cmu.sphinx.frontend.DataProcessor
           edu.cmu.sphinx.frontend.util.Microphone
           edu.cmu.sphinx.frontend.util.AudioFileDataSource
           edu.cmu.sphinx.frontend.DataBlocker
           java.net.URL
           edu.cmu.sphinx.util.props.ConfigurationManager
           edu.cmu.sphinx.frontend.DataStartSignal
           edu.cmu.sphinx.frontend.DataEndSignal))

(defn make-pipline
  "Makes a pipline using the default values."
  []
  (frontend
   (data-blocker :block-size 10)
   (preemphasizer :preemphasis-factor 0.97)
   (raised-cosine-windower :window-size 25.625
                           :window-shift 10.0
                           :alpha 0.46)
   (discrete-fourier-transform :fft-points -1
                               :invert false)
   (mel-frequency-filter-bank :num-filters 40
                              :min-freq 130.0
                              :max-freq 6800.0)
   (discrete-cosine-transform :num-filters 40
                              :cepstrum-len 13)
   (batch-cmn)
   (deltas-feature-extractor :window-size 3)))

(defn prep-data
  "Tranforms data from the DeltaFeatureExtractor into a map."
  [d]
  (assert (zero? (mod (count (.getValues d)) 3))
          "Extracted features must be a multiple of 3.")
  (let [features (partition 13 (.getValues d))]
    {:timestamp (.getCollectTime d)
     :sample-rate (.getSampleRate d)
     :features {:cepstrum (nth features 0)
                :delta (nth features 1)
                :ddelta (nth features 2)}}))

(defn extract-features
  "Pulls the features out of a DeltasFeaturesExtractor."
  [frontend source]
  (add-data-source frontend source)
  (loop [val (data frontend) 
         accm nil]
    (cond (start? val) (recur (data frontend) nil)
          (end? val) accm
          :else (recur (data frontend)
                       (conj accm (prep-data val))))))

(defn audio-file-feature-extractor
  "Given a path to an audio file, extract all features from it for use
  in model building."
  [path]
  (let [frontend (make-pipline)
        audio (audio-file-data-source :file path)]
    (extract-features frontend audio)))

(def ef
  {:timestamp 1361569455108, :sample-rate 16000,
   :features
   {:cepstrum '(-0.6350142 0.93213284 0.0996516 -0.5628229 -0.009311179 0.47833294 -0.18784887 -0.2874116 -0.3183234 -0.3713474 -0.10051748 0.1914295 0.12308788),
    :delta '(5.394108 -0.192472 -0.7899849 0.1037459 -1.9239167E-4 -0.038117655 -0.506319 -0.286837 0.12566972 0.16874354 0.0024353901 0.49930543 -0.07753961),
    :ddelta '(-0.4581273 -0.49824384 0.17722619 0.09057022 0.16333143 -0.50345284 -0.065142944 0.4884249 0.26812062 0.20054768 0.13845621 -0.1357249 -0.28731254)}})


;; Debug helpers
(defn where-am-i? []  (.getCanonicalPath (File. ".")))
(defn available-methods [object]
  (let [class (.getClass object)]
    (for [method (.getDeclaredMethods class)]
      (.toGenericString method))))

