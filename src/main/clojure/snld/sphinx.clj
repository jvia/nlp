(ns snld.sphinx
  (:import edu.cmu.sphinx.frontend.filter.Preemphasizer
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

(defn frontend
  [& dataprocessors]
  (FrontEnd. dataprocessors))

(defn data-blocker
  [& {:keys [block-size] :or {block-size 10}}]
  (DataBlocker. block-size))

(defn preemphasizer
  [& {:keys [preemphasis-factor] :or {preemphasis-factor 0.97}}]
  (Preemphasizer. preemphasis-factor))

(defn raised-cosine-windower
  [& {:keys [window-size window-shift alpha]
      :or {window-size 25.625
           window-shift 10
           alpha 0.46}}]
  (RaisedCosineWindower. alpha window-size window-shift))

(defn discrete-fourier-transform
  [& {:keys [fft-points invert]
      :or {fft-points -1 invert false}}]
  (DiscreteFourierTransform. fft-points invert))

(defn mel-frequency-filter-bank
  [& {:keys [num-filters min-freq max-freq]
      :or {num-filters 40
           min-freq 130.0
           max-freq 6800.0}}]
  (MelFrequencyFilterBank. min-freq max-freq num-filters))

(defn discrete-cosine-transform
  [& {:keys [num-filters cepstrum-len]
      :or {num-filters 40 cepstrum-len 13}}]
  (DiscreteCosineTransform. num-filters cepstrum-len))

(defn batch-cmn []
  (BatchCMN.))

(defn deltas-feature-extractor
  [& {:keys [window-size]
      :or {window-size 3}}]
  (DeltasFeatureExtractor. window-size))

(defn audio-file-data-source
  "Creates an AudioFileDataSource. If given a file, it will attempt to
  add it to the object, if possible. If this fails, it will just
  return the base object."
  [& {:keys [bytes-per-read listeners file]
      :or {bytes-per-read 3200
           listeners nil
           file nil}}]
  (let [audio (AudioFileDataSource. bytes-per-read listeners)]
    (when-not (nil? file)
      (try
        (.setAudioFile audio (URL. (str "file:" file)) (str file))
        (catch NullPointerException _
          (println (str "Could not locate file: " file)))))
    audio))

(defn start?
  "Returns true if the data is a start signal."
  [data]
  (= DataStartSignal (class data)))

(defn end?
  "Returns true if the data is an end signal."
  [data]
  (= DataEndSignal (class data)))

(defn data
  "Get the data from a DataProcessor."
  [d]
  (.getData d))

(defn add-data-source
  "Add a data source to a pipeline."
  [pipeline source]
  (.setDataSource pipeline source))