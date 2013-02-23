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
           edu.cmu.sphinx.util.props.ConfigurationManager))


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


(defn add-data-source [pipeline source]
  (.setDataSource pipeline source))

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

(defn where-am-i? []  (.getCanonicalPath (File. ".")))

(defn word-prob [W])
(defn aw-prob
  "The probability of the acoustic sequence given the "
  [A W])

(defn bigram
  "Computes the bigram probabilities on given corpora.

   Should perform backoff to prevent any 0 probabilities to rare utterances."
  [corpora] nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPRECATED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the processing pipeline without the data source
(def xml "
<config>
<component name=\"frontend\" type=\"edu.cmu.sphinx.frontend.FrontEnd\">
     <propertylist name=\"pipeline\">
        <!--<item>audioFileDataSource</item>-->
        <item>dataBlocker</item>
        <item>preemphasizer</item>
        <item>windower</item>
        <item>dft</item>
        <item>melFilterBank</item>
        <item>dct</item>
        <item>batchCMN</item>
        <item>featureExtractor</item>
     </propertylist>
 </component>
 <component name=\"audioFileDataSource\" type=\"edu.cmu.sphinx.frontend.util.AudioFileDataSource\"/>
<component name=\"dataBlocker\" type=\"edu.cmu.sphinx.frontend.DataBlocker\"/>
 <component name=\"preemphasizer\" type=\"edu.cmu.sphinx.frontend.filter.Preemphasizer\"/>
 <component name=\"windower\" type=\"edu.cmu.sphinx.frontend.window.RaisedCosineWindower\"/>
 <component name=\"dft\" type=\"edu.cmu.sphinx.frontend.transform.DiscreteFourierTransform\"/>
 <component name=\"melFilterBank\" type=\"edu.cmu.sphinx.frontend.frequencywarp.MelFrequencyFilterBank\"/>
 <component name=\"dct\" type=\"edu.cmu.sphinx.frontend.transform.DiscreteCosineTransform\"/>
 <component name=\"batchCMN\" type=\"edu.cmu.sphinx.frontend.feature.BatchCMN\"/>
 <component name=\"featureExtractor\" type=\"edu.cmu.sphinx.frontend.feature.DeltasFeatureExtractor\"/>
</config>
")

(defn get-frontend []
  (spit "tmp" xml)
  (let [cm (ConfigurationManager. "tmp")]
    (.lookup cm "frontend")))

(defn amp
  "Computes the amplitude of the raw sound-stream.

   Based off of
   http://stackoverflow.com/questions/4708613/graphing-the-pitch-frequency-of-a-sound"
  [stream]
  (for [i (range (count stream)) :when (even? i)]
    (let [low  (int (aget stream i))
          high (int (aget stream (inc i)))]
      (+ (bit-shift-left high 8)
         (bit-and low 0x00ff)))))

(defn audio-stream-as-bytes [file]
  (with-open [stream (AudioSystem/getAudioInputStream
                      (BufferedInputStream. (FileInputStream. file)))]
    (let [len (* (.getFrameLength stream)
                 (.getFrameSize (.getFormat stream)))
          bytes (byte-array len)]
      (.read stream bytes)
      bytes)))


(defn fft [amp]
  (let [fft (DoubleFFT_1D. (count amp))
        arr (double-array amp)]
    (.realForward fft arr)
    (into [] arr)))

(defn spectrogram [stream]
  (map #(Math/pow (Math/abs %) 2)
       (fft (amp stream))))

(defn process [stream]
  (fft (amp stream)))


(defn plot-stream [stream]
  (let [data (process stream)
        len (range (count data))]
    (view (line-chart len data)))) 

(defn plot-sound-file [file]
  (plot-stream (audio-stream-as-bytes file)))
