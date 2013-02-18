;; Need:
;; - DONE signal processor
;; - TODO speech decoder
;; - TODO speech model: P(W)
;; - TODO language model: P(A|W))

;; http://stackoverflow.com/questions/7674877/how-to-get-frequency-from-fft-result

(ns ^{:doc "Convert acoustic signals to text."
      :author "Jeremiah Via"}
  snld.speech
  (:use [incanter core charts stats])
  ;; incanter.Matrix 	  incanter.Weibull 	incanter.charts 	incanter.core
  ;; incanter.datasets 	incanter.infix 	incanter.internal 	incanter.io
  ;; incanter.main 	  incanter.stats
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

(defn make-pipline
  "Makes a pipline using the default values."
  []
  (FrontEnd. [(DataBlocker. 10)
              (Preemphasizer. 0.97)
              (RaisedCosineWindower. 0.46 25.625 10.0)
              (DiscreteFourierTransform. 512 false)
              (MelFrequencyFilterBank. 130.0 6800.0 40)
              (DiscreteCosineTransform. 40 13)
              (BatchCMN.)
              (DeltasFeatureExtractor.)]))

(defn get-frontend []
  (spit "tmp" xml)
  (let [cm (ConfigurationManager. "tmp")]
    (.lookup cm "frontend")))

(def preemphasizer (Preemphasizer.))
(def windower (RaisedCosineWindower.))
(def dft (DiscreteFourierTransform. 512 false))
(def filter-bank (MelFrequencyFilterBank.))
(def dct (DiscreteCosineTransform.))
(def batch-cmn (BatchCMN.))
(def feature-extractor (DeltasFeatureExtractor.))
(def pipeline (FrontEnd. [preemphasizer
                          windower
                          dft
                          filter-bank
                          dct
                          batch-cmn
                          feature-extractor]))


(defn add-data-source [pipeline source]
  (.setDataSource pipeline source))

;;(def pipeline (FrontEnd.))



(defn file [url]
  (File. url))
;; <component name="preemphasizer" type="edu.cmu.sphinx.frontend.filter.Preemphasizer"/>
;; <component name="windower" type="edu.cmu.sphinx.frontend.window.RaisedCosineWindower"/>
;; <component name="dft" type="edu.cmu.sphinx.frontend.transform.DiscreteFourierTransform"/>
;; <component name="melFilterBank" type="edu.cmu.sphinx.frontend.frequencywarp.MelFrequencyFilterBank"/>
;; <component name="dct" type="edu.cmu.sphinx.frontend.transform.DiscreteCosineTransform"/>
;; <component name="batchCMN" type="edu.cmu.sphinx.frontend.feature.BatchCMN"/>
;; <component name="featureExtractor" type="edu.cmu.sphinx.frontend.feature.DeltasFeatureExtractor"/>

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


(defn where-am-i? []  (.getCanonicalPath (File. ".")))

(defn word-prob [W])
(defn aw-prob
  "The probability of the acoustic sequence given the "
  [A W])


;; Papers on HMM for computing a statistical representation of an
;; acoustic signal
;; - Baum, 1972
;; - Baker, 1975
;; - Jelinek, 1976

(defn bigram
  "Computes the bigram probabilities on given corpora.

   Should perform backoff to prevent any 0 probabilities to rare utterances."
  [corpora] nil)