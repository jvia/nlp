;; Need:
;; - DONE signal processor
;; - TODO speech decoder
;; - TODO speech model: P(W)
;; - TODO language model: P(A|W))

(ns ^{:doc "Convert acoustic signals to text."
      :author "Jeremiah Via"}
  snld.speech
  (:use [incanter core charts stats])
;; incanter.Matrix 	  incanter.Weibull 	incanter.charts 	incanter.core
;; incanter.datasets 	incanter.infix 	incanter.internal 	incanter.io
;; incanter.main 	  incanter.stats
  (:import [java.io File FileInputStream InputStream BufferedInputStream] 
           [javax.sound.sampled AudioFormat AudioInputStream AudioSystem]
           [edu.emory.mathcs.jtransforms.fft DoubleFFT_1D]))


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


(defn plot-stream [stream]
  (let [data (process stream)
        len (range (count data))]
    (view (line-chart len data)))) 

(defn plot-sound-file [file]
  (plot-stream (audio-stream-as-bytes file)))



(defn fft [amp]
  (let [fft (DoubleFFT_1D. (count amp))
        arr (double-array amp)]
    (.realForward fft arr)
    (into [] arr)))

(defn process [stream]
  (fft (amp stream)))


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