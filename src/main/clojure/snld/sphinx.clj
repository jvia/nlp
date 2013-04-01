(ns
    ^{:doc "Wrappers around Sphinx"
      :author "Jeremiah Via"}
  snld.sphinx
  (:import java.net.URL
           edu.cmu.sphinx.decoder.Decoder
           edu.cmu.sphinx.decoder.search.PartitionActiveListFactory
           edu.cmu.sphinx.decoder.search.SimpleBreadthFirstSearchManager
           edu.cmu.sphinx.decoder.scorer.ThreadedAcousticScorer
           edu.cmu.sphinx.decoder.pruner.SimplePruner
           edu.cmu.sphinx.frontend.DataBlocker
           edu.cmu.sphinx.frontend.DataEndSignal
           edu.cmu.sphinx.frontend.DataProcessor
           edu.cmu.sphinx.frontend.DataStartSignal
           edu.cmu.sphinx.frontend.FrontEnd
           edu.cmu.sphinx.frontend.feature.BatchCMN
           edu.cmu.sphinx.frontend.feature.DeltasFeatureExtractor
           edu.cmu.sphinx.frontend.filter.Preemphasizer
           edu.cmu.sphinx.frontend.frequencywarp.MelFrequencyFilterBank
           edu.cmu.sphinx.frontend.transform.DiscreteCosineTransform
           edu.cmu.sphinx.frontend.transform.DiscreteFourierTransform
           edu.cmu.sphinx.frontend.util.AudioFileDataSource
           edu.cmu.sphinx.frontend.util.Microphone
           edu.cmu.sphinx.frontend.util.Microphone
           edu.cmu.sphinx.frontend.window.RaisedCosineWindower
           edu.cmu.sphinx.instrumentation.MemoryTracker
           edu.cmu.sphinx.instrumentation.BestPathAccuracyTracker
           edu.cmu.sphinx.instrumentation.SpeedTracker
           edu.cmu.sphinx.recognizer.Recognizer
           edu.cmu.sphinx.util.LogMath
           edu.cmu.sphinx.util.props.ConfigurationManager))

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

(defn microphone
  "Creates an instance of a Sphinx microphone for use in speech
   reconigiton. If no arguments are supplied, it will attempt to do
   the right thing.

   - sample-rate: sample rate of the data

   - bits-per-sample: number of bits per value.

   - channels: number of channels.

   - bigEndian: the endianness of the data

   - signed: whether the data is signed.

   - close-between-utterances: whether or not the microphone will
     release the audio between utterances.  On certain systems (Linux
     for one), closing and reopening the audio does not work too
     well. The default is false for Linux systems, true for others

   - msec-per-read: the number of milliseconds of audio data to read
     each time from the underlying Java Sound audio device.

   - keep-last-audio: whether to keep the audio data of an utterance
     around until the next utterance is recorded.

   - stereo-to-mono: how to convert stereo audio to mono. Currently,
     the possible values are 'average', which averages the samples
     from at each channel, or 'selectChannel', which chooses audio
     only from that channel. If you choose 'selectChannel', you should
     also specify which channel to use with the 'selectChannel'
     property.

   - selected-channel: the channel to use if the audio is stereo

   - sected-mixer-index: the mixer to use.  The value can be
     'default,' (which means let the AudioSystem decide), 'last,'
     (which means select the last Mixer supported by the AudioSystem),
     which appears to be what is often used for USB headsets, or an
     integer value which represents the index of the Mixer.Info that
     is returned by AudioSystem.getMixerInfo(). To get the list of
     Mixer.Info objects, run the AudioTool application with a command
     line argument of '-dumpMixers'.
"
  [& {:keys [sample-rate bits-per-sample channels
             big-endian signed close-between-utterances
             msec-per-read keep-last-audio stereo-to-mono
             selected-channel selected-mixer buffer-size]
      ;; All magic values are taken from Sphinx source
      :or {sample-rate              1600
           bits-per-sample          16
           channels                 1
           big-endian               false
           signed                   true
           close-between-utterances true
           msec-per-read            10
           keep-last-audio          false
           stereo-to-mono           "average"
           selected-channel         0
           selected-mixer           "default"
           buffer-size              6400}}]
  ;; A constructor only a mother could love
  (Microphone. sample-rate bits-per-sample channels
               big-endian signed close-between-utterances
               msec-per-read keep-last-audio stereo-to-mono
               selected-channel selected-mixer buffer-size))

(defn start-recording
  "Initialize and start recording data from the microphone."
  [mic]
  (.initialize mic)
  (.startRecording mic))

(defn stop-recording
  "Stop recording the microphone and clear its data cache."
  [mic]
  (.stopRecording mic)
  (.clear mic))


(defn threaded-acoustic-scorer
  [frontend & {:keys [score-normalizer min-scoreable num-threads cpu-relative thread-priority]
               :or [score-normalizer nil
                    min-scoreable 10
                    num-threads 0
                    cpu-relative true
                    prior Thread.NORM_PRIORITY]}]
  (ThreadedAcousticScorer. frontend score-normalizer min-scoreable
                           cpu-relative num-threads thread-priority))

(defn simple-pruner []
  (SimplePruner.))


(defn log-math [& {:keys [log-base use-add-table]
                   :or {log-base 1.0001 use-add-table true}}]
  (LogMath. log-base use-add-table))

(defn partition-active-list-factory [absolute-beam-width relative-beam-width log-math]
  (PartitionActiveListFactory. absolute-beam-width
                               relative-beam-width
                               log-math))


(defn bfs-manager [log-math linguist pruner scorer active-list-factory
                   show-token-count relative-beam-width
                   grow-skip-interval word-entry-pruning]
  (SimpleBreadthFirstSearchManager. log-math linguist pruner scorer active-list-factory
                                    show-token-count relative-beam-width
                                    grow-skip-interval word-entry-pruning))

(defn decoder [search-manager fire-non-final-results
               auto-allocate result-listeners feature-block-size]
  (Decoder. search-manager fire-non-final-results auto-allocate
            result-listeners feature-block-size))

(defn recognizer [decoder monitors]
  (Recognizer. decoder monitors))

(defn memory-tracker [recognizer & {:keys [show-summary show-details]
                                    :or {show-summary true
                                         show-details true}}]
  (MemoryTracker. recognizer show-summary show-details))

(defn best-path-accuracy-tracker
  [recognizer & {:keys [summary details results aligned-results raw-results full-path]
                 :or {summary false details false
                      results false aligned-results false
                      raw-results false full-path false}}]
  (BestPathAccuracyTracker. recognizer summary details results aligned-results raw-results full-path))

(defn speed-tracker [recognizer frontend & {:keys [summary details response-time timers]
                                            :or {sumarry true details false
                                                 response-time false timers false}}]
  (SpeedTracker. recognizer frontend summary details response-time timers))