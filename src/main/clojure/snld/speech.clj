;;; Assignment 1
;; Write a speech recognizer that can recognize simple words.  Feel
;; free to be as creative as you would like to be in devising the
;; recognizer (there are no requirements for a particular algorithm).
;; You are allowed to use whatever supporting software you'd like to
;; use short of using an existing speech recognizer (e.g., here is FFT
;; software in C that reads an audio stream from the mic).  Test your
;; recognizer on the three color words: "red", "green", and "white".
;; You might want to record your own words as well.  Finally, think of
;; ways of biasing your recognizers so that it is better at
;; distinguishing words with a bias than words without (e.g., you
;; could bias it towards "on" so that it will recognize "on" more
;; likely compared to "one").

;; Papers on HMM for computing a statistical representation of an
;; acoustic signal
;; - Baum, 1972
;; - Baker, 1975
;; - Jelinek, 1976

;; Good info:
;; - https://en.wikipedia.org/wiki/Speech_recognition#Hidden_Markov_models
;; - http://cs.brown.edu/research/ai/dynamics/tutorial/Documents/HiddenMarkovModels.html
;; - http://www.cslu.ogi.edu/people/hosom/cs552/

(ns ^{:doc "Convert acoustic signals to text." :author "Jeremiah Via"}
  snld.speech
  (:use [snld sphinx data]
        [incanter core stats charts]))

(defprotocol Density
  (density [mixture obs] "Probability density of an observation given a mixture model."))

(defrecord GaussMixture [weight mean covariance])

(defrecord HMM [output states trans-prob emit-prob init-prob])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Acoustic processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pipline
  "Makes a pipline using the default values. (Good for running in batch
  mode.)"
  []
  (frontend (data-blocker :block-size 10)
            (preemphasizer :preemphasis-factor 0.97)
            (raised-cosine-windower :window-size 25.625 :window-shift 10.0 :alpha 0.46)
            (discrete-fourier-transform :fft-points -1  :invert false)
            (mel-frequency-filter-bank  :num-filters 40 :min-freq 130.0 :max-freq 6800.0)
            (discrete-cosine-transform  :num-filters 40 :cepstrum-len 13)
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
                :delta    (nth features 1)
                :ddelta   (nth features 2)}}))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Viterbi Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gauss-pdf
  "Calcuate the probability of an observation given the parameters of a
   multi-variate Gaussian."
  [obs mean cov]
  (let [n (length obs)
        norm (minus obs mean)]
    (exp (mult -1/2 (mmult (trans norm) (solve cov) norm)))))


(defn key->num [key]
  (Integer/valueOf (subs (str key) 1)))


(defn num->key [num]
  (keyword (str num)))


(defn max-cur
  "Given a Viterbi table and a timestamp, find the most likely state."
  [db t]
  (let [key (num->key t) entry (get db key)]
    (loop [states (keys entry) best-state -1 best-prob -1]
      (if (empty? states)
        (conj {:best best-state} (get (get db key) best-state))
        (let [cur-state (first states) cur-prob  (:prob (get entry cur-state))]
          (if (> cur-prob best-prob)
            (recur (rest states) cur-state cur-prob)
            (recur (rest states) best-state best-prob)))))))



(defn max-time [db]
  (apply max (map key->num (keys db))))


(defn get-prob-at-time [state time table]
  (let [statekey (num->key state)
        timekey (num->key time)
        prob (-> table timekey statekey :prob)]
    (if (nil? prob) 1.0M prob)))


(defn get-transition-prob [hmm prev-state cur-state]
  (-> hmm :trans-prob (.get prev-state cur-state)))


(defn bigdec-mul
  "Multiply the arguments, casting them all to big decimal. Will be
  slow but whatevs."
  [& some]
  (apply * (map bigdec some)))


(defn max-transition [observation hmm table state]
  (let [mixture   (get (:emit-prob hmm) state)
        emit-prob (gauss-pdf observation (:mean mixture) (:covariance mixture))]
    (let [candidates
          (for [prev-state (range (:states hmm))]
            (let [prev-state-prob (get-prob-at-time prev-state (max-time table) table)
                  transition-prob (get-transition-prob hmm prev-state state)]
              [prev-state (bigdec-mul emit-prob prev-state-prob transition-prob)]))
          ;; pull out the highest state-prob pair
          best (reduce #(if (> (second %1) (second %2)) %1 %2) candidates)]
      {:prev-best (num->key (first best)) :prob (second best)})))


(defn initialize [hmm]
  (let [states (range (:states hmm))]
    {:0 (into {} (for [state states]
                   {(num->key state) (get (:init-prob hmm) state)}))}))


(defn update [observation hmm table]
  (let [next-t (num->key (inc (max-time table)))
        states (range (:states hmm))]
    (into table
          {next-t
           (into {}
                 (for [cur-state states]
                   {(num->key cur-state)
                    (max-transition observation hmm table cur-state)}))})))


(defn viterbi-step
  "Performs one step of the viterbi algorithm so that it can be used
  incremenetally. If table is nil, a table is initialized based off
  this observation."
  [observation hmm table]
  (if (nil? table)
    (update observation hmm (initialize hmm))
    (update observation hmm table)))


(defn extract-solution
  "Extract the viterbi path from the table and return it with its
  probability."
  [table]
  (let [final-t (max-time table)
        {best :best prev :prev-best prob :prob} (max-cur table final-t)]
    (loop [t (dec final-t) cur prev path (list best)]
      (if (nil? cur) {:path path :prob prob}
          (let [entry (get (get table (num->key t)) cur)]
            (recur (dec t) (:prev-best entry) (cons cur path)))))))

(defn viterbi
  "Given a set of observations and a hiddern markov model, find the
   Viterbi path through the hidden markov model which best explains
   the observation sequence."
  [observations hmm]
  (loop [[obs & rest] observations
         table nil]
    (if (nil? obs)
      table
      (recur rest (viterbi-step obs hmm table)))))

(defn batch-recognize
  "Bath recognizer. Given an observation set, output the word
  associated with the best fitting hidden markov model."
  [observations hmms]
  (:output
   (reduce #(if (> (:prob %1) (:prob %2)) %1 %2)
           (pmap #(into {:output (:output %)} (extract-solution (viterbi observations %))) hmms))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; HMM training
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - To start training, can assign random, equally-likely, or other values to pi and a_ij, but does not work for b_j(o_t)
;;
;; - For b_j:
;;   - divide training data into equal length segments., computer bj(o) for each segment. Called a flat start.
;;   - split segment (state) into mixture components using VQ
;;   -


;; STEPS:  1) compute initial probabilities using VQ
;;         2) use baum-welch to better fit data (only do if basic VQ doesn't work well enough)

(defn random-vectors
  "Select num random vectors in the given feature list to use as initial
  values for the codebook vectors."
  [n feature-list]
  (let [max (dec (length feature-list))]
    (take n (repeatedly #(nth feature-list (Math/round (* (Math/random) max)))))))

(defn closest-vector [v cb]
  (reduce (fn [a b] (if (< (euclidean-distance (key a) v)
                          (euclidean-distance (key b) v))
                     a b)) cb))

(defn assign
  "Assign each feature to its best-fitting codebook."
  [codebook-m features]
  (loop [cb codebook-m
         [x & xs] features]
    (if (nil? x) cb
        (let [[k v] (closest-vector x cb)
              cb' (assoc cb k (conj v x))]
          (recur cb' xs)))))


(def minicb
  {[2.0 -0.1] []
   [1.0  0.5] []
   [2.5 -0.5] []})

(def minifeat
  [[1.0 1.0]
   [2.0 2.0]
   [1.0 2.0]
   [2.0 1.0]])


#_(defn vector-quantization [feature-list & {:keys [mixture-components]
                                             :or {mixture-components 1}}]
    (loop [codebooks ])
    (let [codebooks (random-vectors mixture-components feature-list)]
      codebooks))

#_(defn baum-welch [codebooks feature-list]
    (let [num-states (length codebooks)]))

#_(defn train [feature-list & {:keys [num-states] :or {num-states 5}}]
    (let [codebooks (vector-quantization feature-list num-states)]
      (baum-welch codebooks feature-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DATA  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def nohmm
  (map->HMM
   {:output "no"
    :states 5
    :init-prob
    [1.0 0.0 0.0 0.0 0.0]
    :trans-prob
    (matrix
     '((0.94 0.05 0.0  0.0  0.0)
       (0.0  0.92 0.07 0.0  0.0)
       (0.0  0.0  0.93 0.06 0.0)
       (0.0  0.0  0.0  0.87 0.13)
       (0.0  0.0  0.0  0.0  0.95)))
    :emit-prob
    [(GaussMixture. 1.0
                    [-2.53457 -0.52503 -0.03429 -0.13210 0.01341 -0.05493 -0.18780 -0.04576 -0.01952 -0.00593 -0.01056 0.01584 0.01032 0.01530]
                    (diag [0.16152 0.04408 0.04349 0.05813 0.11706 0.08391 0.13401 0.00804 0.00281 0.00597 0.00876 0.01370 0.01054 0.01414]))
     (GaussMixture. 1.0
                    [0.81322 0.39103 0.16393 0.61980 0.40126 -0.05607 -0.14594 0.58560 0.09958 -0.03508 0.02324 0.00184 -0.03203 0.02947]
                    (diag [7.67641 0.38931 0.30903 0.38134 0.35050 0.14392 0.11679 0.22702 0.02651 0.04004 0.04765 0.04619 0.01666 0.01500]))
     (GaussMixture. 1.0
                    [5.25131 1.07706 -0.43141 -0.59588 -0.28788 -0.14505 0.60569 -0.13300 0.02562 0.05991 -0.04623 -0.05389 0.00647 0.04008]
                    (diag [0.57589 0.01593 0.17815 0.05941 0.12991 0.11279 0.12714 0.05008 0.00162 0.00907 0.01053 0.00522 0.00684 0.01050]))
     (GaussMixture. 1.0
                    [0.98755 0.54840 0.46072 0.02585 -0.20168 -0.03173 0.43830 -0.56677 -0.19778 0.00703 0.14499 0.15456 0.09357 -0.11081]
                    (diag [2.29717 0.35795 0.04368 0.26011 0.44733 0.29240 0.26898 0.05886 0.00874 0.00749 0.00990 0.02648 0.02438 0.02656]))
     (GaussMixture. 1.0
                    [-2.03884 -0.64122 0.06727 0.13439 0.05994 0.21659 -0.27696 -0.07143 -0.01426 -0.02588 -0.03053 -0.01634 -0.00490 -0.00279]
                    (diag [0.25059 0.03916 0.07630 0.11588 0.11216 0.10312 0.09185 0.01214 0.00509 0.00320 0.00881 0.00845 0.01059 0.00901]))]}))


(def yeshmm
  (map->HMM
   {:output "yes"
    :states 7
    :init-prob
    [1.0 0.0 0.0 0.0 0.0 0.0 0.0]
    :trans-prob
    (matrix
     '((0.0000 1.0000 0.0000 0.0000 0.0000 0.0000 0.0000)
       (0.0000 0.9762 0.0238 0.0000 0.0000 0.0000 0.0000)
       (0.0000 0.0000 0.9338 0.0632 0.0000 0.0000 0.0000)
       (0.0000 0.0000 0.0000 0.9625 0.0328 0.0000 0.0000)
       (0.0000 0.0000 0.0000 0.0000 0.9419 0.0544 0.0000)
       (0.0000 0.0000 0.0000 0.0000 0.0000 0.9734 0.0260)
       (0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000)))
    :emit-prob
    [(map->GaussMixture {:weight 0.0 :mean [0.0 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0]
                         :covariance (diag [1.0 1.0 1.0 1.0 1.0 1.0 1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0])})
     (map->GaussMixture {:weight 1.00000 :mean [-2.41808  0.00970 -0.28852 -0.38376  0.26532  0.09810  0.15164 -0.01649 -0.00556  0.00724  0.00930  0.01216  0.01425  0.01017]
                         :covariance (diag [0.92173 0.07493 0.08322 0.14323 0.13188 0.12200 0.15046 0.02863 0.00627 0.00556 0.00773 0.00780 0.00646 0.00991])})
     (map->GaussMixture {:weight 1.00000 :mean [ 2.77985  0.29170  0.70859  1.49642 -0.29487 -0.65209  0.18536  0.53990  0.04980  0.01819  0.06664 -0.02487 -0.12755 -0.04871]
                         :covariance (diag [6.01920  0.10914  0.23596  0.41431  0.17107  0.49547  0.16285  0.22413  0.01166  0.03259  0.06680  0.02226  0.01519  0.01329])})
     (map->GaussMixture {:weight 1.00000 :mean [ 4.25997  0.11441  0.21787  0.02356 -0.19038 -0.32093 -0.37590 -0.12458 -0.03944  0.00841 -0.01486 -0.02460  0.09278  0.00060]
                         :covariance (diag [1.39589  0.98086  0.31231  0.26232  0.23350  1.05698  0.23738  0.03056  0.01554  0.01737  0.02575  0.01535  0.01250  0.01453])})
     (map->GaussMixture {:weight 1.00000 :mean [ 0.31160  0.00889  0.15643  0.30229 -0.49070  0.45393  0.12475 -0.25398  0.06078 -0.01499 -0.00980  0.07949 -0.05125  0.04533]
                         :covariance (diag [1.94311 0.32805 0.06093 0.21266 0.37614 0.22867 0.21589 0.03857 0.02143 0.01168 0.00753 0.01791 0.01709 0.01984])})
     (map->GaussMixture {:weight 1.00000 :mean [-2.41165 -0.09141 -0.22959 -0.36962  0.24838  0.05349  0.08217 -0.01910 -0.01022 -0.01855 -0.01339 -0.01482 -0.00294  0.00292]
                         :covariance (diag [0.52574  0.06199  0.08811  0.16488  0.11336  0.12230  0.13745  0.02724  0.00510  0.00411  0.00555  0.00995  0.00988  0.01141])})
     (map->GaussMixture {:weight 0.00000 :mean [ 0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000]
                         :covariance (diag [1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000])})]}))


(def hmms [nohmm yeshmm])


;;; Some vars to hold the files
(def red2 "src/main/resources/red2.wav")
(def on3 "src/main/resources/on3.wav")
(def green2 "src/main/resources/green2.wav")
(def one1 "src/main/resources/one1.wav")
(def white1 "src/main/resources/white2.wav")

