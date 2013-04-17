(ns ^{:doc "Convert acoustic signals to text."
      :author "Jeremiah Via"}
  snld.speech
  (:use [snld sphinx data]
        [incanter core stats charts]))


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


(defn max-time
  "Find the largest time iteration in the viterbi table."
  [db]
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


(defn max-transition
  "Return the most probable transition and its probability."
  [observation hmm table state]
  (let [mixture   (nth (:emit-prob hmm) state)
        emit-prob (gauss-pdf observation (:mean mixture) (:covariance mixture))]
    (let [candidates
          (for [prev-state (range (:states hmm))]
            (let [prev-state-prob (get-prob-at-time prev-state (max-time table) table)
                  transition-prob (get-transition-prob hmm prev-state state)]
              [prev-state (bigdec-mul emit-prob prev-state-prob transition-prob)]))
          ;; pull out the highest state-prob pair
          best (reduce #(if (> (second %1) (second %2)) %1 %2) candidates)]
      {:prev-best (num->key (first best)) :prob (second best)})))


(defn initialize
  "Initialize an empty Viterbi table."
  [hmm]
  (let [states (range (:states hmm))]
    {:0 (into {} (for [state states]
                   {(num->key state) (get (:init-prob hmm) state)}))}))


(defn update
  "Update the Viterbi table with the given observation, hmm and
  table."
  [observation hmm table]
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

(defn random-vectors
  "Select num random vectors in the given feature list to use as initial
  values for the codebook vectors."
  [n feature-list]
  (let [max (dec (length feature-list))]
    (take n (repeatedly #(nth feature-list (Math/round (* (Math/random) max)))))))

(defn init-codebook [n feature-list]
  (into {} (for [v (random-vectors n feature-list)]
             {v nil})))

(defn closest-vector [v cb]
  (reduce (fn [a b] (if (< (euclidean-distance (key a) v)
                          (euclidean-distance (key b) v))
                     a b)) cb))


(defn kmm-assign
  "Assign each feature to its best-fitting codebook."
  ;; TODO: Look into using partition-by...
  [codebook-m features]
  (loop [cb codebook-m
         [x & xs] features]
    (if (nil? x) cb
        (let [[k v] (closest-vector x cb)
              cb' (assoc cb k (conj v x))]
          (recur cb' xs)))))


(defn kmm-update
  "Given a map of codebook vectors and the assigned feature vectors,
  update the centroids to better fit the data."
  [cb-vector]
  (into {}
        (for [cb cb-vector]
          (let [normalizing-factor (/ 1.0 (length (val cb)))
                unadjsted-u (reduce plus (val cb))]
            {(vec (mult normalizing-factor unadjsted-u)) nil}))))


(defn vector-quantization
  "Perform k-means on the feature list, returning the centroid vectors
  and the associated feature vectors."
  [feature-list & {:keys [mixture-components] :or {mixture-components 1}}]
  (loop [cb (init-codebook mixture-components feature-list) old-cb nil]
    (if (= cb old-cb) (kmm-assign cb feature-list)
        (recur (kmm-update (kmm-assign cb feature-list)) cb))))


(defn vector-mean
  "Calculate the mean vecotr of a set of vectors."
  [vectors]
  (map (partial * (/ 1.0 (length vectors)))
       (reduce plus vectors)))

(defn vector-covariance
  "Given a mean vector and a list of vectors, compute the covariance."
  [mean features]
  (let [dim (length (first features))
        flat (for [x (range dim)
                   y (range dim)]
               (let [ex (nth mean x)
                     ey (nth mean y)
                     xs (map #(nth % x) features) 
                     ys (map #(nth % y) features)
                     raw (mult (map (fn [x] (- x ex)) xs)
                               (map (fn [y] (- y ey)) ys))]
                 (/ (apply + raw) (length raw))))]
    (matrix flat dim)))

(defn extract-mixture-model
  "Given the map of codebook vectors and their closest feature vectors,
  extract the Gaussian mixture model that represents the data.

  This initial extraction assumes that all Gaussian mixtures have the same weighting.

  TODO: Extend ---  Currently only creates GMM of a single mixture component. "
  [cb-map]
  (let [weight (/ 1.0 (length cb-map))
        [mean features] (first cb-map)]
    (map->GaussMixture {:weight weight :mean mean
                        :covariance (vector-covariance mean features)})))

(defn baum-welch [codebooks feature-list]
  (let [num-states (length codebooks)]))

(defn partition-into
  "Partions multiple collections simulataenously and combine the same
  partition position in each collection"
  [num coll]
  (let [partitions (map #(partition (int (/ (length %) num)) %) coll)]
    (for [i (range (length (first partitions)))]
      (mapcat #(nth % i) partitions))))


(defn train-hmm
  "Given a phrase and a list of feature vectors, train a HMM."
  [word-str features & {:keys [num-states] :or {num-states 5}}]
  (let [feat-per-state (int (/ (length features) num-states))]
    (map->HMM
     {:output word-str
      :states num-states
      :init-prob  (vec (cons 1.0 (take (dec num-states) (repeatedly (fn [] 0.0)))))
      :trans-prob (matrix (for [x (range num-states) y (range num-states)] (if (or (= x y) (= x (dec y))) 0.5 0.0)) num-states)
      :emit-prob (map #(-> % vector-quantization extract-mixture-model) (partition-flat feat-per-state features))})))



(defn gordon
  "A function wherein Gordon verifies exercise success"
  []
  (let [ ;; Be patient
        hmms [(train-hmm "red" red)
              (train-hmm "green" green)
              (train-hmm "white" white)]
        ;; Be real patient
        output {"red"   (map #(batch-recognize % hmms) (noisify red2-features   :range 3.0 :copies 400))
                "green" (map #(batch-recognize % hmms) (noisify green2-features :range 3.0 :copies 400))
                "white" (map #(batch-recognize % hmms) (noisify white1-features :range 3.0 :copies 400))}
        stats (double (pc output))]
    (println "Percent correct: " stats)))