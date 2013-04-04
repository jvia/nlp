(ns snld.hmm
  (:use [incanter core stats charts])
  (:import org.apache.commons.math3.distribution.MultivariateNormalDistribution))


(defn gauss-pdf
  "Calcuate the probability of an observation given the parameters of a
   multi-variate Gaussian."
  [obs mean cov]
  (let [n (length obs)
        norm (minus obs mean)]
    (exp (mult -1/2 (mmult (trans norm) (solve cov) norm)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Density
  (density [mixture obs] "Probability density of an observation given a mixture model."))

(defrecord GaussMixture [weight mean covariance]
  Density
  (density [mixture obs]
    (gauss-pdf obs (:mean mixture) (:covariance mixture))))


(def observations
  [[-2.0056 -0.5617 -0.1262 -0.2387 -0.3990 0.2257 0.5405 -0.1414 0.0202 0.0240 0.0511 0.2644 0.0624 -0.1833]
   [-2.3322 -0.4646 -0.0372 -0.0283 0.0949 0.0824 -0.2091 -0.1130 -0.0097 -0.0318 -0.0512 0.1962 -0.0203 -0.2374]
   [-2.5494 -0.5093 -0.0507 -0.0882 0.6763 0.6096 -0.0011 -0.0980 -0.0208 -0.0356 -0.0619 0.0578 -0.1435 -0.2317]
   [-2.2989 -0.6366 -0.3229 -0.5701 0.0443 -0.0677 -0.3757 -0.0445 -0.0627 -0.0542 -0.0311 -0.1095 -0.1620 -0.0746]
   [-2.5120 -0.5796 -0.1612 -0.2776 -0.0847 -0.4169 -0.5346 0.0209 -0.0222 -0.0106 0.0763 -0.0853 -0.0983 -0.0299]])

(def transitions
  (matrix
   '((0.0 1.0 0.0 0.0 0.0 0.0 0.0)
     (0.0 0.9484938249124053 0.048496676944224396 0.0 0.0 0.0 0.0)
     (0.0 0.0 0.928253885592965 0.0690924372950916 0.0 0.0 0.0)
     (0.0 0.0 0.0 0.9343165308873307 0.06422132232401329 0.0 0.0)
     (0.0 0.0 0.0 0.0 0.8718045675272101 0.12760828253871562 0.0)
     (0.0 0.0 0.0 0.0 0.0 0.9528002486246617 0.04553518295828341)
     (0.0 0.0 0.0 0.0 0.0 0.0 0.0))))

;; Each element in the vector is the mixture of a 
(def mixture
  [(GaussMixture. 0.0
                  [0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000]
                  (diag [0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000]))
   (GaussMixture. 1.0
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
                  (diag [0.25059 0.03916 0.07630 0.11588 0.11216 0.10312 0.09185 0.01214 0.00509 0.00320 0.00881 0.00845 0.01059 0.00901]))
   (GaussMixture. 0.0
                  [0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000]
                  (diag [0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000  0.00000]))])