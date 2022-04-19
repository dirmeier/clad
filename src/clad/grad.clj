;(ns clojure-etudes.mcmc)
;(require '[clojure-etudes.data :as data])
;(require '[distributions.core :as dist])
;
;(defn acceptance [D xt x0]
;  (let [likt (dist/log-pdf (dist/normal xt 1.0) 1.0)
;        priort (dist/log-pdf (dist/normal 0.0 1.0) xt)
;        lik0 (dist/log-pdf (dist/normal x0 1.0) 1.0)
;        prior0 (dist/log-pdf (dist/normal 0.0 1.0) x0)]
;    likt + priort - lik0 - prior0))
;
;(defn propose [mean D]
;  (let
;    [x (data/sample-normal 1 mean 1.0)
;     a (min 1.0 (acceptance D x mean))]
;    (if (> (first (data/sample-uniform 1))  a)
;      mean
;      (first x))))
;
;(defn metropolis-hastings [D]
;  (loop [means [0.0]]
;    (if (< (count means) 10)
;      (recur (conj means (propose (first means) D)))
;      means)))
;
;(defn mcmc []
;  (let
;    [n 100
;     params '(1.1 1.0)
;     D (data/sample-normal n (first params) (second params))]
;    (println (metropolis-hastings D))))