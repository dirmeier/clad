(ns clad.main
  (:require [clad.core :as core]))


(defn f [y mu sigma]
  (- (Math/log sigma)
     (- (/ (Math/pow (- y mu) 2.0) (* (- 2.0) (Math/pow sigma 2.0)))
        (/ (Math/log (* 2.0 Math/PI)) 2.0))))

(defn -main
  [& args]
  (println ((core/grad f 0) 0.0 1.0 1.0)))