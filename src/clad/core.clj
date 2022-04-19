(ns clad.core)
(require '[clad.node :as node])

(defn f ^double [^double y ^double mu ^double sigma]
  (- (/ (Math/pow (- y mu) 2) (* -2.0 (Math/pow sigma 2)))
     (Math/log sigma)
     (/ (Math/log (* 2.0 Math/PI)) 2.0)))

(defn -main
  [& args]
  (print (f 0 2 3)))