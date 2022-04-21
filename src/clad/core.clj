(ns clad.core)
(require '[clad.node :as node])
(require '[clad.grad :as grad])

(defn log-pdf ^double [^double y ^double mu ^double sigma]
  (- (/ (Math/pow (- y mu) 2) (* -2.0 (Math/pow sigma 2)))
     (Math/log sigma)
     (/ (Math/log (* 2.0 Math/PI)) 2.0)))

(def log-pdf "(/ (* 2.0 Math/PI) 2.0)")

(defn -main
  [& args]
  ;(print log-pdf)
  (grad/grad log-pdf 1)
  ;(print ((grad/grad log-pdf 1) 1.0 0.0 1.0))
  )