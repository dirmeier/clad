(ns clad.core-test
  (:require [clojure.test :refer :all]
            [clad.core :refer [grad]]))

(defn log-pdf ^double [^double y ^double mu ^double sigma]
  (- (/ (Math/pow (- y mu) 2) (* -2.0 (Math/pow sigma 2)))
     (Math/log sigma)
     (/ (Math/log (* 2.0 Math/PI)) 2.0)))
;
;(defn tanh ^double [^double x ^double y]
;  (/ (1.0 - (Math/exp (- x)))
;     (1.0 + (Math/exp (- y)))))

(def log-pdf "(* (* 2.0 Math/PI) 7.0)")

(defn close-to?
  [x y epsilon]
  (<= (abs (- x y)) epsilon))

(deftest test-normal-logpdf-gradients
  (testing "gradient w.r.t y"
    (is (close-to? (grad log-pdf 0) 21.991148575128552  0.001)))
  (testing "gradient w.r.t mu"
    (is (close-to? (grad log-pdf 1) 14.0  0.001)))
  (testing "gradient w.r.t sigma"
    (is (close-to? (grad log-pdf 2) 6.283185307179586  0.001))))