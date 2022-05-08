(ns clad.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clad.core :refer [grad]]))

(defn f [x y]
  (/ (- 1.0 (Math/exp (- x)))
     (+ 1.0 (Math/exp (- y)))))


(defn close-to?
  [x y epsilon]
  (<= (abs (- x y)) epsilon))

(deftest test-tanh-gradients
  (testing "gradient w.r.t y"
    (is (close-to? ((grad f 0) 1.0 2.0) 0.3240271368319427 0.001)))
  (testing "gradient w.r.t mu"
    (is (close-to? ((grad f 1) 1.0 2.0) 0.06636860387867843 0.001))))