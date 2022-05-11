(ns clad.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clad.core :refer [grad]]))

(defn f [x y]
  (/ (- 1.0 (Math/exp (- x)))
     (+ 1.0 (Math/exp (- y)))))

(defn close-to?
  [x y epsilon]
  (<= (abs (- x y)) epsilon))

(deftest test-gradients
  (testing "gradient w.r.t x"
    (is (close-to? ((grad f 0) 2.0 1.0) 0.09893801980144722 0.001)))
  (testing "gradient w.r.t y"
    (is (close-to? ((grad f 1) 2.0 1.0) 0.17000340156854793 0.001))))