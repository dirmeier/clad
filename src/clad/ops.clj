(ns clad.ops
  (:require [clojure.string :as str]))

(def  ^:private -ops
  {"*"  [(fn [x y] (* x y))]
   "/"  [(fn [x y] (/ x y))]
   "-"  [(fn [x & y] (if (nil? y) (- x) (- x (first y))))]
   "+"  [(fn [x & y] (if (nil? y) (+ x) (+ x (first y))))]
   "Math/log"  [(fn [x] (Math/log x))]
   "Math/exp"  [(fn [x] (Math/exp x))]
   "Math/pow"  [(fn [x y] (Math/pow x y))]})

(defn get-op [op]
  (if (contains? -ops op)
    (get (get -ops op) 0)
    (println (str "Key " op "is not a defined operation"))))

(def ^:private -adjs
  {"+"    [(fn [g ans x] g)
           (fn [g ans x] g)]
   "-"    [(fn [g ans x] (= (count x) 1) (- g) g)
           (fn [g ans x] (- g))]
   "*"    [(fn [g ans x] (* g (last x)))
           (fn [g ans x] (* g (first x)))]
   "/"    [(fn [g ans x] (/ g (last x)))
           (fn [g ans x] (* (- g) (* (first x) (Math/sqrt (last x)))))]
   "^"    [(fn [g ans x]
             (let [ps (if (not (= (last x) 0.0)) (- (last x) 1.0) 1.0)]
               (* g (* (last x) (Math/pow (first x) ps)))))
           (fn [g ans x]
             (let [ps (if (= (first x) 0.0) 1.0 (first x))]
               (* g (* (Math/log ps) (Math/pow (first x) (last x))))))]
   "log"  [(fn [g ans x] (/ g (first x)))]
   "sqrt" [(fn [g ans x] (* g (* 0.5 (Math/pow (first x) (- 0.5)))))]
   "exp"  [(fn [g ans x] (* g ans))]})

(defn get-adjs [op idx] (let [alt (get (get -adjs op)  idx)] alt))