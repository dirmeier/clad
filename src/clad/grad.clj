(ns clad.grad)
(require '[clad.expr :as expr])

(defn grad [f idx]
  (let [graph (expr/expression-graph f)]
    (println 1123123131)
    (doseq [item graph]
      (println item))))