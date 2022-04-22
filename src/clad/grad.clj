(ns clad.grad)
(require '[clad.expr :as expr])

(defn grad [f idx]
  (let [graph (expr/expression-graph f)]
    (doseq [item graph]
      (println item))))