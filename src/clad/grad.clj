(ns clad.grad)
(require '[clad.expr :as expr])

(defn grad [f idx]
  (expr/expression-graph f))