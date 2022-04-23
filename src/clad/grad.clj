(ns clad.grad)
(require '[clad.expr :as expr])
(require '[clad.node :as node])

(defn grad [f idx]
  (let [graph (expr/expression-graph f)
        adj (node/adjacency graph)]
    (println "------------")
    (println adj)
    (doseq [item graph]
      (println item))))