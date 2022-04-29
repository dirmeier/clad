(ns clad.grad)
(require '[clad.expr :as expr])
(require '[clad.node :as node])

(defn grad [f idx]
  (let [graph (expr/expression-graph f)
        sorted-nodes (node/topological-sort graph)
        ]
    (println (:adj graph))
    (println sorted-nodes)
    (doseq [item (:nodes graph)]
      (println item))))