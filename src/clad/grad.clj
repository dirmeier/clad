(ns clad.grad)
(require '[clad.expr :as expr])
(require '[clad.node :as node])

(defn grad [f idx]
  (let [graph (expr/expression-graph f)
        ;graph (node/bottom-up graph)
        ]
    (doseq [item (:nodes graph)]
      (println item))))