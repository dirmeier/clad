(ns clad.node)

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children adjoint value])

(defn create-node [op parent children adjoint value]
  (node. (next-value) op parent children adjoint value))