(ns clad.node)

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children adjoint])

(defn create-node [op parent children adjoint]
  (node. (next-value) op parent children adjoint))
