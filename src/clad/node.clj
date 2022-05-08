(ns clad.node)

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children ^double adjoint ^double value ^boolean is-variable name])

(defn create-node-with-idx
  [idx op parent children adjoint value name]
  (node. idx op parent children adjoint value false name))

(defn create-node [op parent children adjoint value name]
  (create-node-with-idx (int (next-value)) op parent children adjoint value name))