(ns clad.node)

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children ^double adjoint ^double value ^boolean is-leaf])

(defn create-node [op parent children adjoint value]
  (node. (int (next-value)) op parent children adjoint
         (if (string? value)
           (try
             (Double/parseDouble value)
             (catch Exception _ (get {"Math/PI" Math/PI} value)))
           value)
         false))