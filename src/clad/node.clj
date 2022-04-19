(ns clad.node)

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defn create-node [op]
  {:idx (next-value) :op op})
