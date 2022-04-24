(ns clad.node
  (:require [clojure.core.matrix :as m]))

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children adjoint value])

(defn create-node [op parent children adjoint value]
  (node. (int (next-value)) op parent children adjoint value))

(defn adjacency [graph]
  (let [n-nodes (+ (reduce max (map (fn [element] (:idx element)) graph)) 1)]
    (loop [nodes graph adj (m/new-matrix n-nodes n-nodes)]
      (if (empty? nodes)
        adj
        (let [[part & remaining] nodes]
          (recur
            (if (nil? (:parent part)) remaining (conj remaining (:parent part)))
            (if
              (nil? (:idx (:parent part)))
              adj
              (m/mset adj (:idx (:parent part)) (:idx part) 1))))))))

(defn get-op [op]
  (get
    {"/" (fn [x y] (/ x y))
     "*" (fn [x y] (* x y))}
    op))

(defn bottom-up [graph]
  (loop [nodes graph new-graph []]
    (if
      (empty? nodes)
      (set new-graph)
      (let [[part & remaining] nodes]
        (recur
          (if (nil? (:parent part))
            remaining
            (conj remaining (:parent part)))
          (if
            (nil? (:value part))
            (conj new-graph
                  (assoc part :value
                              (reduce
                                (get-op (:op part))
                                (map
                                  (fn [child] (:value child))
                                  (:children part)
                                  ))))
            (conj new-graph part)))))))