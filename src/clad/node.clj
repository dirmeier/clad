(ns clad.node
  (:require [clojure.core.matrix :as m])
  (:require [clojure.set :as set]))

(def counter (atom -1))

(defn next-value []
  (swap! counter inc))

(defrecord node [idx op parent children adjoint value])

(defn create-unique-tree-node [op parent children adjoint value]
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

(defn graph-as-set [graph]
  (loop [remaining-nodes graph new-nodes {}]
    (if (empty? remaining-nodes)
      new-nodes
      (let [[part & remaining] remaining-nodes]
        (recur
          (if (nil? (:parent part)) remaining (conj remaining (:parent part)))
          (assoc new-nodes (:idx part) {:op (:op part) :adjoint (:adjoint part) :value (:value part)})
          )))))

(defn adj-to-edge-map [graph]
  (let [adj (:adj graph)
        m (m/row-count adj)
        n (m/column-count adj)]
    (loop [i 0 parent-node-map {}]
      (if (= i m)
        parent-node-map
        (recur
          (+ i 1)
          (loop [j 0 parent-node-row []]
            (if (= j n)
              (assoc parent-node-map i parent-node-row)
              (recur
                (+ j 1)
                (if (= (m/mget adj i j) 1) (conj parent-node-row j) parent-node-row)))))))))

(defn in? [coll elm] (some #(= elm %) coll))

(defn filter-map [lhs rhs]
  (loop [keys rhs arr []]
    (if (empty? keys)
      arr
      (let [[par & remaining] keys]
        (recur
          remaining
          (if (in? lhs par) (conj arr par))
          )))))

(defn topological-sort [graph]
  (let [edge-map (adj-to-edge-map graph)]
    (loop [node-topological [] current-edge-map edge-map]
      (if (empty? current-edge-map)
        node-topological
        (let [nodes (filter #(= (count (get current-edge-map %)) 0) (keys current-edge-map)) ]
          (recur
            (into node-topological nodes)
            (let [
                  current-edge-map (apply dissoc current-edge-map nodes)
                  keys (keys current-edge-map)]
              (loop [curk keys curm current-edge-map]
                (if (empty? curk)
                  curm
                  (let [[part & remaining] curk]
                    (recur
                      remaining
                      (assoc curm part (filter-map (get curm part) keys)))))))))))))
