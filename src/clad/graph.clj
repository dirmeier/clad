(ns clad.graph
  (:require [clojure.core.matrix :as m]))

(defn ^:private -adj-to-edge-map [graph]
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

(defn -remove-values [hashmap items]
  (loop [rem-items items mmap hashmap]
    (if (empty? rem-items)
      mmap
      (let [[item & remaining] rem-items]
        (recur remaining (into {} (map (fn [[k v]] [k (remove #{item} v)])) mmap))))))

(defn -drop-empty [mmap]
  (doall (filter #(> (count (val %)) 0) mmap)))

(defn topological-sort [graph]
  (let [edge-map (-adj-to-edge-map graph)]
    (loop [node-topological []
           current-edge-map edge-map]
      (if (empty? current-edge-map)
        node-topological
        (let [nodes (filter #(= (count (get current-edge-map %)) 0) (keys current-edge-map))]
          (recur
           (into node-topological nodes)
           (-remove-values (-drop-empty current-edge-map) nodes)))))))