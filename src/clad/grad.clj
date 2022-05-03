(ns clad.grad
  (:require [clojure.core.matrix :as m]
            [clad.util :as utl]
            [clad.expr :as expr]
            [clad.graph :as graph]
            [clad.ops :as ops]))

(defn bottom-up [graph]
  (let [sorted-node-idxs (graph/topological-sort graph)
        adj (:adj graph)
        nodes (:nodes graph)]
    (loop [remaining-node-idxs sorted-node-idxs new-nodes {}]
      (if (empty? remaining-node-idxs)
        {:adj adj :nodes new-nodes}
        (let [[curr-node-idx & remaining] remaining-node-idxs
              curr-node (get nodes curr-node-idx)]
          (recur
           remaining
           (if
            (= (reduce + (m/get-row adj curr-node-idx)) 0.0)
             (assoc new-nodes curr-node-idx curr-node)
             (assoc new-nodes curr-node-idx
                    (assoc curr-node
                           :value
                           (reduce
                            (ops/get-op (:op curr-node))
                            (map
                             (fn [idx] (:value (get new-nodes idx)))
                             (utl/positions #{1} (m/get-row adj curr-node-idx)))))))))))))

(defn ^:private -get-parent-idxs [adj idx]
  (let [column (m/get-column adj idx)
        parents-idxs (utl/which column 1)]
    parents-idxs))

(defn ^:private -get-children-idxs [adj idx]
  (let [row (m/get-row adj idx) children-idxs (utl/which row 1)]
    children-idxs))

(defn top-down [graph]
  (let [sorted-node-idxs (reverse (graph/topological-sort graph))
        adj (:adj graph)
        nodes (:nodes graph)]
    (loop [remaining-node-idxs sorted-node-idxs new-nodes nodes]
      (if (empty? remaining-node-idxs)
        {:adj adj :nodes new-nodes}
        (let [[curr-node-idx & remaining] remaining-node-idxs
              curr-node (get nodes curr-node-idx)
              curr-parent-idxs (-get-parent-idxs adj curr-node-idx)]
          (recur
           remaining
           (if
            (nil? curr-parent-idxs)
             (assoc new-nodes curr-node-idx curr-node)
             (loop [remaining-curr-parent-idxs curr-parent-idxs curr-node curr-node]
               (if
                (empty? remaining-curr-parent-idxs)
                 (assoc new-nodes curr-node-idx curr-node)
                 (let [[curr-parent-idx & remaining] remaining-curr-parent-idxs
                       curr-parent (get new-nodes curr-parent-idx)]
                   (recur
                    remaining
                    (let [curr-parent-child-idxs (-get-children-idxs adj curr-parent-idx)
                          curr-node-equation-idx (utl/which curr-parent-child-idxs curr-node-idx)
                          adj-fun (ops/get-adjs (:op curr-parent) (first curr-node-equation-idx))
                          values (map (fn [ii] (:value (get nodes ii))) curr-parent-child-idxs)
                          adj (adj-fun (:adjoint curr-parent) (:value curr-parent) values)
                          curr-node (assoc curr-node :adjoint (+ (:adjoint curr-node) adj))]
                      curr-node))))))))))))

(defn grad [f idx]
  (let [graph (top-down (bottom-up (expr/expression-graph f)))]
  (:adjoint (nth (filter
           (fn [node] (:is-leaf node))
           (vals (into (sorted-map) (:nodes graph)))
           ) idx)
   ) )
  )