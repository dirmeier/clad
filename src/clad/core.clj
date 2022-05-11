(ns clad.core
  (:require [clad.expr :as expr]
            [clad.graph :as graph]
            [clad.ops :as ops]
            [clad.util :as utl]
            [clojure.core.matrix :as m]))

(defn ^:private -compute [op vals]
  (if
   (> (count vals) 1)
    (op (first vals) (last vals))
    (op (first vals))))

(defn ^:private -bottom-up [graph]
  (let [sorted-node-idxs (graph/topological-sort graph)
        adj (:adj graph)
        nodes (:nodes graph)]
    (loop [remaining-node-idxs sorted-node-idxs new-nodes {}]
      (if (empty? remaining-node-idxs)
        {:adj adj :nodes new-nodes :args (:args graph)}
        (let [[curr-node-idx & remaining] remaining-node-idxs
              curr-node (get nodes curr-node-idx)]
          (recur
           remaining
           (if
            (= (reduce + (m/get-row adj curr-node-idx)) 0.0)
             (assoc new-nodes curr-node-idx curr-node)
             (let [children (utl/positions #{1} (m/get-row adj curr-node-idx))]
               (assoc new-nodes curr-node-idx
                      (assoc
                       curr-node
                       :value
                       (-compute
                        (ops/get-op (:op curr-node))
                        (map
                         (fn [idx] (:value (get new-nodes idx)))
                         children))))))))))))

(defn ^:private -get-parent-idxs [adj idx]
  (let [column (m/get-column adj idx)
        parents-idxs (utl/which column 1)]
    parents-idxs))

(defn ^:private -get-children-idxs [adj idx]
  (let [row (m/get-row adj idx) children-idxs (utl/which row 1)]
    children-idxs))

(defn ^:private -set-values [graph y]
  (let [nodes (:nodes graph)
        args (:args graph)]
    (loop [args args
           y y
           new-nodes nodes]
      (if
       (empty? args)
        {:args (:args graph) :adj (:adj graph) :nodes new-nodes}
        (let [[arg & remaining-args] args
              [value & remaining-values] y]
          (recur
           remaining-args
           remaining-values
           (let [var-nodes (filter (fn [node] (= (:name (val node)) arg)) new-nodes)]
             (into new-nodes (for [[k v] var-nodes] [k (assoc v :value value)])))))))))

(defn ^:private -top-down [graph]
  (let [sorted-node-idxs (reverse (graph/topological-sort graph))
        adj (:adj graph)
        nodes (:nodes graph)]
    (loop [remaining-node-idxs sorted-node-idxs new-nodes nodes]
      (if (empty? remaining-node-idxs)
        {:adj adj :nodes new-nodes :args (:args graph)}
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
  (let [graph (expr/expression-graph f)]
    (fn [& y]
      (let [graph (-top-down (-bottom-up (-set-values graph y)))]
        (:adjoint
         (nth
          (filter
           (fn [node] (:is-variable node))
           (vals (into (sorted-map) (:nodes graph))))
          idx))))))
