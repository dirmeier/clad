(ns clad.util)

(defn positions
  [pred coll]
  (let [x (keep-indexed (fn [idx x] (when (pred x) idx)) coll)] x))

(defn which [vec idx]
  (let [id (doall (map first (filter #(= (second %) idx) (map-indexed vector vec))))] id))
