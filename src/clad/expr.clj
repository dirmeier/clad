(ns clad.expr
  (:require [clojure.repl :refer [source-fn demunge]]
            [clojure.core.matrix :as m]
            [instaparse.core :as insta]
            [clad.node :as node]))

(def ^:private grammar
  "exprs =  exps
   <exps> = {ws? exp ws?}
   <exp> = list | vec | map | set | num
         | string | regex | comment | literal
   list = <'('> exps <')'>
   vec = <'['> exps <']'>
   map = <'{'> exps <'}'>
   set = <'#{'> exps <'}'>
   <num> = int | ratio | float | big
   int = #'([-+]?)(?:([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[xX]([0-9A-Fa-f]+)|[0-9]+)'
   big = int <'N'> | float <'M'>
   ratio = #'[-+]?[0-9]+/[0-9]+'
   float = #'[-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?'
   string = #'\"(\\\\\"|[^\"])*\"'
   regex =  #'#\"(\\\\\"|[^\"])*\"'
   comment = #';[^\\n\\r]*'
   <literal> = anfn | unquote | unquote-splice | meta
             | var | quote | op | char | scalar
   anfn = '#' list
   quote = '\\'' ws? exp
   unquote = <'~'> ws? exp
   unquote-splice = <'~@'> ws? exp
   meta = <'^'> ws? exp
   var = <'#\\''> ws? exp
   op = '*' | '/' | '+' | '-' | 'Math/log' | 'Math/exp' | 'Math/pow' | sym-pat
   scalar = 'Math/PI' | 'mu' | 'y' | 'sigma' | 'x' | 'z'
   <sym-pat> = #'[^@~(),\\\\;`\\[\\]{}~^\\s:#/\\'\\d]((:?[^@~(),\\\\;`\\[\\]{}~^\\s:])*[^@~(),\\\\;`\\[\\]{}~^\\s:/])?'
   char = #'\\\\(newline|space|tab|backspace|formfeed|return|.|\\n)'
   <ws> = <#'[\\s,]+'>
  ")

(def consts {"Math/PI" Math/PI})


(defn -cast-value [value]
  (if (string? value)
    (try
      (Double/parseDouble value)
      (catch Exception _
        (if (nil? (get consts value))
          0.0
          (get consts value))))
    value))


(defn ^:private -create-node [parent list itr]
  (if (identical? (get list 0) :list)
    (-create-node parent (rest list) itr)
    (if (identical? (get (first list) 0) :op)
      (let
        [op (get (first list) 1)
         child (node/create-node op parent [] 0.0 0.0 nil)]
        (loop [remaining-grand-children (rest list)
               grand-children []]
          (if (empty? remaining-grand-children)
            grand-children
            (let [[part & remaining] remaining-grand-children]
              (recur remaining (conj grand-children (-create-node child part itr)))))))
      (node/create-node (get list 0) parent [] 0.0 (-cast-value (get list 1)) (get list 1)))))


(defn ^:private -build-tree [parent rest]
  (let [itr (atom -1)]
    (loop [nodes rest leaves []]
      (if (empty? nodes)
        (doall (map (fn [leaf] (assoc leaf :is-variable (if (= 0.0 (:value leaf)) true false))) (flatten leaves)))
        (let [[part & remaining] nodes]
          (recur remaining
                 (into leaves
                       (let [n (-create-node parent part itr)]
                         (if (or (list? n) (vector? n)) n [n])))))))))


(defn ^:private -create-root [graph]
  (let [root-node (first graph)]
    (if
      (identical? (get root-node 0) :op)
      (let [root-op (get root-node 1)]
        (node/create-node-with-idx 0 root-op nil [] 1.0 0.0 nil))
      nil)))


(defn ^:private -adjacency [graph]
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


(defn ^:private -graph-as-set [graph]
  (loop [remaining-nodes graph new-nodes {}]
    (if (empty? remaining-nodes)
      new-nodes
      (let [[part & remaining] remaining-nodes]
        (recur
          (if
            (nil? (:parent part))
            remaining
            (conj remaining (:parent part)))
          (assoc new-nodes (:idx part) {:op          (:op part)
                                        :adjoint     (:adjoint part)
                                        :is-variable (:is-variable part)
                                        :name        (:name part)
                                        :value       (:value part)}))))))


(defn -fn-source [f]
  (let [fn-name (-> f .getClass .getName)
        fn-name (demunge fn-name)
        fn-sym (symbol fn-name)]
    (source-fn fn-sym)))

; this doesnt work
(defn -merge-children [nodes variables]
  (let [variable-leaves (filter (fn [node] (:is-variable node)) nodes)
        non-var-leaves (filter (fn [node] (not (:is-variable node))) nodes)]
    (loop [remaining-variables variables new-nodes []]
      (if (empty? remaining-variables)
        (into new-nodes non-var-leaves)
        (let [[variable & remaining] remaining-variables
              nodes-with-same-name (filter (fn [node] (= variable (:name node))) variable-leaves)
              fom (first nodes-with-same-name)]
          (recur
            remaining
            (conj new-nodes (node/create-node-with-idx 0 (:op fom) (map :parent nodes-with-same-name) [] 0.0 0.0 (:name fom)))))))))

(defn -assign-ids [leaves variables]
  (let [leaves (-merge-children leaves variables) itr (atom -1)]
    (loop [remaining-nodes variable-leaves new-nodes []]
      (if (empty? remaining-nodes)
        new-nodes
        (let [[node & remaining] remaining-nodes]
          (recur
            (sort-by :idx (into remaining (:parent node)))
            (conj new-nodes {:idx (int (swap! itr inc))
                             :is-variable (:is-variable node)
                             :op (:op node)
                             :parent (:parent node)
                             :children (:children node)
                             :adjoint (:adjoint node)
                             :value (:value node)
                             :name (:name node)})
            ))))))

(defn expression-graph [f]
  (let [parsed (get ((insta/parser grammar) (-fn-source f)) 1)
        grammar (rest (get parsed 4))
        args (map (fn [el] (second el)) (rest (get parsed 3)))
        root (-create-root grammar)
        nodes (-assign-ids (-build-tree root (rest grammar)) args)]
    {:args args
     :adj (-adjacency nodes)
     :nodes (-graph-as-set nodes)}))
