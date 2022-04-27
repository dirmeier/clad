(ns clad.expr)
(require '[instaparse.core :as insta])
(require '[clojure.pprint])
(require '[clad.node :as node])


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
             | var | quote | op | keyword | char | scalar
   anfn = '#' list
   quote = '\\'' ws? exp
   unquote = <'~'> ws? exp
   unquote-splice = <'~@'> ws? exp
   meta = <'^'> ws? exp
   var = <'#\\''> ws? exp
   op = '*' | '/' | '+' | '-' | sym-pat
   scalar = 'clojure.core/' | 'Math/PI'
   keyword = ':' ':'? sym-pat
   <sym-pat> = #'[^@~(),\\\\;`\\[\\]{}~^\\s:#/\\'\\d]((:?[^@~(),\\\\;`\\[\\]{}~^\\s:])*[^@~(),\\\\;`\\[\\]{}~^\\s:/])?'
   char = #'\\\\(newline|space|tab|backspace|formfeed|return|.|\\n)'
   <ws> = <#'[\\s,]+'>
  ")


(defn ^:private -create-node [parent list]
  (if (identical? (get list 0) :list)
    (-create-node parent (rest list))
    (if (identical? (get (first list) 0) :op)
      (let
        [op (get (first list) 1)
         child (node/create-unique-tree-node op parent [] 0.0 0.0)]
        (loop [remaining-grand-children (rest list) grand-children []]
          (if (empty? remaining-grand-children)
            grand-children
            (let [[part & remaining] remaining-grand-children]
              (recur remaining (conj grand-children (-create-node child part)))))))
      (node/create-unique-tree-node (get list 0) parent [] 0.0 (get list 1)))))

(defn ^:private -build-tree [parent rest]
  (loop [nodes rest leaves []]
    (if (empty? nodes)
      leaves
      (let [[part & remaining] nodes]
        (recur remaining
               (into leaves
                     (let [n (-create-node parent part)]
                       (if (vector? n) n [n]))))))))

(defn ^:private -create-root [graph]
  (let [root-node (first graph)]
    (if
      (identical? (get root-node 0) :op)
      (let [root-op (get root-node 1)]
        (node/create-unique-tree-node root-op nil [] 1.0 nil)))))


(defn expression-graph [f]
  (let [grammar (rest (get ((insta/parser grammar) f) 1))
        root (-create-root grammar)
        graph (-build-tree root (rest grammar))
        adj (node/adjacency graph)
        nodes (node/graph-as-set graph)]
    {:adj adj :nodes nodes}))


