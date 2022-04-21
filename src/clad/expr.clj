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

(defn create-node [node list]
  (if
    (identical? (get list 0) :list)
    (let
      [op (rest list)]
      list
      )))

(defn ^:private traverse [node rest]
  (loop [nodes rest]
    (if
      (not (empty? nodes))
      (let [[part & remaining] nodes]
        (println part)
        (create-node node part)
        (recur remaining)))))

(defn ^:private create-root [graph]
  (let [root-node (first graph)]
    (if
      (identical? (get root-node 0) :op)
      (let [root-op (get root-node 1)]
        (node/create-node root-op nil [] 1.0)))))

(defn expression-graph [f]
  (let [graph (rest (get ((insta/parser grammar) f) 1))
        root (create-root graph)]
    (println graph)
    (println (rest graph))
    (println "----------")
    (traverse root (rest graph))
    ))


