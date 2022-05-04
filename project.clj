(defproject clad "0.1.0-SNAPSHOT"
  :description "Reverse-mode autodiff in Clojure"
  :license {:name "GPL-3.0" :url  "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.11"]
                 [net.mikera/core.matrix "0.62.0"]]
  :repl-options {:init-ns clad.core})
