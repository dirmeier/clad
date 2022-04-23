(defproject clad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [instaparse "1.4.11"]
                 [org.clojure/tools.analyzer "1.1.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/tools.analyzer.jvm "1.2.2"]]
  :repl-options {:init-ns clad.core})
