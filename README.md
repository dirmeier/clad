# clad

[![Project Status](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Build status](https://github.com/dirmeier/clad/workflows/ci/badge.svg)](https://github.com/dirmeier/clad/actions)

> Reverse-mode autodiff for Clojure

## Introduction

This package implements a proof-of-concept of reverse-mode autodiff in Clojure. It is mainly thought for learning Clojure, 
hence it offers minimal functionality, lacks (code) quality, and is probably buggy. 

```{clojure}
(require '[clad.core :refer [grad]])

(defn f [x y]
  (/ (- 1.0 (Math/exp (- x)))
     (+ 1.0 (Math/exp (- y)))))
     
(def g ((grad f 0) 2.0 1.0))

g
=> 0.0989
```

## Author

Simon Dirmeier <a href="mailto:sfyrbnd @ pm me">sfyrbnd @ pm me</a>
