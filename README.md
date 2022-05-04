# clad

[![Project Status](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Build status](https://github.com/dirmeier/clad/workflows/CI/badge.svg)](https://github.com/dirmeier/clad/actions)

> Reverse-mode autodiff for Clojure

## Introduction

This package implements a proof-of-concept of reverse-mode autodiff in Clojure. It is mainly thought for learning Clojure,
offers minimal functionality and lacks (code) quality. 

```{clojure}
(require '[clad.core :refer [grad]])

(defn log-pdf ^double [^double y ^double mu ^double sigma]
  (- (/ (Math/pow (- y mu) 2.0) (* -2.0 (Math/pow sigma 2.0)))
     (Math/log sigma)
     (/ (Math/log (* 2.0 Math/PI)) 2.0)))
     
(def (grad(logpdf, 0) 1.0 0.0 1.0))
=> -1.0
```

## Author

* Simon Dirmeier <a href="mailto:sfyrbnd @ pm me">sfyrbnd @ pm me</a>
