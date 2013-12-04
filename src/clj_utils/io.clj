(ns ^{:doc "Functions related to caching results of computations."}
  clj-utils.io
  (:use clj-utils.misc)
  (:use [clojure.java.shell :only [sh]]))

(defn pr-str-to-file
  "Write an object to file, so that it can be read by load-file
(assuming it is well-behaved w.r.t. pr-str)."
  [filename object]
  (spit filename
        (pr-str object)))
