(ns tsp.utils
  (:require [clojure.math.numeric-tower :as math]))

(defn distance
  "Euclidean distance between two points"
  [point-a point-b]
  (math/sqrt (+ (math/expt (- (:x point-a)
                              (:x point-b))
                           2)
                (math/expt (- (:y point-a)
                              (:y point-b))
                           2))))
