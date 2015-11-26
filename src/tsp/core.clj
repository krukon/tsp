(ns tsp.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [tsp.graph :as graph]))

(defn read-input []
  "Returns a lazy sequence of points read from *in*"
  (let [n (read)]
    {:n n
     :points (for [id (range n)]
               {:x (read)
                :y (read)
                :id id})}))

(defn -main
  [& args]
  (let [input (read-input)
        points (:points input)
        graph (graph/build-graph points)]
    (println "Graph:")
    (pprint graph)))
