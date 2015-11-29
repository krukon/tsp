(ns tsp.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [tsp.graph :as graph]
            [tsp.utils :refer [distance]]))

(defn read-points []
  "Returns a lazy sequence of points read from *in*"
  (let [n (read)]
    (for [id (range n)]
      {:x (read)
       :y (read)
       :id id})))

(defn print-result
  "Prints the result of TSP solving functions"
  [{found-path :path
    weight :weight}]
  (println "Found path:")
  (dorun
    (for [v found-path]
      (println (:x v) (:y v))))
  (println "Weight of path:" weight)
  (println "Weight of cycle:" (+ weight
                                 (distance (first found-path)
                                           (last found-path)))))

(defn -main
  [& args]
  (println "Solving Travelling Salesman Problem...")
  (let [points (read-points)
        graph (graph/build-graph points)
        v-1 (first (:vertices graph))
        a*-result (graph/a-star graph v-1)
        greedy-result (graph/greedy graph v-1)]
    (println "A*:")
    (print-result a*-result)
    (println "Greedy:")
    (print-result greedy-result)))
