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

(defn -main
  [& args]
  (println "Solving Travelling Salesman Problem...")
  (let [points (read-points)
        graph (graph/build-graph points)
        result (graph/a-star graph (first (:vertices graph)))]
    (println "Found path:")
    (dorun
      (for [v (:path result)]
        (println (:x v) (:y v))))
    (println "Weight of path:" (:weight result))
    (println "Weight of cycle:" (+ (:weight result)
                                   (distance (first (:path result))
                                             (last (:path result)))))))
