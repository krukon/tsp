(ns tsp.graph
  (:require [tsp.utils :refer :all]))

(defn build-graph
  [points]
  "Constructs a full graph from provided points.
  Under :vertices key there is a list of vertices.
  :edges contains a map of vertex id to edge.
  Each edge contins :source, :target and :weight"
  {:vertices points
   :edges (apply merge-with
                 (cons
                  concat
                  (for [u points
                        v points
                        :when (not= (:id u)
                                    (:id v))]
                    {(:id u)
                     [{:source u
                       :target v
                       :weight (distance u v)}]})))})
