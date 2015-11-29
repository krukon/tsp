(ns tsp.graph
  (:require [tsp.utils :refer [distance]]))

(defn build-graph
  "Constructs a full graph from provided points.
  Under :vertices key there is a list of vertices.
  :edges contains a map of vertex id to edge.
  Each edge contins :source, :target and :weight"
  [points]
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

(defn remove-vertex
  "Returns a subgraph of graph without vertex"
  [graph vertex]
  (let [remove-edge (fn [coll [v e-coll]]
                      (concat coll [v (remove #(= (:target %)
                                                  vertex)
                                              e-coll)]))
        edges (dissoc (:edges graph) (:id vertex))
        edges (reduce remove-edge [] edges)
        edges (apply hash-map edges)]
    {:vertices (remove #{vertex} (:vertices graph))
     :edges edges}))

(defn get-edges-coll
  "Returns a collection of edges from the graph"
  [graph]
  (reduce concat (map second (:edges graph))))

(defn get-edges
  "Return edges adjacent to given vertex"
  [graph vertex]
  (get (:edges graph)
       (:id vertex)))
