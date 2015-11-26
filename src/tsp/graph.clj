(ns tsp.graph
  (:require [jordanlewis.data.union-find :refer [union-find get-canonical union]]
            [tsp.utils :refer :all]))

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

(defn mst-weight
  "Computes the weight of MST for a given graph using Kruskal's algorithm"
  [graph]
  (loop [uf (apply union-find (:vertices graph))
         edges (sort-by :weight (get-edges-coll graph))
         weight 0]
    (if (empty? edges)
      weight
      (let [edge (first edges)
            u (:source edge)
            v (:target edge)
            uf-u (get-canonical uf u)
            uf-v (get-canonical (first uf-u) v)]
        (if (= (second uf-u)
               (second uf-v))
          (recur (first uf-v) (rest edges) weight)
          (recur (union (first uf-v) u v)
                 (rest edges)
                 (+ weight
                    (:weight edge))))))))
