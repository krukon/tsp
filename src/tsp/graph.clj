(ns tsp.graph
  (:require [clojure.data.priority-map :refer [priority-map]]
            [jordanlewis.data.union-find :refer [union-find get-canonical union]]
            [tsp.utils :refer :all]))

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

(defn get-edges
  "Return edges adjacent to given vertex"
  [graph vertex]
  (get (:edges graph)
       (:id vertex)))

(defn g
  "Computes the length of path extended by edge"
  [edge path-weight]
  (+ path-weight
     (:weight edge)))

(defn h
  "Computes the value of heuristic function"
  [subgraph v-edges v-1]
  (let [c-mst (mst-weight subgraph)
        vertices (:vertices subgraph)
        min-v-w (if (empty? v-edges)
                  0
                  (reduce min (map :weight v-edges)))
        min-v1-w (if (empty? vertices)
                   0
                   (reduce min (map (partial distance v-1)
                                    vertices)))]
    (+ min-v-w
       c-mst
       min-v1-w)))

(defn f
  "Function used to evaluate vertices used for extending path.
  f(v) = g(v) + h(v)"
  [edge graph path-weight v-1]
  (let [v (:target edge)
        v-edges (get-edges graph v)
        subgraph (remove-vertex graph v)]
    {:value (+ (g edge
                  path-weight)
               (h subgraph
                  v-edges
                  v-1))
     :subgraph subgraph
     :vertex v
     :weight (:weight edge)}))

(defn update-priority-queue
  "Adds/updates (:target edge) to priority queue using cost function f"
  [graph path-weight v-1 queue edge]
  (let [cost (f edge
                graph
                path-weight
                v-1)]
    (assoc queue
           (:vertex cost)
           [(:value cost)
            (:weight cost)
            (:subgraph cost)
            (get-edges graph (:vertex cost))])))

(defn a-star
  "Computes aproximation of travelling salesman path on a given full graph
  starting from vertex v-1.

  Uses A* heuristic algorithm with f as cost function.

  Priority queue contains vertices as keys and vectors as a value.
  Each value vector contains cost, weight of last traversed edge,
  subgraph with left vertices, and a list of outgoing edged.
  Last 3 additional values are used to compute the value of cost function."
  [graph v-1]
  (loop [found-path []
         path-weight 0
         queue (priority-map v-1 [0
                                  0
                                  (remove-vertex graph v-1)
                                  (get-edges graph v-1)])]
    (if (empty? queue)
      {:path found-path
       :weight path-weight}
      (let [[v [_ edge-weight graph v-edges]] (peek queue)
            queue (reduce (partial update-priority-queue
                                   graph
                                   path-weight
                                   v-1)
                          (pop queue)
                          v-edges)]
        (recur (conj found-path v)
               (+ path-weight edge-weight)
               queue)))))

(defn greedy
  "Computes travelling salesman path using a greedy algorithm. Chooses
  the closest vertex to the last vertex on already found path."
  [graph v-1]
  (loop [found-path [v-1]
         path-weight 0
         graph graph]
    (if (>= 1 (count (:vertices graph)))
      {:path found-path
       :weight path-weight}
      (let [v (last found-path)
            edges (get-edges graph v)
            min-edge (apply min-key :weight edges)
            w (:target min-edge)]
        (recur (conj found-path w)
               (+ path-weight (:weight min-edge))
               (remove-vertex graph v))))))
