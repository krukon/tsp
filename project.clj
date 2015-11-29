(defproject tsp "0.1.0"
  :description "Traveling Salesman Problem - Heuristic solution using A* alogrithm"
  :url "http://github.com/krukon/tsp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.jordanlewis/data.union-find "0.1.0"]]
  :main ^:skip-aot tsp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
