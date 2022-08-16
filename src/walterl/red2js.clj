(ns walterl.red2js
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [walterl.red2js.js :as js]
            [walterl.red2js.nodes :as n]
            [walterl.red2js.node-types]
            [walterl.red2js.util :as u])
  (:gen-class))

(defn- load-nodes
  [filename]
  (json/parse-string (slurp filename) keyword))

(defn- useful-metadata-pair?
  [[k _v]]
  ((complement #{:func :x :y :w :h}) k))

(defn- metadata-js-comment
  [metadata]
  (->> metadata
       (filter useful-metadata-pair?)
       (map js/pair->comment-line)
       (u/join-lines)))

(defn- annotate-node
  [node nodes]
  (assoc node
         ::js (n/node->js node nodes)
         ::js-comment (metadata-js-comment node)))

(defn- convert-nodes
  [nodes all-nodes]
  (->> nodes
       (map #(annotate-node % all-nodes))
       (map ::js)
       (remove nil?)
       (str/join "\n\n")))

(defn- convert-nodes-on-tab
  [tab-node nodes]
  (-> (n/nodes-on-tab tab-node nodes)
      (->> (sort-by (juxt :y :x))
           (into [tab-node]))
      (convert-nodes nodes)))

(defn- convert-flows
  [nodes]
  (str/join "\n\n"
            (into [(convert-nodes (n/untabbed-nodes nodes) nodes)]
                  (map #(convert-nodes-on-tab % nodes) (n/type-nodes "tab" nodes)))))

(defn convert-flows-file
  "Callable entry point to the application."
  [{:keys [filename]}]
  (convert-flows (load-nodes filename)))

(defn -main
  "Convert flows.json to JavaScript pseudo code."
  [& args]
  (println (convert-flows-file {:filename (first args)})))

(comment
  (convert-flows-file {:filename "./flows.json"})
  (def flows (load-nodes "flows.json"))
  (count flows)

  (->> flows
       #_(n/type-nodes "function")
       (convert-nodes flows))

  (->> flows
       (n/type-nodes "function")
       (take 3)
       (map #(annotate-node % flows)))

  (->> flows
       (n/type-nodes "http request")
       (take-last 2)
       (convert-nodes flows)
       (println))

  (->> flows
       (n/type-nodes "switch")
       (take-last 2)
       (map #(annotate-node % flows))
       (map ::js)
       (str/join "\n\n")
       (println))

  (->> flows
       (n/downstream-nodes "c4a1325.c0783d")
       (into ["c4a1325.c0783d"])
       (reverse)
       (map #(n/node-with-id % flows))
       (convert-nodes flows)
       (println))
  ,)
