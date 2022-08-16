(ns walterl.red2js.nodes
  (:require [clojure.set :as set]
            [cheshire.core :as json]
            [walterl.red2js.loopr :refer [loopr]]))

;;; Multi-methods

(defmulti js-fn-name
  "Determine JavaScript function name for the given node.
  Dispatched on node `:type`."
  :type)

(defmethod js-fn-name :default
  [_node]
  nil)

(defmulti node->js
  "Convert Node-RED node's JSON to JavaScript source code string.
  Dispatched on node `:type`."
  (fn [node _nodes]
    (:type node)))

(defmethod node->js :default
  [node _nodes]
  (binding [*out* *err*]
    (println (format "[WARNING] Unsupported node type: %s (id %s)"
                     (:type node)
                     (:id node))))
  nil)

;;; Node selection

(defn node-with-id
  "Returns node in `nodes` with `id`."
  [id nodes]
  (when (not-empty id)
    (first (filter #(= id (:id %)) nodes))))

(defn output-node-ids
  "Returns IDs of `node`'s output nodes."
  [{:keys [wires] :as _node}]
  (reduce into [] wires))

(defn output-nodes
  "Returns `node`'s output nodes."
  [node nodes]
  (mapv #(node-with-id % nodes) (output-node-ids node)))

(defn type-nodes
  "Filters `nodes` for those with `:type` of `ntype`."
  [ntype nodes]
  (filter #(= ntype (:type %)) nodes))

(defn- assoc-node-connections
  [conns {:keys [id] :as node}]
  (let [targets (set (output-node-ids node))
        conns (update conns id set/union targets)] ; Add targets to id mapping
    (reduce #(update %1 %2 set/union #{id}) conns targets))) ; Add id to target mappings

(defn all-connections
  "Returns a mapping of node ID to the set of node IDs connected to it."
  [nodes]
  (reduce assoc-node-connections {} nodes))

(defn output-inputs
  "Returns map of output node to nodes that provide input to it."
  [nodes]
  (loopr [acc {}]
         [node nodes
          output-id (output-node-ids node)]
         (recur (update acc output-id set/union #{(:id node)}))))

(defn collect-nodes
  "Collects nodes by repeatedly calling `traverse` on `node-id` to get a set of
  nodes adjacent to node with `node-id`.
  Used by `local-graph-nodes` and `downstream-nodes`."
  [node-id traverse]
  (loop [queue (traverse node-id)
         collected []]
    (let [current-id (first queue)
          collected (cond-> collected (some? current-id) (conj current-id))
          neighbors (filterv (complement (set collected)) (traverse current-id))
          queue (into neighbors (vec (rest queue)))] ; neighbors insert at head
      (if (empty? queue)
        collected
        (recur queue collected)))))

(defn local-graph-nodes
  "Returns all nodes from `nodes` that are in the same graph as the node with
  `node-id`."
  [node-id nodes]
  (let [conns (all-connections nodes)]
     (collect-nodes node-id (fn [id] (get conns id)))))

(defn downstream-nodes
  "Returns all nodes downstream (on the output side) of `node-id`'s node graph."
  [node-id nodes]
  (collect-nodes node-id #(output-node-ids (node-with-id % nodes))))

(comment
  (def flows walterl.red2js/flows)

  (output-inputs (->> (downstream-nodes "c4a1325.c0783d" flows)
                      (into ["c4a1325.c0783d"])
                      (map #(node-with-id % flows))))

  (downstream-nodes "22cbd68c.ec6a7a" flows) ; debug "msg"

  (downstream-nodes "790daae1.035df4" flows) ; amqp-out
  (downstream-nodes "5ca9f393.39c76c" flows) ; function
  (downstream-nodes "ecef2b2b.027378" flows) ; http request
  (downstream-nodes "bae0a2b2.28626" flows) ; switch

  (downstream-nodes "c4a1325.c0783d" flows) ; Eventbus no-op

  (count (downstream-nodes "ab09931.065fd7" flows)) ; switch
  (map #(js-fn-name (node-with-id % flows)) (downstream-nodes "ab09931.065fd7" flows))

  ;; The local graph sets of all nodes in the graph, are equal
  (->> (map #(set (local-graph-nodes % flows))
            ["ae1027.2a456fd8"
             "a0c5e1b4.f25e"
             "c20449c8.eb1fb8"
             "6ea8a4f3.430ecc"
             "96bfbdf6.b5f83"
             "b3644a05.1c8ef8"
             "7d13c4d7.6db2cc"
             "94f1227c.ba63"
             "caf5042a.1f7378"
             "a8c126e4.90de28"])
       (apply =))

  (local-graph-nodes "a8c126e4.90de28" flows)

  (count (local-graph-nodes "c8b9babd.fa30f8" flows))

  (filter (fn [[_ v]] (nil? v)) (all-connections flows))
  ,)

(defn untabbed-nodes
  "Nodes that don't represent tabs, and aren't on any tab.

  Normally what's left are configuration nodes."
  [nodes]
  (let [tab-nodes (type-nodes "tab" nodes)
        tab-node-ids (set (map :id tab-nodes))
        tabbed-nodes (filter #(or (tab-node-ids (:id %)) (tab-node-ids (:z %))) nodes)
        tabbed-node-ids (set (map :id tabbed-nodes))]
    (remove (comp tabbed-node-ids :id) nodes)))

(defn nodes-on-tab
  "Nodes on the specified `_tab-node`."
  [{tab-node-id :id :as _tab-node} nodes]
  (filter #(= tab-node-id (:z %)) nodes))

;;; Utilities

(defn config-json
  "Returns node config as a JSON string; all fields except `:id`, `:name` and `:type`."
  ([node]
   (config-json node nil))
  ([node opts]
   (-> (dissoc node :id :name :type)
       (json/generate-string opts))))

(defn- comment-pos
  [nodes {cy :y}]
  (-> (reduce (fn [{prev-x :x, prev-y :y, :as prev} [i {:keys [x y]}]]
                (if (or (and (<= cy y) (< y prev-y))
                        (and (= prev-y y) (< x prev-x)))
                  {:x x, :y y, :index i}
                  prev))
              {:x Long/MAX_VALUE, :y Long/MAX_VALUE}
              (map-indexed vector nodes))
      :index))

(defn- sort-comment-node
  [nodes comm]
  (let [nodes* (remove #(= % comm) nodes)
        ins-pos (comment-pos nodes* comm)
        [before after] (split-at ins-pos nodes*)]
    (reduce into [] [before [comm] after])))

(defn sort-comments
  "Sort comment nodes based on the `nodes`'s positions.

  Returns a vector of `nodes`, reordered such that each comment node is in a
  position before the node directly \"below\" it (X- and Y-coordinates greater
  than the comment's)."
  [nodes]
  (reduce sort-comment-node
          nodes
          (type-nodes "comment" nodes)))

(comment
  (def nodes
    [{:x 100, :y 300, :id "line 3"}
     {:x 100, :y 200, :id "line 2"}
     {:x 200, :y 100, :id "line 1, second"}
     {:x 100, :y 100, :id "line 1, first"}
     {:x 0,   :y 99,  :id "comment1", :type "comment", :name "Comment node"}
     {:x 110, :y 100,  :id "comment2", :type "comment", :name "Comment node"}])
  (sort-comments nodes)
  ,)
