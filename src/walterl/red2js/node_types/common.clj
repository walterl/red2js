(ns walterl.red2js.node-types.common 
  (:require
    [clojure.string :as str]
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

;;; Node type: inject

(defmethod n/js-fn-name "inject"
  [{:keys [id once] :as _node}]
  (js/identifier ["inject" (when once "once") id]))

(defn- inject-msg
  [{:keys [topic] :as node}]
  (u/join-lines
    ["const msg = {"
     (when (= "date" (:payloadType node))
       (js/indent "payload: new Date(),"))
     (when (not-empty topic)
       (js/indent (format "topic: %s" (pr-str topic))))
     "};"]))

(defn- inject-body
  [{repeatt :repeat, :as node} nodes]
  (let [body (u/join-lines
               [(js/sleep (u/->double (:onceDelay node)))
                (inject-msg node)
                (js/node-calls (n/output-nodes node nodes))])]
    (if (not-empty repeatt)
      (js/repeated-body body repeatt)
      body)))

(defmethod n/node->js "inject"
  [node nodes]
  (js/fn-src {::js/params []
              ::js/name (n/js-fn-name node)
              ::js/body (inject-body node nodes)}))

(comment
  (def flows walterl.red2js/flows)
  (println (inject-body (n/node-with-id "5440c81d.695b88" flows) flows))
  (println (inject-body (n/node-with-id "2ede69c8.94e106" flows) flows))
  (println (inject-body (n/node-with-id "e7b81840.e6bba8" flows) flows))

  (println (n/node->js (n/node-with-id "5440c81d.695b88" flows) flows))
  (println (n/node->js (n/node-with-id "2ede69c8.94e106" flows) flows))
  (println (n/node->js (n/node-with-id "e7b81840.e6bba8" flows) flows))

  (->> flows
       (n/type-nodes "inject")
       (map :topic)
       (frequencies))

  (require '[walterl.red2js.helpers :as h])
  (h/val-freqs (n/type-nodes "inject" flows))
  ,)

;;; Node type: debug

(defmethod n/js-fn-name "debug"
  [{:keys [id name] :as _node}]
  (js/identifier ["debug" name id]))

(defn- debug-target
  [node]
  (cond
    (= "msg" (:targetType node)) (str "msg." (:complete node))
    :else "msg"))

(defn- debug-body
  [{:keys [console tosidebar tostatus] :as node}]
  (u/join-lines
    [(format "const out = %s;" (debug-target node))
     (when console "console.debug(out);")
     (when tosidebar "sidebar.debug(out);")
     (when tostatus
       (format "set_node_status(%s, %s);"
               (:id node)
               (pr-str (:statusVal node))))]))

(defmethod n/node->js "debug"
  [node _nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (debug-body node)}))

(comment
  (->> flows
       (n/type-nodes "debug")
       (map :statusType)
       frequencies)
  ,)

;;; Node type: status

(defmethod n/js-fn-name "status"
  [{:keys [id] :as _node}]
  (js/identifier ["status" id]))

(defn- status-selected-nodes
  [scope]
  (if scope
    (format "[%s].map(RED.nodes.getNode);" (js/format-values scope))
    "RED.nodes.getNodeList();"))

(defn- status-body
  [{:keys [scope] :as node} nodes]
  (let [out-fn-name (n/js-fn-name (first (n/output-nodes node nodes)))]
    (u/join-lines
      [(str "const selected = " (status-selected-nodes scope))
       "for (const n of selected) {"
       (js/indent (format "%s(n);" out-fn-name))
       "}"])))

(defmethod n/node->js "status"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (status-body node nodes)}))

(comment
  (def status-node (first (n/type-nodes "status" flows)))
  (status-body status-node flows)

  (println (n/node->js status-node flows))
  (let [status-node (assoc status-node :scope (map :id (vec (take 3 flows))))]
    (println (n/node->js status-node flows)))
  ,)

;;; Node type: link in

(defmethod n/js-fn-name "link in"
  [{:keys [id name] :as _node}]
  (js/identifier ["link_in" name id]))

(defn- link-in-body
  [{:keys [links] :as node} nodes]
  (u/join-lines
    [(format "// Linked from %s" (str/join ", " links))
     (js/node-calls (n/output-nodes node nodes))]))

(defmethod n/node->js "link in"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (link-in-body node nodes)}))

(comment
  (def link-in-node (first (n/type-nodes "link in" flows)))
  (println (n/node->js link-in-node flows))
  ,)

;;; Node type: link out

(defmethod n/js-fn-name "link out"
  [{:keys [id name] :as _node}]
  (js/identifier ["link_out" name id]))

(defmethod n/node->js "link out"
  [{:keys [links] :as node} nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (->> links
                             (mapv #(n/node-with-id % nodes))
                             js/node-calls)}))

(comment
  (def link-out-node (first (n/type-nodes "link out" flows)))
  (js/node-calls (mapv #(n/node-with-id % flows) (:links link-out-node)))
  (println (n/node->js link-out-node flows))
  ,)

;;; Node type: comment

(defmethod n/node->js "comment"
  [{:keys [name info]} _nodes]
  (u/join-lines
    [(str "/// COMMENT: " name)
     (when (not-empty info)
       (js/comment-lines info))]))
