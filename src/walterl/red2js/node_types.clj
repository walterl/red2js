(ns walterl.red2js.node-types
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [walterl.red2js.helpers :as h]
            [walterl.red2js.js :as js]
            [walterl.red2js.nodes :as n]
            [walterl.red2js.util :as u]))

;;; Node type: function

(defmethod n/js-fn-name "function"
  [{:keys [id name] :as _node}]
  (js/identifier ["function" name id]))

(defmethod n/node->js "function"
  [{:keys [func] :as node} nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (js/result-passing-body func node nodes)}))

(comment
  (def flows walterl.red2js/flows)

  (println (n/node->js (n/node-with-id "f681d1ce.19801" flows) flows))

  (->> flows
       (n/type-nodes "function")
       (map (juxt :id :wires)))
  ,)

;;; Node type: http request

(defmethod n/js-fn-name "http request"
  [{:keys [id method name ret] :as _node}]
  (let [base-name (if (= "obj" ret)
                    "http_request_json"
                    "http_request")]
    (js/identifier [base-name method name id])))

(defn- http-request-body
  [{:keys [method ret url] :as _node}]
  (str "return "
       (if (= "use" method)
         (js/fetch 'msg.method 'msg.url ret)
         (js/fetch method url ret))))

(defmethod n/node->js "http request"
  [node nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (js/result-passing-body
                      (http-request-body node)
                      node
                      nodes)}))

(comment
  (def req-node (first (n/type-nodes "http request" flows)))

  (println (n/node->js req-node flows))

  (->> flows
       (n/type-nodes "http request")
       #_(filter #(not= "use" (:method %)))
       (map n/js-fn-name))
  ,)

;;; Node type: amqp-broker

(defmethod n/js-fn-name "amqp-broker"
  [{:keys [id] :as _node}]
  (js/identifier ["amqp_broker" id]))

(defn- amqp-broker-args
  [node]
  (-> (dissoc node :id :type)
      (json/generate-string)))

(defmethod n/node->js "amqp-broker"
  [node _nodes]
  (format "const %s = amqp_broker(%s);"
          (n/js-fn-name node)
          (amqp-broker-args node)))

(comment
  (amqp-broker-args (first (n/type-nodes "amqp-broker" flows)))
  (println (n/node->js (first (n/type-nodes "amqp-broker" flows))
                       flows))
  ,)

;;; Node type: amqp-in

(defmethod n/js-fn-name "amqp-in"
  [{:keys [id] :as node}]
  (js/identifier ["amqp_in" (:exchangeRoutingKey node) id]))

(defn- amqp-in-body
  [{:keys [broker] :as node} nodes]
  (u/join-lines
    ["return amqp_in({"
     (js/indent (format "exchangeRoutingKey: %s," (pr-str (:exchangeRoutingKey node))))
     (js/indent (format "exchangeName: %s," (pr-str (:exchangeName node))))
     (js/indent (format "exchangeType: %s," (pr-str (:exchangeType node))))
     (js/indent (format "exchangeDurable: %s," (:exchangeDurable node)))
     (js/indent (format "noAck: %s," (:noAck node)))
     (js/indent (format "queueName %s," (:queueName node)))
     (js/indent (format "broker: %s," (n/js-fn-name (n/node-with-id broker nodes))))
     "});"]))

(defmethod n/node->js "amqp-in"
  [node nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (js/result-passing-body
                      (amqp-in-body node nodes)
                      node
                      nodes)}))

(comment
  (println (n/node->js (n/node-with-id "a8c126e4.90de28" flows) flows))
  (h/val-freqs (n/type-nodes "amqp-in" flows))
  ,)

;;; Node type: amqp-out

(defmethod n/js-fn-name "amqp-out"
  [{:keys [id] :as node}]
  (js/identifier ["amqp_out" (:exchangeRoutingKey node) id]))

(defn- amqp-out-body
  [{:keys [broker] :as node} nodes]
  (u/join-lines
    ["amqp_out({"
     (js/indent "msg: msg.payload,")
     (js/indent (format "exchangeRoutingKey: %s," (pr-str (:exchangeRoutingKey node))))
     (js/indent (format "exchangeName: %s," (pr-str (:exchangeName node))))
     (js/indent (format "exchangeType: %s," (pr-str (:exchangeType node))))
     (js/indent (format "exchangeDurable: %s," (:exchangeDurable node)))
     (js/indent (format "noAck: %s," (:noAck node)))
     (js/indent (format "broker: %s," (n/js-fn-name (n/node-with-id broker nodes))))
     "});"]))

(defmethod n/node->js "amqp-out"
  [node nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (amqp-out-body node nodes)}))

(comment
  (def ao-node (first (n/type-nodes "amqp-out" flows)))
  (println (amqp-out-body ao-node flows))
  (h/val-freqs (n/type-nodes "amqp-out" flows))
  ,)

;;; Node type: no-op

(defmethod n/js-fn-name "no-op"
  [{:keys [id] :as _node}]
  (js/identifier ["noop" id]))

(defn- noop-body
  [node nodes]
  (js/node-calls (n/output-nodes node nodes)))

(defmethod n/node->js "no-op"
  [node nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (noop-body node nodes)}))

(comment
  (def noop-node (first (n/type-nodes "no-op" flows)))
  (noop-body noop-node flows)
  (println (n/node->js noop-node flows))
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
  (js/fn-src {:name (n/js-fn-name node)
              :body (status-body node nodes)}))

(comment
  (def status-node (first (n/type-nodes "status" flows)))
  (status-body status-node flows)

  (println (n/node->js status-node flows))
  (let [status-node (assoc status-node :scope (map :id (vec (take 3 flows))))]
    (println (n/node->js status-node flows)))
  ,)

;;; Node type: switch

(defmethod n/js-fn-name "switch"
  [{:keys [id name] :as _node}]
  (js/identifier ["switch" name id]))

(defn- typed-literal
  [value vtype]
  (cond
    (= "str" vtype) (pr-str value)
    :else (str value)))

(defn- rule-check-condition
  [prop rule]
  (let [rule-type-operators {"eq" "=="}]
    (str/join \space [prop
                      (rule-type-operators (:t rule))
                      (typed-literal (:v rule) (:vt rule))])))

(defn- rule-check-statement
  [rule test-prop callee return-on-match]
  (u/join-lines
    [(str "if (" (rule-check-condition test-prop rule) ") {")
     (js/indent (str callee "(msg);"))
     (when return-on-match
       (js/indent "return;"))
     "}"]))

(defn- switch-body
  [{:keys [checkall property rules wires] :as node} nodes]
  (let [test-prop (str (:propertyType node) "." property)
        callees (->> (map first wires) ; remove wrapping vector
                     (mapv #(n/js-fn-name (n/node-with-id % nodes))))]
    (u/join-lines
      (map (fn [r c] (rule-check-statement r test-prop c (not= "true" checkall)))
           rules callees))))

(defmethod n/node->js "switch"
  [node nodes]
  (js/fn-src {:name (n/js-fn-name node)
              :body (switch-body node nodes)}))

(comment
  (def switch-node
    (->> flows
         (n/type-nodes "switch")
         (filter #(= 2 (:outputs %)))
         first))
  (println (switch-body switch-node flows))
  (println (n/node->js switch-node flows))

  (->> flows
       (n/type-nodes "switch")
       (filter #(= 2 (:outputs %)))
       first
       :wires
       (map first)
       (map #(n/node-with-id % flows))
       (map n/js-fn-name))

  (->> flows
       (n/type-nodes "switch")
       (map :rules)
       #_(filter #(= 2 (:outputs %)))
       )
  ,)

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
               [(js/sleep (:onceDelay node))
                (inject-msg node)
                (js/node-calls (n/output-nodes node nodes))])]
    (if (not-empty repeatt)
      (js/repeated-body body repeatt)
      body)))

(defmethod n/node->js "inject"
  [node nodes]
  (js/fn-src {:params []
              :name (n/js-fn-name node)
              :body (inject-body node nodes)}))

(comment
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

  (h/val-freqs (n/type-nodes "inject" flows))
  ,)

;;; Node type: tab

(defmethod n/node->js "tab"
  [{:keys [label]} _nodes]
  (str "/// Tab: " label))

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
  (js/fn-src {:name (n/js-fn-name node)
              :body (debug-body node)}))

(comment
  (println (n/node->js (n/node-with-id "94f1227c.ba63" flows) flows))

  (->> flows
       (n/type-nodes "debug")
       (map :statusType)
       frequencies)
  ,)

(comment
  ;; Unsupported node types:
  (->> flows
       (map :type)
       (remove #{"function" "http request" "amqp-broker" "amqp-in" "amqp-out"
                 "no-op" "switch" "inject" "debug"})
       (frequencies))
  ,)
