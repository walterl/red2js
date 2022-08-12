(ns walterl.red2js.node-types
  (:require [clojure.string :as str]
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body func node nodes)}))

(comment
  (def flows walterl.red2js/flows)

  (println (n/node->js (n/node-with-id "f681d1ce.19801" flows) flows))

  (->> flows
       (n/type-nodes "function")
       (map (juxt :id :wires)))
  ,)

;;; Node type: http in

(defmethod n/js-fn-name "http in"
  [{:keys [id method url] :as _node}]
  (js/identifier ["http_in" method url id]))

(defn- http-in-body
  [{:keys [method url] :as _node}]
  (u/join-lines
    [(format "// %s %s" (str/upper-case method) url)
     "return msg.request;"]))

(defmethod n/node->js "http in"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (http-in-body node)
                          node
                          nodes)}))

(comment
  (def http-in-node (first (n/type-nodes "http in" flows)))
  (println (n/node->js http-in-node flows))
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
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

;;; Node type: http response

(defmethod n/js-fn-name "http response"
  [{:keys [id name] :as node}]
  (js/identifier ["http_response" name (:statusCode node) id]))

(defn- http-response-body
  [node]
  (u/join-lines
    ["// https://github.com/node-red/node-red/blob/235690064fe25bba1f5442b59c3cfc3993cb6dc3/packages/node_modules/%40node-red/nodes/core/network/21-httpin.js#L347="
     (format "msg.res._res.status(%s).send(msg.payload);"
             (or (not-empty (str (:statusCode node))) "msg.statusCode || 200"))]))

(defmethod n/node->js "http response"
  [node _nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (http-response-body node)}))

(comment
  (def http-response-node (first (n/type-nodes "http response" flows)))
  (println (n/node->js http-response-node flows))
  ,)

;;; Node type: amqp-broker

(defmethod n/js-fn-name "amqp-broker"
  [{:keys [id] :as _node}]
  (js/identifier ["amqp_broker" id]))

(defmethod n/node->js "amqp-broker"
  [node _nodes]
  (format "const %s = amqp_broker(%s);"
          (n/js-fn-name node)
          (n/config-json node)))

(comment
  (n/type-nodes "amqp-broker" flows)
  (n/config-json (first (n/type-nodes "amqp-broker" flows)))
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (amqp-out-body node nodes)}))

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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (noop-body node nodes)}))

(comment
  (def noop-node (first (n/type-nodes "no-op" flows)))
  (noop-body noop-node flows)
  (println (n/node->js noop-node flows))
  ,)

;;; Node type: objectid

(defmethod n/js-fn-name "objectid"
  [{:keys [id] :as _node}]
  (js/identifier ["objectid" id]))

(defn- objectid-body
  [{prop :selectedProperty, :as _node}]
  (u/join-lines
    [(format "// Will convert all if %s is an array" prop)
     (format "let value = bson.ObjectId(RED.util.getMessageProperty(msg, %s));"
             (js/format-value prop))
     (format "RED.util.setMessageProperty(msg, %s, value, true);" prop)
     "return msg;"]))

(defmethod n/node->js "objectid"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (objectid-body node)
                          node
                          nodes)}))

(comment
  (def objectid-node (first (n/type-nodes "objectid" flows)))
  (println (n/node->js objectid-node flows))
  ,)

;;; Node type: mongodb

(defmethod n/js-fn-name "mongodb"
  [{:keys [id] :as _node}]
  (js/identifier ["mongodb" id]))

(defmethod n/node->js "mongodb"
  [node _nodes]
  (format "const %s = mongodb.connect(%s);"
          (n/js-fn-name node)
          (n/config-json node)))

(comment
  (def mongodb-node (first (n/type-nodes "mongodb" flows)))
  (println (n/node->js mongodb-node flows))
  ,)

;; Node type: mongodb in

(defmethod n/js-fn-name "mongodb in"
  [{:keys [id] :as _node}]
  (js/identifier ["mongodb_in" id]))

(defn- mongodb-in-body
  [{:keys [collection mongodb operation] :as _node} nodes]
  (format "return %s.%s.%s(msg);"
          (n/js-fn-name (n/node-with-id mongodb nodes))
          collection
          operation))

(defmethod n/node->js "mongodb in"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (mongodb-in-body node nodes)
                          node
                          nodes)}))

(comment
  (def mongodb-in-node (first (n/type-nodes "mongodb in" flows)))
  (println (n/node->js mongodb-in-node flows))
  ,)

;;; Node type: mongodb out

(defmethod n/js-fn-name "mongodb out"
  [{:keys [collection id name operation] :as _node}]
  (js/identifier ["mongodb_out" operation (or (not-empty name) collection) id]))

(defn- mongodb-out-body
  [{:keys [collection mongodb operation payonly] :as _node} nodes]
  (format "return %s.%s.%s(%s);"
          (n/js-fn-name (n/node-with-id mongodb nodes))
          collection
          operation
          (if payonly "msg.payload" "msg")))

(defmethod n/node->js "mongodb out"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (mongodb-out-body node nodes)
                          node
                          nodes)}))

(comment
  (def mongodb-out-node (first (n/type-nodes "mongodb out" flows)))
  (println (n/node->js mongodb-out-node flows))
  ,)

;;; Node type: mongodb-config

(defmethod n/js-fn-name "mongodb-config"
  [{:keys [id] :as _node}]
  (js/identifier ["mongodb_config" id]))

(defmethod n/node->js "mongodb-config"
  [node _nodes]
  (format "const %s = mongodb_config.connect(%s);"
          (n/js-fn-name node)
          (n/config-json node {:pretty true})))

(comment
  (def mongodb-config-node (first (n/type-nodes "mongodb-config" flows)))
  (println (n/node->js mongodb-config-node flows))
  ,)

;;; Node type: mongodb-collection

(defmethod n/js-fn-name "mongodb-collection"
  [{:keys [collection id method] :as _node}]
  (js/identifier ["mongodb_collection" method collection id]))

(defn- mongodb-collection-body
  [{:keys [collection config method] :as _node} nodes]
  (u/join-lines
    [(format "const coll = %s;" (if (not-empty collection)
                                  (js/format-value collection)
                                  "msg.collection"))
     (format "return %s[coll].%s(msg);"
             (n/js-fn-name (n/node-with-id config nodes))
             method)]))

(defmethod n/node->js "mongodb-collection"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (mongodb-collection-body node nodes)
                          node
                          nodes)}))

(comment
  (map (juxt :name :collection) (n/type-nodes "mongodb-collection" flows))
  (def mongo-coll-node (first (n/type-nodes "mongodb-collection" flows)))
  (println (n/node->js mongo-coll-node flows))
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (switch-body node nodes)}))

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

;;; Node type: template

(defmethod n/js-fn-name "template"
  [{:keys [id name] :as _node}]
  (js/identifier ["template" name id]))

(defn- rendered-template
  [{:keys [syntax template] :as _node}]
  (if (= "mustache" syntax)
    (u/join-lines
      [(format  "require('mustache').render(%s, {" (js/format-value template))
       (js/indent "flow,")
       (js/indent "global,")
       (js/indent "payload: msg.payload,")
       "});"])
    (js/format-value template)))

(defn- formatted-template-output
  [output-name fmt]
  (cond
    (= "json" fmt) (format "%s = JSON.parse(%s);" output-name output-name)
    (= "yaml" fmt) (format "%s = require('js-yaml').load(%s);"  output-name output-name)))

(defn- template-output-prop
  [{:keys [field], field-type :fieldType}]
  (str/join \. [field-type field]))

(defn- template-body
  [{:keys [output template] :as node}]
  (u/join-lines
    ["// Template:"
     (js/comment-lines template)
     (format "let output = %s;" (rendered-template node))
     (formatted-template-output "output" output)
     (format "%s = output;" (template-output-prop node))
     "return msg;"]))

(defmethod n/node->js "template"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (template-body node)
                          node
                          nodes)}))

(comment
  (def template-node (first (n/type-nodes "template" flows)))
  (def template-node (cheshire.core/parse-string "{\n\"id\": \"85adcedf.a6738\",\n    \"type\": \"template\",\n    \"z\": \"58e157c1.6f5268\",\n    \"name\": \"xxx\",\n    \"field\": \"payload\",\n    \"fieldType\": \"global\",\n    \"format\": \"handlebars\",\n    \"syntax\": \"mustache\",\n    \"template\": \"This is the payload: {{payload}} !\",\n    \"output\": \"json\",\n    \"x\": 1380,\n    \"y\": 400,\n    \"wires\": [\n        []\n    ]\n}"
                                                 true))
  (println (n/node->js template-node flows))
  ,)

;;; Node type: delay

(defmethod n/js-fn-name "delay"
  [{:keys [id name] :as _node}]
  (js/identifier ["delay" name id]))

(defn- delay-body
  [node]
  (u/join-lines
    [(format "delay(%s);"
             (n/config-json
               (select-keys node
                            [:pauseType :timeout :timeoutUnits :rate
                             :nbRateUnits :rateUnits :randomFirst :randomLast
                             :randomUnits :drop])
               {:pretty true}))
     "return msg;"]))

(defmethod n/node->js "delay"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (js/result-passing-body
                          (delay-body node)
                          node
                          nodes)}))

(comment
  (def delay-node (first (n/type-nodes "delay" flows)))
  (println (n/node->js delay-node flows))
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
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (debug-body node)}))

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
