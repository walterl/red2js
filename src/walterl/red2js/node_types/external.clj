(ns walterl.red2js.node-types.external 
  (:require
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

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
  (def flows walterl.red2js/flows)
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
  (js/node-fn-src (amqp-in-body node nodes) node nodes))

(comment
  (println (n/node->js (n/node-with-id "a8c126e4.90de28" flows) flows))
  (require '[walterl.red2js.helpers :as h])
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
  (js/node-fn-src (objectid-body node) node nodes))

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
  (js/node-fn-src (mongodb-in-body node nodes) node nodes))

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
  (js/node-fn-src (mongodb-out-body node nodes) node nodes))

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
  (js/node-fn-src (mongodb-collection-body node nodes) node nodes))

(comment
  (map (juxt :name :collection) (n/type-nodes "mongodb-collection" flows))
  (def mongo-coll-node (first (n/type-nodes "mongodb-collection" flows)))
  (println (n/node->js mongo-coll-node flows))
  ,)
