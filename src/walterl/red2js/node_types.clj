(ns walterl.red2js.node-types
  (:require
    [walterl.red2js.nodes :as n]
    [walterl.red2js.node-types.common]
    [walterl.red2js.node-types.external]
    [walterl.red2js.node-types.function]
    [walterl.red2js.node-types.network]))

;;; Node type: tab

(defmethod n/node->js "tab"
  [{:keys [label]} _nodes]
  (str "/// Tab: " label))

(comment
  (def flows walterl.red2js/flows)
  ;; Unsupported node types:
  (->> flows
       (map :type)
       (remove #{"function" "http request" "amqp-broker" "amqp-in" "amqp-out"
                 "no-op" "switch" "inject" "debug"})
       (frequencies))
  ,)
