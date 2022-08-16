(ns walterl.red2js.node-types
  (:require
    [walterl.red2js.nodes :as n]
    [walterl.red2js.node-types.common]
    [walterl.red2js.node-types.external]
    [walterl.red2js.node-types.function]
    [walterl.red2js.node-types.network]
    [walterl.red2js.node-types.parser]
    [walterl.red2js.node-types.sequence]))

;;; Node type: tab

(defmethod n/node->js "tab"
  [{:keys [label]} _nodes]
  (str "/// Tab: " label))
