(ns walterl.red2js.node-types
  (:require
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.node-types.common]
    [walterl.red2js.node-types.external]
    [walterl.red2js.node-types.function]
    [walterl.red2js.node-types.network]
    [walterl.red2js.node-types.parser]
    [walterl.red2js.node-types.sequence]
    [walterl.red2js.util :as u]))

;;; Node type: comment

(defmethod n/node->js "comment"
  [{:keys [name info]} _nodes]
  (u/join-lines
    [name
     (when info
       (js/comment-lines info))]))

;;; Node type: tab

(defmethod n/node->js "tab"
  [{:keys [label]} _nodes]
  (str "/// Tab: " label))
