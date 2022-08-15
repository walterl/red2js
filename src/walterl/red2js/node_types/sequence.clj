(ns walterl.red2js.node-types.sequence 
  (:require
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

;;; Node type: splt
(defmethod n/js-fn-name "split"
  [{:keys [id name] :as node}]
  (js/identifier ["split" name (:spltType node) id]))

(defn- split-body
  [{:keys [addname arraySplt splt spltType] :as node}]
  (u/join-lines
    [(js/dump-obj-comment
       (select-keys node [:splt :spltType :arraySplt :arraySpltType :stream :addname]))
     "var parts;"
     "if (typeof msg.payload === \"string\") {"
     (js/indent
       (if (#{"str" "bin"} spltType)
         (format "parts = msg.payload.split(%s);" (js/format-value splt))
         ;; Assume "len"
         (format "parts = msg.payload.match(/.{1,%s}/g);" splt)))
     "} else if (Array.isArray(msg.payload)) {"
     (js/indent (format "parts = msg.payload.split(%s);" arraySplt))
     "} else if (typeof msg.payload === \"object\") {"
     (js/indent
       (u/join-lines
         ["parts = Object.entries(msg.payload).map(([k, v] = {"
          (js/indent
            (u/join-lines
              [(when (not-empty addname)
                 (format "v.%s = k;" addname))
               "return v;"]))
          "}));"]))
     "}"
     "msg.parts = parts;"
     "return msg;"]))

(defmethod n/node->js "split"
  [node nodes]
  (js/node-fn-src (split-body node) node nodes))

(comment
  (def flows walterl.red2js/flows)
  (def split-node (first (n/type-nodes "split" flows)))
  (println (n/node->js split-node flows))
  ,)

;;; Node type: join

(defmethod n/js-fn-name "join"
  [{:keys [id build name] :as _node}]
  (js/identifier ["join" name build id]))

(defn- join-body
  [{:keys [joiner property propertyType] :as node}]
  (u/join-lines
    [(js/dump-obj-comment
       (select-keys node [:mode :build :property :propertyType :key :joiner
                          :joinerType :accumulate :timeout :count :reduceRight
                          :reduceExp :reduceInit :reduceInitType :reduceFixup]))
     ""
     "// NOTE: REAL \"join\" NODES DO A LOT MORE THAN THIS!"
     (format "msg.payload = %s.%s.join(%s);" propertyType property joiner)
     "return msg;"]))

(defmethod n/node->js "join"
  [node nodes]
  (js/node-fn-src (join-body node) node nodes))

(comment
  (def join-node (first (n/type-nodes "join" flows)))
  (println (n/node->js join-node flows))
  ,)
