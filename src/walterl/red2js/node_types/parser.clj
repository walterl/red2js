(ns walterl.red2js.node-types.parser 
  (:require
    [clojure.string :as str]
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

;;; Node type: html

(defmethod n/js-fn-name "html"
  [{:keys [id name tag] :as _node}]
  (js/identifier ["html" name tag id]))

(defn- html-body
  [{:keys [property tag ret outproperty] :as _node}]
  (u/join-lines
    [(format "const dom = new DOM(msg.%s);" property)
     (format "const elem = dom.querySelector(%s);" (js/format-value tag))
     (format "const ret = %s;"
             (condp = ret
               "html" "elem.outerHTML"
               "text" "elem.innerHTML"
               "attr" "elem.attributes"))
     (format "msg.%s = ret;" outproperty)
     "return msg;"]))

(defmethod n/node->js "html"
  [node nodes]
  (js/node-fn-src (html-body node) node nodes))

(comment
  (def flows walterl.red2js/flows)
  (def html-node (first (n/type-nodes "html" flows)))
  (println (n/node->js html-node flows))
  ,)

;;; Node type: json

(defmethod n/js-fn-name "json"
  [{:keys [id action name] :as _node}]
  (js/identifier ["json" name action id]))

(defn- json-convert-expression
  [action prop]
  (condp = action
    "" (u/join-lines
         [(format "if (typeof(msg.%s) == \"object\") {" prop)
          (js/indent (format "msg.%s = JSON.stringify(msg.%s)" prop prop))
          (format "} else if (typeof(msg.%s) == \"string\") {" prop)
          (js/indent (format "msg.%s = JSON.parse(msg.%s)" prop prop))
          "}"])
    "str" (format "JSON.stringify(msg.%s)" prop)
    "obj" (format "JSON.parse(msg.%s)" prop)))

(defn- json-body
  [{:keys [action property] :as _node}]
  (u/join-lines
    [(format "msg.%s = %s;" property (json-convert-expression action property))
     "return msg;"]))

(defmethod n/node->js "json"
  [node nodes]
  (js/node-fn-src (json-body node) node nodes))

(comment
  (def json-node (first (n/type-nodes "json" flows)))
  (println (n/node->js json-node flows))
  ,)
