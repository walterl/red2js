(ns walterl.red2js.node-types.function 
  (:require
    [clojure.string :as str]
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

;;; Node type: function

(defmethod n/js-fn-name "function"
  [{:keys [id name] :as _node}]
  (js/identifier ["function" name id]))

(defmethod n/node->js "function"
  [{:keys [func] :as node} nodes]
  (js/node-fn-src func node nodes))

(comment
  (def flows walterl.red2js/flows)

  (println (n/node->js (n/node-with-id "f681d1ce.19801" flows) flows))

  (->> flows
       (n/type-nodes "function")
       (map (juxt :id :wires)))
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

;;; Node type: change

(defmethod n/js-fn-name "change"
  [{:keys [id name rules] :as _node}]
  (let [actions (->> (map :t rules)
                     (str/join "$"))]
    (js/identifier ["change" name actions id])))

(defn- change-rule-statement
  [{:keys [pt p tot to], rule-type :t :as rule}]
  (condp = rule-type
    "set" (format "%s.%s = %s.%s;" pt p tot to)
    ;; "change" support is incomplete: the arguments to '.replace' are assumed
    ;; to be string values, and not references to other objects.
    "change" (format "%s.%s = %s.%s.replace(%s, %s);"
                     pt p ; %s.%s = ...
                     pt p ; ... = %s.%s.replace...
                     (js/format-value (:from rule)) (js/format-value to)) ; ...replace(%s, %s)
    "delete" (format "delete %s.%s;" pt p)
    "move" (u/join-lines
             [(format "%s.%s = %s.%s;" tot to pt p)
              (format "delete %s.%s;" pt p)])))

(defn- change-body
  [{:keys [rules] :as _node}]
  (u/join-lines
    (into (mapv change-rule-statement rules)
          ["return msg;"])))

(defmethod n/node->js "change"
  [node nodes]
  (js/node-fn-src (change-body node) node nodes))

(comment
  (def change-node (first (n/type-nodes "change" flows)))
  (def change-node (cheshire.core/parse-string "{ \"id\": \"1b7e8b0b.2c6f95\", \"type\": \"change\", \"z\": \"6ab5d417.e92fac\", \"name\": \"\", \"rules\": [ { \"t\": \"set\", \"p\": \"payload\", \"pt\": \"msg\", \"to\": \"payload.data\", \"tot\": \"msg\" }, { \"t\": \"change\", \"p\": \"payload\", \"pt\": \"msg\", \"from\": \"srch\", \"fromt\": \"str\", \"to\": \"repl\", \"tot\": \"str\" }, { \"t\": \"delete\", \"p\": \"payload\", \"pt\": \"msg\" }, { \"t\": \"move\", \"p\": \"payload\", \"pt\": \"msg\", \"to\": \"to_payload\", \"tot\": \"msg\" } ], \"action\": \"\", \"property\": \"\", \"from\": \"\", \"to\": \"\", \"reg\": false, \"x\": 400, \"y\": 880, \"wires\": [ [ \"32ce3710.8f8ba8\" ] ] }" true))
  (println (n/node->js change-node flows))
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
  (js/node-fn-src (template-body node) node nodes))

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
  (js/node-fn-src (delay-body node) node nodes))

(comment
  (def delay-node (first (n/type-nodes "delay" flows)))
  (println (n/node->js delay-node flows))
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
