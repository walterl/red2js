(ns walterl.red2js.node-types.network 
  (:require
    [clojure.string :as str]
    [walterl.red2js.js :as js]
    [walterl.red2js.nodes :as n]
    [walterl.red2js.util :as u]))

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
  (js/node-fn-src (http-in-body node) node nodes))

(comment
  (def flows walterl.red2js/flows)
  (def http-in-node (first (n/type-nodes "http in" flows)))
  (println (n/node->js http-in-node flows))
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
  (js/node-fn-src (http-request-body node) node nodes))

(comment
  (def req-node (first (n/type-nodes "http request" flows)))

  (println (n/node->js req-node flows))

  (->> flows
       (n/type-nodes "http request")
       #_(filter #(not= "use" (:method %)))
       (map n/js-fn-name))
  ,)

;;; Node type: websocket-client

(defmethod n/js-fn-name "websocket-client"
  [{:keys [id] :as _node}]
  (js/identifier ["websocket-client" id]))

(defmethod n/node->js "websocket-client"
  [{:keys [path tls] :as node} _nodes]
  (format "const %s = new WebSocket(%s, %s);"
          (n/js-fn-name node)
          (js/format-value path)
          (js/format-value tls)))

(comment
  (def websocket-client-node (first (n/type-nodes "websocket-client" flows)))
  (println (n/node->js websocket-client-node flows))
  ,)

;;; Node type: websocket-listener

(defmethod n/js-fn-name "websocket-listener"
  [{:keys [id] :as _node}]
  (js/identifier ["websocket-listener" id]))

(defmethod n/node->js "websocket-listener"
  [{:keys [path] :as node} _nodes]
  (format "const %s = new WebSocketServer(%s);"
          (n/js-fn-name node)
          (js/format-value path)))

(comment
  (def websocket-listener-node (cheshire.core/parse-string "{ \"id\": \"312b03e0.93850c\", \"type\": \"websocket-listener\", \"z\": \"\", \"path\": \"wss://ws.example.com:9443\", \"wholemsg\": \"false\" }"
                                                           keyword))
  (println (n/node->js websocket-listener-node flows))
  ,)

;;; Node type: websocket in

(defmethod n/js-fn-name "websocket in"
  [{:keys [id client name server] :as _node}]
  (let [client-server (cond client "client", server "server")]
    (js/identifier ["websocket in" name client-server id])))

(defn- websocket-in-body
  [{:keys [client server] :as node} nodes]
  (u/join-lines
    [(format "%s.onmessage((event) => {"
             (n/js-fn-name (n/node-with-id (or client server) nodes)))
     (js/indent (js/node-calls (n/output-nodes node nodes) ['event.data]))
     "});"]))

(defmethod n/node->js "websocket in"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/params []
              ::js/body (websocket-in-body node nodes)}))

(comment
  (def websocket-in-node (first (n/type-nodes "websocket in" flows)))
  (println (n/node->js websocket-in-node flows))
  ,)

;;; Node type: websocket out

(defmethod n/js-fn-name "websocket out"
  [{:keys [id client name server] :as _node}]
  (let [client-server (cond client "client", server "server")]
    (js/identifier ["websocket out" name client-server id])))

(defn- websocket-out-body
  [{:keys [client server] :as _node} nodes]
  (format "%s.send(msg.payload);"
          (n/js-fn-name (n/node-with-id (or client server) nodes))))

(defmethod n/node->js "websocket out"
  [node nodes]
  (js/fn-src {::js/name (n/js-fn-name node)
              ::js/body (websocket-out-body node nodes)}))

(comment
  (def websocket-out-node (first (n/type-nodes "websocket out" flows)))
  (println (n/node->js websocket-out-node flows))
  ,)
