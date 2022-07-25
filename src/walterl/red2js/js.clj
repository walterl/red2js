(ns walterl.red2js.js
  (:require [clojure.string :as str]
            [walterl.red2js.nodes :as n]
            [walterl.red2js.util :as u]))

(defn pair->comment-line
  "JavaScript comment with formatted key-value pair."
  [[k v]]
  (format "// %s: %s" (name k) (pr-str v)))

(defn- safe-for-identifier
  [s]
  (-> s
      (str/replace " " "_")
      (str/replace "-" "_")
      (str/replace "|" "_")
      (str/replace "." "$")
      (str/replace "#" "$")))

(defn identifier
  "Creates valid JavaScript identifier from non-empty `parts`."
  [parts]
  (->> parts
       (map str)
       (filter not-empty)
       (map safe-for-identifier)
       (str/join "__")))

(def ^:private indent-level "    ")

(defn- indent-line
  [s]
  (if (not-empty s)
    (str indent-level s)
    ""))

(defn indent
  "Indent lines of `s` by one `indent-level`."
  [s]
  (when s
    (->> (str/split-lines s)
         (map indent-line)
         (u/join-lines))))

(defn format-value
  "Format `x` value for inclusion in JavaScript source.

  Currently only strings are passed to `pr-str`, all other values are passed to `str`."
  [x]
  (cond
    (string? x) (pr-str x)
    :else (str x)))

(defn format-values
  "Format args and join with commas."
  [args]
  (str/join ", " (map format-value args)))

(defn fn-src
  "Generate JavaScript source for function with `name` and `body`, and taking `params`.

      => (fn-src {::name \"foo\", ::params ['x], ::body \"return x+1;\"})
      \"function foo(x) {\n    return x+1;\n}\"
  "
  [{:keys [::name ::body ::params]}]
  (let [params (or params ['msg])]
    (u/join-lines
      [(format "function %s(%s) {" name (format-values params))
       (indent body)
       "}"])))

(defn repeated-body
  "Repeat `body` `n` times."
  [body n]
  (u/join-lines
    [(format "for (i=0; i<%s; i++) {" n)
     (indent body)
     "}"]))

(defn sleep
  "Call sleep, as defined at https://devtrium.com/posts/how-sleep-javascript"
  [secs]
  (when (some? secs)
    (format "await sleep(%s);" (* 1000 secs))))

(defn fetch
  "Call fetch to make `method` HTTP request to `url`. If `ret` is \"obj\", the
  result is parsed as JSON."
  [method url ret]
  (u/join-lines
    [(format "await fetch(%s, {" (format-value url))
     (indent (format "method: %s," (format-value method)))
     (indent "headers: msg.headers,")
     (indent "body: msg.payload && JSON.stringify(msg.payload),")
     (str "})" (when (= "obj" ret) ".json()") ";")]))

(defn call-node
  "Call to function corresponding to node with ID `node-id`, taking `args`."
  [node args]
  (format "%s(%s);"
          (n/js-fn-name node)
          (format-values args)))

(defn node-calls
  "JavaScript source to call node functions wired to `node` as outputs."
  ([nodes]
   (node-calls nodes nil))
  ([nodes args]
   (let [lines (mapv #(call-node % (or args ['msg])) nodes)]
     (when (not-empty lines)
       (u/join-lines (into [""] lines)))))) ; Prefix with empty line

(defn result-passing-body
  "JavaScript source that captures return value of `body` and calls all
  `node`'s outputs with it."
  ([body node nodes]
   (u/join-lines
     ["const res = (function() {"
      (indent body)
      "})();"
      (node-calls (n/output-nodes node nodes) ['res])])))

(comment
  (def flows walterl.red2js/flows)

  (call-node (n/node-with-id "c4a1325.c0783d" flows)
             ['msg "foo" 123])
  ,)
