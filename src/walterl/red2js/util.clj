(ns walterl.red2js.util
  (:require [clojure.string :as str]))

(defn join-lines
  "Join all non-nil items in `ss` with a newline."
  [ss]
  (str/join \newline (remove nil? ss)))

(defn ->double
  [x]
  (cond
    (double? x) x
    (string? x) (Double/parseDouble x)
    :else       (throw (ex-info (str "Invalid double value: " (pr-str x))
                                {:type ::invalid-double-value, :value x}))))
