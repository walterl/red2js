(ns walterl.red2js.util
  (:require [clojure.string :as str]))

(defn join-lines
  "Join all non-nil items in `ss` with a newline."
  [ss]
  (str/join \newline (remove nil? ss)))
