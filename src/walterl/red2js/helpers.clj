(ns walterl.red2js.helpers
  (:require [clojure.set :as set]))

(defn val-freqs
  "Maps keys of maps in `ms` to frequencies of values.

  Intended to illustrate the distribution of record values.

  Default ignored keys: `#{:x :y :z :g :id :type :name :wires}`"
  ([ms]
   (val-freqs ms #{:x :y :z :g :id :type :name :wires}))
  ([ms ignored-ks]
   (let [ks (-> (set (keys (first ms)))
                (set/difference ignored-ks))]
     (into {}
           (map (fn [k] [k (frequencies (map k ms))])
                ks)))))
