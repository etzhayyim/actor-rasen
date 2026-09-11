(ns rasen.app
  "cljc-native WASM entry for the rasen 螺旋 actor (ADR-2606261200). Binds the WIT world
  exports (analyze/datoms/coverage) to the shared methods/*.cljc over the embedded bounded
  PUBLIC genome seed. Sandbox has no FS/network (G1: no individual genotypes; the component
  cannot leak what it does not contain)."
  (:require [rasen.methods.analyze :as az]
            [rasen.methods.datom-emit :as de]
            [rasen.methods.coverage-report :as cov]
            [rasen.seed :as seed]))

(defn- load* []
  ;; app.py _load: read-edn → split nodes(:genome/id) / edges(:en/from,:en/to). load-graph
  ;; accepts a parsed-forms vector and returns [nodes edges].
  (az/load-graph (az/read-edn seed/edn)))

(defn- round4 [v] (/ (js/Math.round (* (js/Number v) 10000)) 10000))

(defn- rows [d nodes]
  ;; sorted by (-score, id) like Python's sorted(key=lambda kv: (-kv[1], kv[0]))[:20]
  (into-array
   (map (fn [[nid v]] #js {:id    nid
                           :label (get-in nodes [nid ":genome/label"] nid)
                           :score (round4 v)})
        (take 20 (sort-by (fn [[k v]] [(- v) k]) d)))))

(defn ^:export analyze []
  (let [{:keys [nodes edges]} (load*)
        res (az/analyze nodes edges)]
    (js/JSON.stringify #js {:care       (rows (get res "care") nodes)
                            :burden     (rows (get res "burden") nodes)
                            :pleiotropy (rows (get res "pleiotropy") nodes)})))

(defn ^:export datoms [tx]
  (let [{:keys [nodes edges]} (load*)] (de/emit nodes edges (az/analyze nodes edges) tx)))

(defn ^:export coverage []
  (let [{:keys [nodes edges]} (load*)] (cov/report nodes edges)))
