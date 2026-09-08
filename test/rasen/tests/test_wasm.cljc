(ns rasen.tests.test-wasm
  "rasen 螺旋 — WASM component entry tests (ADR-2606101000). Pure stdlib, NETWORK-FREE.
  1:1 Clojure port of tests/test_wasm.py.

  The Python test imports `wasm/app.py` (the componentize-py entry) and exercises its three
  exports. `wasm/app.py` is NOT in this port's scope (it is the WASM glue, not a method module),
  so the three export functions are reconstructed HERE as `app-*` helpers that call the real
  rasen.methods.* siblings exactly as app.py does — keeping the test self-contained without
  creating an unlisted .cljc. The JSON `analyze` export is serialized with cheshire
  (json.dumps ensure_ascii=False analogue) and re-parsed for the assertions, mirroring
  `json.loads(...)`. The dev fallback reads data/seed-genome-graph.kotoba.edn (no embedded seed
  under bb), exactly like app.py's _seed_text() fallback."
  (:require [clojure.test :refer [deftest is run-tests]]
            [kotoba.lang.text :as str]
            [clojure.set]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [rasen.methods.analyze :as analyze]
            [rasen.methods.datom-emit :as datom-emit]
            [rasen.methods.coverage-report :as coverage]))

(def actor-dir (io/file "."))
(def seed (io/file actor-dir "data" "seed-genome-graph.kotoba.edn"))

(defn- load* [] (analyze/load-file* seed))

(defn- round4 [v]
  (-> (java.math.BigDecimal/valueOf (double v))
      (.setScale 4 java.math.RoundingMode/HALF_EVEN)
      (.doubleValue)))

;; ── wasm/app.py export reconstruction (calls the real siblings, 1:1 with app.py) ──────────

(defn- rows
  ([nodes d] (rows nodes d 20))
  ([nodes d n]
   (->> (sort-by (fn [[nid v]] [(- (double v)) nid]) d)
        (take n)
        (mapv (fn [[nid v]]
                {"id" nid
                 "label" (get-in nodes [nid ":genome/label"] nid)
                 "score" (round4 v)})))))

(defn app-analyze []
  (let [{:keys [nodes edges]} (load*)
        res (analyze/analyze nodes edges)]
    (json/generate-string {"care" (rows nodes (get res "care"))
                           "burden" (rows nodes (get res "burden"))
                           "pleiotropy" (rows nodes (get res "pleiotropy"))})))

(defn app-datoms
  ([] (app-datoms 1))
  ([tx]
   (let [{:keys [nodes edges]} (load*)
         res (analyze/analyze nodes edges)]
     (datom-emit/emit nodes edges res (long tx)))))

(defn app-coverage []
  (let [{:keys [nodes edges]} (load*)]
    (coverage/report nodes edges)))

;; ── tests ─────────────────────────────────────────────────────────────────────

(deftest test-analyze-export-shape
  (let [out (json/parse-string (app-analyze))]
    (is (= #{"care" "burden" "pleiotropy"} (set (keys out))))
    (is (seq (get out "care")) "no care-priority rows")
    (let [top (first (get out "care"))]
      (is (clojure.set/subset? #{"id" "label" "score"} (set (keys top)))))
    ;; rows are score-descending
    (let [scores (mapv #(get % "score") (get out "care"))]
      (is (= scores (vec (reverse (sort scores))))))))

(deftest test-datoms-export-is-eavt-edn
  (let [edn (app-datoms 7)]
    (is (and (str/starts-with? (str/triml edn) ";;") (str/includes? edn "[")))
    (is (str/includes? edn " 7 :add]"))                 ;; tx threads through ground datoms
    (is (str/includes? edn ":bond/is-transient true")))) ;; derived readouts flagged transient

(deftest test-coverage-export-is-markdown
  (let [md (app-coverage)]
    (is (and (str/starts-with? md "# rasen")
             (str/includes? md "coverage of all genes/variants is ~0 by design")))))

(deftest test-exports-are-deterministic
  (is (= (app-analyze) (app-analyze)))
  (is (= (app-datoms 1) (app-datoms 1))))

;; The Python __main__ demo runner is intentionally omitted.
