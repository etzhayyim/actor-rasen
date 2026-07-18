(ns rasen.tests.test-coverage
  "rasen 螺旋 — coverage-report tests (ADR-2606101000). 1:1 Clojure port of tests/test_coverage.py."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [rasen.methods.analyze :as analyze]
            [rasen.methods.coverage-report :as coverage]))

(def actor-dir (io/file "."))
(def seed (io/file actor-dir "data" "seed-genome-graph.kotoba.edn"))

(defn load-seed [] (analyze/load-file* seed))

(deftest test-coverage-renders-and-is-honest
  (let [{:keys [nodes edges]} (load-seed)
        md (coverage/report nodes edges)]
    ;; honest denominator disclosure present
    (is (str/includes? md "coverage of all genes/variants is ~0 by design"))
    ;; the G1 public-only guard is surfaced
    (is (str/includes? md "no individual genotypes"))
    ;; the gap map names next-wave targets
    (is (str/includes? md "Gap map"))))

(deftest test-taxa-span-life-not-just-humans
  (let [{:keys [nodes]} (load-seed)
        taxa (set (for [n (vals nodes)
                        :when (= ":gene" (get n ":genome/kind"))]
                    (get n ":gene/taxon")))]
    (is (contains? taxa ":homo-sapiens") "human genetics missing")
    ;; at least one non-human taxon (life is broader than humans)
    (is (>= (count (disj taxa ":homo-sapiens")) 1)
        (str "only human taxa present: " taxa))))

(deftest test-populations-are-aggregate
  (let [{:keys [nodes]} (load-seed)
        pops (set (for [n (vals nodes)
                        :when (= ":population" (get n ":genome/kind"))]
                    (get n ":population/code")))]
    (is (and (contains? pops ":global") (>= (count pops) 3))
        (str "thin population aggregates: " pops))))
