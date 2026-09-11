(ns rasen.tests.test-datom-emit
  "rasen 螺旋 — Datom-emit tests (ADR-2606101000). 1:1 port of the two datom_emit-dependent
  tests deferred when analyze was ported: test_datom_emit_ground_and_transient + test_determinism.

  Mirrors tests/test_analyze.py: loads the seed via the datom-emit ordered loader (which
  carries ::node-order so emit walks nodes in EDN read order), analyzes, emits, and asserts
  the ground/transient split + determinism (repeat-emit byte-identical)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [kotoba.lang.text :as str]
            [clojure.java.io :as io]
            [rasen.methods.analyze :as analyze]
            [rasen.methods.datom-emit :as datom-emit]))

(def actor-dir (io/file "."))
(def seed (str (io/file actor-dir "data" "seed-genome-graph.kotoba.edn")))

(deftest test-datom-emit-ground-and-transient
  (let [{:keys [nodes edges]} (datom-emit/load-file* seed)
        res (analyze/analyze nodes edges)
        out (datom-emit/emit nodes edges res 7)]
    (is (str/includes? out ":add]") "no ground :add datoms emitted")
    (is (str/includes? out ":gene/symbol") "node attribute datoms missing")
    (is (str/includes? out ":en/grasping-load") "edge attribute datoms missing")
    (is (str/includes? out ":en/clinsig") "clinsig edge datoms missing")
    ;; derived readouts must be flagged transient, NOT persisted as :add
    (is (str/includes? out ":bond/is-transient true"))
    (is (str/includes? out ":bond/care-priority"))
    ;; every :bond/* readout line must be op :derived (never persisted as :add)
    (doseq [line (str/split-lines out)]
      (when (and (str/starts-with? line "[") (str/includes? line ":bond/"))
        (is (str/includes? line ":derived]")
            (str "derived readout not flagged transient: " line))))
    ;; tx threads through
    (is (str/includes? out " 7 :add]"))))

(deftest test-determinism
  (let [{:keys [nodes edges]} (datom-emit/load-file* seed)
        res (analyze/analyze nodes edges)
        a (datom-emit/emit nodes edges res 1)
        {nodes2 :nodes edges2 :edges} (datom-emit/load-file* seed)
        res2 (analyze/analyze nodes2 edges2)
        b (datom-emit/emit nodes2 edges2 res2 1)]
    (is (= a b) "Datom emit is not deterministic")))
