(ns rasen.tests.test-analyze
  "rasen 螺旋 — analyzer tests (ADR-2606101000). 1:1 Clojure port of tests/test_analyze.py.

  Verifies the constitutional invariants empirically:
    - graph loads (nodes + 縁), seed is non-trivial, no dangling 縁
    - edge-primary (N1): gene care-priority is the integral of incident variant-association
      (attributed via :located-in) + gene-linkage edges × disclosed clinsig — recomputed
      independently here and asserted equal
    - the top care-priority gene is a pathogenic-linked gene (sanity of the lens)
    - locus burden is non-empty and every burden-bearer is a :variant
    - G1: no individual-genotype fields; allele frequency is population-aggregate only

  NOTE on scope: the Python test_analyze additionally exercises the `datom_emit` sibling
  (test_datom_emit_ground_and_transient + test_determinism). Those two assertions depend on
  the unported `datom_emit` module, so they are intentionally omitted here (the datom_emit
  port is a separate unit). All five PURE analyze assertions are ported 1:1."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [clojure.set]
            [clojure.java.io :as io]
            [rasen.methods.analyze :as analyze]))

(def actor-dir (io/file "."))
(def seed (io/file actor-dir "data" "seed-genome-graph.kotoba.edn"))

(defn load-seed [] (analyze/load-file* seed))

(deftest test-load-nontrivial
  (let [{:keys [nodes edges]} (load-seed)]
    (is (>= (count nodes) 30) (str "expected a real seed, got " (count nodes) " nodes"))
    (is (>= (count edges) 40) (str "expected a real 縁 web, got " (count edges) " edges"))
    (let [kinds (set (map #(get % ":genome/kind") (vals nodes)))]
      (is (clojure.set/subset? #{":gene" ":variant" ":phenotype"} kinds)
          (str "missing core kinds: " kinds)))
    (doseq [e edges]
      (is (contains? nodes (get e ":en/from")) (str "dangling from: " (get e ":en/from")))
      (is (contains? nodes (get e ":en/to")) (str "dangling to: " (get e ":en/to"))))))

(deftest test-edge-primary-integral
  (testing "N1: care-priority MUST equal the independent integral of incident clinical 縁."
    (let [{:keys [nodes edges]} (load-seed)
          res (analyze/analyze nodes edges)
          variant-gene (into {} (for [e edges
                                      :when (= ":located-in" (get e ":en/kind"))]
                                  [(get e ":en/from") (get e ":en/to")]))
          expect (reduce
                  (fn [m e]
                    (let [k (get e ":en/kind")
                          w (get analyze/clinsig-weight (get e ":en/clinsig") 0.3)
                          load- (double (get e ":en/grasping-load"))]
                      (cond
                        (= k ":associated-with")
                        (if-let [gene (get variant-gene (get e ":en/from"))]
                          (update m gene (fnil + 0.0) (* load- w))
                          m)
                        (= k ":linked-to")
                        (update m (get e ":en/from") (fnil + 0.0) (* load- w))
                        :else m)))
                  {} edges)]
      (doseq [[nid v] expect]
        (is (< (Math/abs (- (get-in res ["care" nid]) v)) 1e-9)
            (str nid ": " (get-in res ["care" nid]) " != " v)))
      ;; there is NO stored per-node score key on any node (edge-primary only)
      (doseq [n (vals nodes)]
        (is (not (some #(or (str/starts-with? % ":bond/") (= % ":genome/score-of-gene"))
                       (keys n))))))))

(deftest test-care-top-is-pathogenic-gene
  (let [{:keys [nodes edges]} (load-seed)
        res (analyze/analyze nodes edges)
        top (key (apply max-key val (get res "care")))]
    (is (= ":gene" (get-in nodes [top ":genome/kind"]))
        (str "top care node " top " is not a gene"))
    ;; the top gene must carry at least one disclosed pathogenic linkage/association
    (let [located (set (for [e edges
                             :when (and (= ":located-in" (get e ":en/kind"))
                                        (= top (get e ":en/to")))]
                         (get e ":en/from")))
          incident-clinsig (into (set (for [e edges
                                            :when (or (= top (get e ":en/from"))
                                                      (= top (get e ":en/to")))]
                                        (get e ":en/clinsig")))
                                 (for [e edges
                                       :when (contains? located (get e ":en/from"))]
                                   (get e ":en/clinsig")))]
      (is (contains? incident-clinsig ":pathogenic")
          (str "top gene " top " has no pathogenic evidence")))))

(deftest test-locus-burden-nonempty-and-variant
  (let [{:keys [nodes edges]} (load-seed)
        res (analyze/analyze nodes edges)]
    (is (seq (get res "burden")) "no locus burden computed")
    (doseq [nid (keys (get res "burden"))]
      (is (= ":variant" (get-in nodes [nid ":genome/kind"]))
          (str "burden-bearer " nid " is not a variant")))))

(deftest test-g1-no-individual-genotypes-and-aggregate-only
  (testing "G1: PUBLIC reference only — no individual-genotype fields; freq is population-aggregate."
    (let [{:keys [nodes edges]} (load-seed)
          forbidden #{":sample/id" ":individual/id" ":patient/id" ":genotype/call"
                      ":sequence/raw" ":family/id"}]
      (doseq [n (vals nodes)]
        (let [leaked (clojure.set/intersection (set (keys n)) forbidden)]
          (is (empty? leaked) (str "individual-level field leaked: " leaked))))
      ;; every :allele-frequency edge originates from a :population node (aggregate, never a person)
      (doseq [e edges
              :when (= ":allele-frequency" (get e ":en/kind"))]
        (let [src (get nodes (get e ":en/from"))]
          (is (= ":population" (get src ":genome/kind"))
              (str "allele-frequency not sourced from a population aggregate: " (get e ":en/from")))
          (let [af (double (get e ":en/grasping-load"))]
            (is (and (<= 0.0 af) (<= af 1.0)))))))))
