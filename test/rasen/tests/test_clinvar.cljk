(ns rasen.tests.test-clinvar
  "Tests for the FULL-CORPUS ClinVar normaliser (ADR-2609062000).

  The G1 controls here assert the REASON, not merely that something was refused: a negative
  test that only checks 'it threw' counts an unrelated failure as a successful discrimination
  (superproject CLAUDE.md, the six questions, #6)."
  (:require [kotoba.lang.text :as str]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.clinvar :as cv]))

(def header
  (str/join "\t" ["#AlleleID" "Type" "Name" "GeneID" "GeneSymbol" "HGNC_ID"
                  "ClinicalSignificance" "ClinSigSimple" "LastEvaluated" "RS# (dbSNP)"
                  "nsv/esv (dbVar)" "RCVaccession" "PhenotypeIDS" "PhenotypeList" "Origin"
                  "OriginSimple" "Assembly" "ChromosomeAccession" "Chromosome" "Start" "Stop"
                  "ReferenceAllele" "AlternateAllele" "Cytogenetic" "ReviewStatus"
                  "NumberSubmitters" "Guidelines" "TestedInGTR" "OtherIDs" "SubmitterCategories"
                  "VariationID" "PositionVCF" "ReferenceAlleleVCF" "AlternateAlleleVCF"]))

(def idx (cv/header-index header))

(defn- row
  "Build a variant_summary row from column overrides."
  [overrides]
  (let [n (inc (apply max (vals idx)))
        base (vec (repeat n "-"))]
    (reduce (fn [r [col v]] (assoc r (get idx col) v)) base overrides)))

(def brca1-row
  (row {"Type" "single nucleotide variant" "GeneSymbol" "BRCA1" "HGNC_ID" "HGNC:1100"
        "ClinicalSignificance" "Pathogenic" "RS# (dbSNP)" "80357914"
        "PhenotypeIDS" "MONDO:MONDO:0011450,MedGen:C0677776,OMIM:604370"
        "PhenotypeList" "Hereditary breast ovarian cancer syndrome"
        "Assembly" "GRCh38" "Chromosome" "17" "Start" "43093464" "Stop" "43093464"
        "Cytogenetic" "17q21.31" "ReviewStatus" "reviewed by expert panel"
        "VariationID" "17662" "PositionVCF" "43093464"}))

;; ── header ──────────────────────────────────────────────────────────────────
(deftest header-index-strips-leading-hash
  (is (= 0 (get idx "AlleleID")) "'#AlleleID' must be addressable as 'AlleleID'")
  (is (= 23 (get idx "Cytogenetic")))
  (is (= 16 (get idx "Assembly"))))

;; ── G1: both directions, and the reason ─────────────────────────────────────
(deftest g1-clean-node-passes
  (let [n {":genome/id" "var.rs334" ":genome/kind" ":variant" ":variant/rsid" "rs334"}]
    (is (empty? (cv/g1-violations n)))
    (is (= n (cv/assert-g1! n)))))

(deftest g1-coordinate-node-is-refused-for-the-stated-reason
  (let [n {":genome/id" "var.rs334" ":variant/chromosome" "11" ":variant/start" "5227002"}
        e (try (cv/assert-g1! n) nil (catch #?(:clj Exception :cljs :default) e e))]
    (is (some? e) "a node carrying precise coordinates must be refused")
    (is (= "G1: precise coordinate refused" (ex-message e))
        "refused for the G1 reason, not for some unrelated failure")
    (is (= [":variant/chromosome" ":variant/start"] (:attrs (ex-data e)))
        "the refusal must name the offending attrs")))

(deftest g1-refuses-to-report-clean-for-a-non-node
  (doseq [bad [nil "var.rs334" 42]]
    (is (thrown? #?(:clj Exception :cljs :default) (cv/g1-violations bad))
        "an unreadable node is UNVERIFIED, never clean")))

(deftest emitted-graph-carries-no-coordinate-even-though-the-row-does
  (let [g (cv/row->graph brca1-row idx)]
    (is (some? g))
    (doseq [n (vals (:nodes g))] (is (empty? (cv/g1-violations n))))
    (is (= "17q21.31" (get-in g [:nodes "gene.brca1" ":gene/cytoband"]))
        "the COARSE cytoband is kept — the refusal is of precision, not of location")
    (is (not-any? #(str/includes? (str %) "43093464") (mapcat vals (vals (:nodes g))))
        "the row's precise position must appear nowhere in the emitted graph")))

;; ── disclosed scales (N3) ───────────────────────────────────────────────────
(deftest clinsig-mapping
  (is (= ":pathogenic" (cv/clinsig->key "Pathogenic")))
  (is (= ":pathogenic" (cv/clinsig->key "Pathogenic/Likely pathogenic")))
  (is (= ":likely-benign" (cv/clinsig->key "Likely benign")))
  (is (= ":uncertain" (cv/clinsig->key "Conflicting classifications of pathogenicity"))
      "no conflicting category exists in the ontology; the conflict survives in confidence")
  (is (= ":pathogenic" (cv/clinsig->key "Pathogenic; risk factor"))
      "the primary token wins; the modifier is dropped (:en/clinsig is single-valued)")
  (is (nil? (cv/clinsig->key "not provided")))
  (is (nil? (cv/clinsig->key "")))
  (is (nil? (cv/clinsig->key nil))))

(deftest review-status-unknown-is-lowest-not-highest
  (is (= 0.9 (cv/review->confidence "reviewed by expert panel")))
  (is (= 1.0 (cv/review->confidence "practice guideline")))
  (is (= 0.2 (cv/review->confidence "")) "blank status is unmeasured, not good")
  (is (= 0.2 (cv/review->confidence "some status ClinVar invented last week"))))

(deftest grasping-load-is-clinsig-times-confidence
  (let [g (cv/row->graph brca1-row idx)
        e (first (filter #(= ":associated-with" (get % ":en/kind")) (:edges g)))]
    (is (= ":pathogenic" (get e ":en/clinsig")))
    (is (= 0.9 (get e ":en/grasping-load")) "1.0 pathogenic × 0.9 expert-panel")))

;; ── phenotype references ────────────────────────────────────────────────────
(deftest mondo-value-keeps-its-own-colon
  (is (= {"mondo" "MONDO:0013342" "medgen" "C3150901" "omim" "613647"}
         (cv/parse-phenotype-ids "MONDO:MONDO:0013342,MedGen:C3150901,OMIM:613647"))
      "split on the FIRST colon only — MONDO values contain one"))

(deftest phenotype-preference-and-skipping
  (is (= "ph.mondo-0011450" (first (cv/phenotype-node "MONDO:MONDO:0011450,OMIM:604370" "HBOC"))))
  (is (= "ph.omim-604370" (first (cv/phenotype-node "OMIM:604370" "HBOC"))))
  (is (nil? (cv/phenotype-node "" "not provided")) "'not provided' names no condition")
  (is (nil? (cv/phenotype-node "-" "not specified"))))

(deftest phenotype-groups-align-with-names
  (let [refs (cv/phenotype-refs "MONDO:MONDO:0013342|MedGen:C3661900"
                                "Hereditary spastic paraplegia 48|Macular dystrophy")]
    (is (= 2 (count refs)))
    (is (= ["ph.mondo-0013342" "ph.medgen-c3661900"] (mapv first refs)))))

;; ── row selection ───────────────────────────────────────────────────────────
(deftest other-assembly-rows-are-skipped-not-emptied
  (let [g37 (cv/row->graph (row {"Assembly" "GRCh37" "RS# (dbSNP)" "334"
                                 "GeneSymbol" "HBB" "ClinicalSignificance" "Pathogenic"}) idx)]
    (is (nil? g37) "a skipped row returns nil, distinguishable from an empty graph")))

(deftest variant-without-rsid-falls-back-to-clinvar-id
  (let [g (cv/row->graph (row {"Assembly" "GRCh38" "RS# (dbSNP)" "-1" "VariationID" "2"
                               "GeneSymbol" "AP5Z1" "Type" "Indel"
                               "ClinicalSignificance" "Pathogenic/Likely pathogenic"
                               "ReviewStatus" "criteria provided, multiple submitters, no conflicts"
                               "PhenotypeIDS" "MONDO:MONDO:0013342" "PhenotypeList" "HSP 48"}) idx)]
    (is (contains? (:nodes g) "var.clinvar-2"))
    (is (= ":indel" (get-in g [:nodes "var.clinvar-2" ":variant/category"])))))

(deftest multi-gene-row-emits-one-located-in-edge-per-gene
  (let [g (cv/row->graph (row {"Assembly" "GRCh38" "RS# (dbSNP)" "111" "GeneSymbol" "A;B"
                               "Cytogenetic" "1p1.1"}) idx)
        locs (filter #(= ":located-in" (get % ":en/kind")) (:edges g))]
    (is (= 2 (count locs)))
    (is (= #{"gene.a" "gene.b"} (set (map #(get % ":en/to") locs))))))

;; ── accumulation reports what it actually saw ───────────────────────────────
(deftest rows->graph-reports-rows-and-skips
  (let [acc (cv/rows->graph [brca1-row
                             (row {"Assembly" "GRCh37" "RS# (dbSNP)" "1"})
                             (row {"Assembly" "GRCh38" "RS# (dbSNP)" "-1" "VariationID" "-"})]
                            idx)]
    (is (= 1 (:rows acc)))
    (is (= 2 (:skipped acc)) "skips are counted, so coverage cannot be silently overstated")
    (is (pos? (count (:nodes acc))))))

(deftest empty-input-is-not-a-clean-corpus
  (let [acc (cv/rows->graph [] idx)]
    (is (= 0 (:rows acc)))
    (is (empty? (:nodes acc)))
    (is (= 0 (:skipped acc))
        "zero rows must be visible as zero — a caller may not read it as a full corpus")))

(deftest gene-nodes-merge-across-rows
  (testing "ClinVar repeats a gene on every one of its variants"
    (let [acc (cv/rows->graph [brca1-row
                               (row {"Assembly" "GRCh38" "RS# (dbSNP)" "80357906"
                                     "GeneSymbol" "BRCA1" "Cytogenetic" "17q21.31"
                                     "ClinicalSignificance" "Likely pathogenic"})]
                              idx)]
      (is (= 2 (:rows acc)))
      (is (= 1 (count (filter #(= ":gene" (get % ":genome/kind")) (vals (:nodes acc))))))
      (is (= "17q21.31" (get-in acc [:nodes "gene.brca1" ":gene/cytoband"]))))))
