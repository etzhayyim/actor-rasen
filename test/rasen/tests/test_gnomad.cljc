(ns rasen.tests.test-gnomad
  "Tests for gnomAD population-frequency extraction (ADR-2609062000).

  Two of these guard constraints that are constitutional rather than technical: a record with
  no rsID is not representable at all under G1, and sex-stratified frequencies are not
  super-population frequencies even though their INFO keys look alike."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.clinvar :as cv]
            [rasen.methods.gnomad :as gn]))

(defn- rec [id info]
  ["chr21" "5030000" id "C" "T" "." "PASS" info "" ""])

(def full-info
  (str "AC=12;AN=1000;AF=0.012;AF_XX=0.9;AF_XY=0.8;AF_afr=0.03;AF_afr_XX=0.7;"
       "AF_amr=0.004;AF_eas=0.0;AF_nfe=0.011;AF_sas=0.002;AF_fin=0.5;AF_asj=0.6"))

;; ── INFO parsing ────────────────────────────────────────────────────────────
(deftest info-parses-to-a-map-and-keeps-flags
  (let [m (gn/parse-info "AC=12;AN=1000;PASS_FLAG;AF=0.5")]
    (is (= "12" (get m "AC")))
    (is (= "0.5" (get m "AF")))
    (is (true? (get m "PASS_FLAG")))))

(deftest af-outside-zero-to-one-is-refused
  (is (= 0.012 (gn/parse-af "0.012")))
  (is (= 0.5 (gn/parse-af "0.5,0.25")) "sites files are split; the first value is this record's")
  (is (nil? (gn/parse-af "1.5")) "not a frequency")
  (is (nil? (gn/parse-af "-0.1")))
  (is (nil? (gn/parse-af "NaN")))
  (is (nil? (gn/parse-af ".")))
  (is (nil? (gn/parse-af nil))))

(deftest rsid-extraction
  (is (= "rs334" (gn/rsid-of "rs334")))
  (is (= "rs334" (gn/rsid-of "someid;rs334")))
  (is (nil? (gn/rsid-of ".")) "gnomAD writes '.' for absent")
  (is (nil? (gn/rsid-of "")) )
  (is (nil? (gn/rsid-of "COSM12345")) "a non-dbSNP id is not an rsID"))

;; ── the two constitutional constraints ──────────────────────────────────────
(deftest a-record-without-an-rsid-is-not-representable
  (testing "identifying it would need CHROM/POS/REF/ALT, which G1 forbids"
    (is (nil? (gn/record->graph (rec "." full-info))))
    (is (nil? (gn/record->graph (rec "" full-info))))))

(deftest sex-stratified-frequencies-are-not-emitted
  (testing "AF_XX and AF_XY look like the others; a prefix match would sweep them in"
    (let [g (gn/record->graph (rec "rs334" full-info))
          pops (set (map #(get % ":en/from") (:edges g)))]
      (is (= #{"pop.global" "pop.afr" "pop.amr" "pop.eas" "pop.eur" "pop.sas"} pops))
      (is (not-any? #(str/includes? % "xx") pops))
      (is (not-any? #(str/includes? % "xy") pops))
      (is (= 6 (count (:edges g))) "six super-populations, not one per AF_ key"))))

(deftest sub-super-population-keys-have-no-target
  (testing "fin / asj / ami / mid / remaining are finer than the ontology admits"
    (doseq [k gn/excluded-af-keys]
      (is (not (contains? gn/af-population k))
          (str k " must not be in the allow-list")))))

(deftest no-coordinate-reaches-the-graph
  (let [g (gn/record->graph (rec "rs334" full-info))]
    (doseq [n (vals (:nodes g))] (is (empty? (cv/g1-violations n))))
    (is (not-any? #(str/includes? (str %) "5030000") (mapcat vals (vals (:nodes g))))
        "the record's position appears nowhere")
    (is (not-any? #(str/includes? (str %) "chr21") (mapcat vals (vals (:nodes g)))))))

;; ── frequencies are copied, not derived ─────────────────────────────────────
(deftest frequency-is-the-grasping-load
  (let [g (gn/record->graph (rec "rs334" full-info))
        by-pop (into {} (map (juxt #(get % ":en/from") #(get % ":en/grasping-load")) (:edges g)))]
    (is (= 0.012 (get by-pop "pop.global")))
    (is (= 0.03 (get by-pop "pop.afr")))
    (is (= 0.011 (get by-pop "pop.eur")) "AF_nfe is the ontology's EUR")
    (is (= 0.0 (get by-pop "pop.eas")) "a measured zero is a value, not an absence")))

(deftest a-record-with-no-usable-af-still-yields-its-variant
  (let [g (gn/record->graph (rec "rs999" "AC=0;AN=0;AF=.") )]
    (is (some? g))
    (is (contains? (:nodes g) "var.rs999"))
    (is (empty? (:edges g)) "no frequency, no frequency edge")))

;; ── coverage is reported, never implied ─────────────────────────────────────
(deftest extract-partitions-every-record-it-saw
  (let [acc (gn/extract [(rec "rs1" full-info) (rec "." full-info)
                         (rec "rs2" full-info) (rec "." full-info) (rec "." full-info)])]
    (is (= 5 (:records acc)))
    (is (= 2 (:with-rsid acc)))
    (is (= 3 (:without-rsid acc)))
    (is (= 5 (+ (:with-rsid acc) (:without-rsid acc))) "every record is accounted for")
    (is (= 12 (:edges-emitted acc)))
    (is (= 0.4 (gn/coverage acc)))))

(deftest coverage-of-nothing-is-nil-not-a-number
  (is (nil? (gn/coverage gn/empty-acc))
      "an extraction that saw no records has no coverage; 1.0 or 0.0 would read as measured")
  (is (nil? (gn/coverage (gn/extract [])))))
