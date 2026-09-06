(ns rasen.tests.test-clinvar-run
  "Tests for the G7 ClinVar streaming runner (ADR-2609062000).

  A download that stops early decompresses cleanly up to the cut and then ends. The loop
  cannot tell that from a short release, so most of what is asserted here is that a truncated
  or short-read source never reports :complete."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.clinvar :as cv]
            [rasen.methods.clinvar-run :as run]
            [rasen.methods.ledger-shards :as ls]))

(def header
  (str/join "\t" ["#AlleleID" "Type" "Name" "GeneID" "GeneSymbol" "HGNC_ID"
                  "ClinicalSignificance" "ClinSigSimple" "LastEvaluated" "RS# (dbSNP)"
                  "nsv/esv (dbVar)" "RCVaccession" "PhenotypeIDS" "PhenotypeList" "Origin"
                  "OriginSimple" "Assembly" "ChromosomeAccession" "Chromosome" "Start" "Stop"
                  "ReferenceAllele" "AlternateAllele" "Cytogenetic" "ReviewStatus"
                  "NumberSubmitters" "Guidelines" "TestedInGTR" "OtherIDs" "SubmitterCategories"
                  "VariationID" "PositionVCF" "ReferenceAlleleVCF" "AlternateAlleleVCF"]))

(def idx (cv/header-index header))

(defn- row-line [i assembly]
  (let [n (inc (apply max (vals idx)))
        base (vec (repeat n "-"))
        put (fn [r col v] (assoc r (get idx col) v))]
    (str/join "\t"
              (-> base
                  (put "Type" "single nucleotide variant")
                  (put "GeneSymbol" (str "GENE" (mod i 5)))
                  (put "ClinicalSignificance" "Pathogenic")
                  (put "RS# (dbSNP)" (str (+ 1000 i)))
                  (put "PhenotypeIDS" (str "MONDO:MONDO:00" (format "%05d" (mod i 7))))
                  (put "PhenotypeList" (str "Condition " (mod i 7)))
                  (put "Assembly" assembly)
                  (put "Chromosome" "17") (put "Start" "43093464") (put "Stop" "43093464")
                  (put "Cytogenetic" "17q21.31")
                  (put "ReviewStatus" "criteria provided, single submitter")
                  (put "VariationID" (str i))))))

(defn- corpus-text [n]
  (str/join "\n" (concat [header]
                         (mapcat (fn [i] [(row-line i "GRCh37") (row-line i "GRCh38")]) (range n)))))

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "rasen-run-" (System/nanoTime)))
    (.mkdirs)))

(defn- rm-r [f] (when (.isDirectory f) (run! rm-r (.listFiles f))) (.delete f))

(defn- write-gz!
  "Write `text` gzipped to `f`; returns the compressed byte count."
  [f text]
  (with-open [out (java.util.zip.GZIPOutputStream. (io/output-stream f))]
    (.write out (.getBytes ^String text "UTF-8")))
  (.length f))

;; ── pure helpers ────────────────────────────────────────────────────────────
(deftest suppress-seen-drops-only-repeating-kinds
  (let [g {:nodes {"gene.a" {":genome/kind" ":gene"}
                   "var.rs1" {":genome/kind" ":variant"}}
           :edges []}
        [g1 seen1] (run/suppress-seen g #{})
        [g2 seen2] (run/suppress-seen g seen1)]
    (is (= 2 (count (:nodes g1))) "first batch emits both")
    (is (contains? seen1 "gene.a"))
    (is (not (contains? seen1 "var.rs1")) "variants are not carried — they do not repeat")
    (is (= #{"var.rs1"} (set (keys (:nodes g2)))) "the gene is suppressed the second time")
    (is (= seen1 seen2))))

(deftest a-checkpoint-without-a-fingerprint-is-not-usable
  (is (false? (run/checkpoint-usable? {":rows/read" 100} "abc"))
      "a checkpoint that cannot say which release it counted must not be resumed")
  (is (false? (run/checkpoint-usable? nil "abc")))
  (is (false? (run/checkpoint-usable? {":source/fingerprint" "other"} "abc")))
  (is (true? (run/checkpoint-usable? {":source/fingerprint" "abc"} "abc"))))

;; ── a whole stream ──────────────────────────────────────────────────────────
(deftest a-complete-stream-reports-complete-and-the-ledger-verifies
  (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
    (try
      (let [n-bytes (write-gz! gz (corpus-text 40))
            r (run/run! root {:source (str gz) :expect-bytes n-bytes
                              :batch {:max-datoms 200}})]
        (is (= :complete (:status r)))
        (is (= 80 (:rows/read r)) "both assemblies were read")
        (is (= 40 (:rows/kept r)) "one assembly was kept")
        (is (= 40 (:rows/skipped r)))
        (is (= n-bytes (:source/bytes r)) "the compressed bytes consumed equal the source size")
        (is (pos? (:ledger/txs r)))
        (is (:ok (ls/verify! root)) "the ledger it wrote is one unbroken chain")
        (is (= (:head r) (ls/head-cid root))))
      (finally (rm-r d)))))

(deftest gene-datoms-are-emitted-once-not-once-per-row
  (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
    (try
      (write-gz! gz (corpus-text 40))
      (let [r (run/run! root {:source (str gz) :batch {:max-datoms 100000}})
            txs (ls/read-shard root 0)
            gene-adds (filter (fn [[_ e a _]] (and (str/starts-with? e "gene.")
                                                   (= a ":gene/symbol")))
                              (mapcat #(get % ":tx/datoms") txs))]
        (is (= :complete (:status r)))
        (is (= 5 (count gene-adds)) "five distinct genes, five :gene/symbol datoms"))
      (finally (rm-r d)))))

;; ── the refusals ────────────────────────────────────────────────────────────
(deftest a-cut-gzip-is-refused-not-reported-as-a-corpus
  (testing "the bytes decompress cleanly up to the cut; only the gzip trailer knows"
    (let [d (tmp-dir) gz (io/file d "cv.txt.gz") cut (io/file d "cut.txt.gz")
          root (io/file d "ledger")]
      (try
        (let [n (write-gz! gz (corpus-text 200))
              bytes (java.util.Arrays/copyOf (java.nio.file.Files/readAllBytes (.toPath gz))
                                             (int (* 0.6 n)))]
          (io/copy bytes cut)
          (let [e (try (run/run! root {:source (str cut) :batch {:max-datoms 200}}) nil
                       (catch clojure.lang.ExceptionInfo ex ex))]
            (is (some? e) "a stream that ends mid-member must not return a result")
            (is (contains? #{:truncated :corrupt} (:reason (ex-data e)))
                "refused as a broken stream, not for some unrelated reason")))
        (finally (rm-r d))))))

(deftest a-short-read-against-a-declared-size-is-truncated-not-complete
  (testing "the gzip is whole, but the caller was told the release is bigger than what arrived"
    (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
      (try
        (let [n (write-gz! gz (corpus-text 20))
              r (run/run! root {:source (str gz) :expect-bytes (+ n 4096)
                                :batch {:max-datoms 200}})]
          (is (= :truncated (:status r)) "declared size not met — this is not a complete corpus")
          (is (not= :complete (:status r))))
        (finally (rm-r d))))))

(deftest a-bounded-run-reports-limited-never-complete
  (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
    (try
      (write-gz! gz (corpus-text 100))
      (let [r (run/run! root {:source (str gz) :limit 10 :batch {:max-datoms 200}})]
        (is (= :limited (:status r)) "a run that stopped early must never claim the whole corpus")
        (is (<= (:rows/kept r) 10)))
      (finally (rm-r d)))))

(deftest resuming-a-different-release-is-refused
  (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
    (try
      (write-gz! gz (corpus-text 20))
      (run/run! root {:source (str gz) :fingerprint "release-A" :batch {:max-datoms 200}})
      (let [e (try (run/run! root {:source (str gz) :fingerprint "release-B" :resume? true
                                   :batch {:max-datoms 200}}) nil
                   (catch clojure.lang.ExceptionInfo ex ex))]
        (is (some? e))
        (is (str/includes? (ex-message e) "different release")
            "row offsets are not stable across releases"))
      (finally (rm-r d)))))

(deftest resuming-the-same-release-skips-what-was-already-read
  (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
    (try
      (write-gz! gz (corpus-text 60))
      (let [first-pass (run/run! root {:source (str gz) :fingerprint "rel-1" :limit 10
                                       :batch {:max-datoms 200}})
            second-pass (run/run! root {:source (str gz) :fingerprint "rel-1" :resume? true
                                        :batch {:max-datoms 200}})]
        (is (= :limited (:status first-pass)))
        (is (= :complete (:status second-pass)))
        (is (= 120 (:rows/read second-pass)) "the reader still walks the file")
        (is (< (:rows/kept second-pass) 60) "but only the unread tail was normalised")
        (is (:ok (ls/verify! root)) "and the two passes form one chain"))
      (finally (rm-r d)))))

(deftest no-source-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (run/run! (tmp-dir) {}))))
