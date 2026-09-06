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
        ;; The counts are cumulative, so "skips what was already read" is not a smaller
        ;; number — it is the ABSENCE of a larger one. 60 GRCh38 rows exist; if the resumed
        ;; run had re-normalised the prefix the total would be 70.
        (is (= 60 (:rows/kept second-pass)) "each row was normalised exactly once")
        (is (:ok (ls/verify! root)) "and the two passes form one chain"))
      (finally (rm-r d)))))

(deftest no-source-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (run/run! (tmp-dir) {}))))

(deftest expect-bytes-against-a-url-is-refused-not-ignored
  (testing "a stream's size is not knowable before the read; ignoring the caller's declared
            size would look exactly like having checked it"
    (let [e (try (run/run! (tmp-dir) {:source "https://example.invalid/x.gz" :expect-bytes 42})
                 nil (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e))
      (is (= :expect-bytes-unmeasurable (:reason (ex-data e)))))))

(deftest progress-is-checkpointed-during-the-run-not-only-at-its-end
  (testing "a checkpoint written only on completion is one that exists only for runs that
            did not need it"
    (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")
          seen (atom [])]
      (try
        (write-gz! gz (corpus-text 60))
        ;; :after-append fires once per appended transaction; read the checkpoint from disk
        ;; there, so what is asserted is what a crashed run would have left behind.
        (run/run! root {:source (str gz) :fingerprint "rel-1" :batch {:max-datoms 200}
                        :after-append (fn [rt]
                                        (swap! seen conj (get (run/read-checkpoint rt)
                                                              ":rows/read")))})
        (is (< 1 (count @seen)) "more than one transaction was appended")
        (is (every? some? @seen) "a checkpoint existed at every append, not just at the end")
        (is (apply <= @seen) "and it only ever moved forward")
        (is (pos? (first @seen)) "the first checkpoint already records real progress")
        (finally (rm-r d))))))

(deftest a-cut-stream-keeps-the-progress-it-had
  (testing "the rows already appended are in the ledger; a failure that reset the checkpoint
            would make the resumed run go find them again — which is the whole cost this
            checkpoint exists to avoid"
    (let [d (tmp-dir) gz (io/file d "cv.txt.gz") cut (io/file d "cut.txt.gz")
          root (io/file d "ledger")]
      (try
        (let [n (write-gz! gz (corpus-text 400))
              bytes (java.util.Arrays/copyOf (java.nio.file.Files/readAllBytes (.toPath gz))
                                             (int (* 0.7 n)))]
          (io/copy bytes cut)
          (try (run/run! root {:source (str cut) :fingerprint "rel-1"
                               :batch {:max-datoms 200}})
               (catch clojure.lang.ExceptionInfo _ nil))
          (let [cp (run/read-checkpoint root)]
            (is (some? cp) "a cut run must leave a checkpoint")
            (is (pos? (get cp ":rows/read")) "carrying the progress it actually made")
            (is (pos? (get cp ":rows/kept")))
            ;; read back as a ':…' STRING, not a keyword — the minimal reader in
            ;; kotoba.cljc keeps those as strings, the same convention the graph nodes use
            (is (contains? #{":truncated" ":corrupt" ":interrupted"} (get cp ":run/status"))
                "and saying which way the stream died")))
        (finally (rm-r d))))))

(deftest stream-failures-are-classified-and-nothing-else-is
  (is (= :truncated (run/stream-failure (java.io.EOFException. "cut"))))
  (is (= :corrupt (run/stream-failure (java.util.zip.ZipException. "bad"))))
  (is (= :interrupted (run/stream-failure (java.net.SocketException. "Connection reset")))
      "the failure a multi-hour stream actually dies of")
  (is (= :interrupted (run/stream-failure (java.net.SocketTimeoutException. "timeout"))))
  (is (nil? (run/stream-failure (IllegalArgumentException. "not a stream problem")))
      "a failure this does not recognise must not be relabelled as a stream problem"))

(deftest checkpoint-counts-share-one-basis
  (testing "rows/read is cumulative because the reader walks the file from the top; if
            kept and skipped stayed per-run, the same map would carry two bases and its
            kept count would read as a total it is not"
    (let [d (tmp-dir) gz (io/file d "cv.txt.gz") root (io/file d "ledger")]
      (try
        (write-gz! gz (corpus-text 60))
        (let [first-pass (run/run! root {:source (str gz) :fingerprint "rel-1" :limit 10
                                         :batch {:max-datoms 200}})
              cp1 (run/read-checkpoint root)
              second-pass (run/run! root {:source (str gz) :fingerprint "rel-1" :resume? true
                                          :batch {:max-datoms 200}})
              cp2 (run/read-checkpoint root)]
          (is (= :limited (:status first-pass)))
          (is (= :complete (:status second-pass)))
          (is (= 120 (get cp2 ":rows/read")) "every line of the release was read")
          (is (= 60 (get cp2 ":rows/kept")) "and every GRCh38 row was kept, across both runs")
          (is (>= (get cp2 ":rows/kept") (get cp1 ":rows/kept"))
              "a resumed run never reports fewer kept rows than the run before it")
          (is (= (get cp2 ":rows/read")
                 (+ (get cp2 ":rows/kept") (get cp2 ":rows/skipped")))
              "read = kept + skipped: one basis, and nothing unaccounted for"))
        (finally (rm-r d))))))
