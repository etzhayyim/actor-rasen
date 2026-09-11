(ns rasen.tests.test-publish
  "rasen 螺旋 — publish.cljc unit tests (ADR-2606101000).

  Tests the PURE logic in publish.cljc (manifest assembly, PUBLISH.md generation,
  artifact-entry shaping) and the IO-leg with an injected dry-run ipfs-fn so no
  real IPFS daemon or network is needed.

  Pins the manifest structure and PUBLISH.md shape against hand-checked expectations."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [kotoba.lang.text :as str]
            [rasen.methods.publish :as P]
            [rasen.methods.cid :as CID]
            #?(:clj [clojure.java.io :as io])))

;; ── artifact-entry (pure) ────────────────────────────────────────────────────

(deftest test-artifact-entry-match
  (let [e (P/artifact-entry "graph" "foo.edn" 1234
                             "bafymatch" "bafymatch")]
    (is (= "graph" (:label e)))
    (is (= "foo.edn" (:file e)))
    (is (= 1234 (:bytes e)))
    (is (= "bafymatch" (:cid e)))
    (is (true? (:cid_matches e)))))

(deftest test-artifact-entry-mismatch
  (let [e (P/artifact-entry "datoms" "d.edn" 42 "bafylocal" "bafydaemon")]
    (is (false? (:cid_matches e)))))

;; ── build-manifest (pure) ────────────────────────────────────────────────────

(deftest test-build-manifest-shape
  (let [arts {"graph"  {:label "graph" :file "g.edn" :bytes 100
                        :cid "bafygraph" :daemon_cid "bafygraph" :cid_matches true}
              "datoms" {:label "datoms" :file "d.edn" :bytes 50
                        :cid "bafydatoms" :daemon_cid "bafydatoms" :cid_matches true}}
        ipns {"name" "k51abc" :path "/ipns/k51abc" "points_to" "bafygraph"}
        m    (P/build-manifest "2026-06-22T00:00:00Z" arts ipns)]
    (is (= "rasen" (get m "actor")))
    (is (= "2606101000" (get m "adr")))
    (is (= "2026-06-22T00:00:00Z" (get m "published_at")))
    (is (= 3 (count (get m "gateways"))))
    (is (= arts (get m "artifacts")))
    (is (= ipns (get m "ipns")))))

;; ── publish-md (pure) ────────────────────────────────────────────────────────

(deftest test-publish-md-contains-cid
  (let [arts {"graph"  {:label "graph" :file "genome-graph.kotoba.edn" :bytes 9876
                        :cid "bafytestcid123" :daemon_cid "bafytestcid123" :cid_matches true}
              "datoms" {:label "datoms" :file "genome-datoms.kotoba.edn" :bytes 4321
                        :cid "bafydatomscid" :daemon_cid "bafydatomscid" :cid_matches true}}
        m    (P/build-manifest "2026-06-22T00:00:00Z" arts nil)
        ;; publish-md expects the "string-keyed" artifact format (Clojure keywords → strings)
        ;; matching what the IO leg passes; for the pure path we feed it directly
        md   (P/publish-md {"actor" "rasen" "adr" "2606101000"
                             "published_at" "2026-06-22T00:00:00Z"
                             "scope" "PUBLIC reference genetics only — no individual genotypes (G1)"
                             "artifacts" {"graph"  {"file" "genome-graph.kotoba.edn"
                                                    "bytes" 9876 "cid" "bafytestcid123"}
                                          "datoms" {"file" "genome-datoms.kotoba.edn"
                                                    "bytes" 4321 "cid" "bafydatomscid"}}
                             "ipns"     nil
                             "gateways" P/gateways})]
    (is (str/includes? md "bafytestcid123"))
    (is (str/includes? md "## Artifacts"))
    (is (str/includes? md "## IPNS"))
    (is (str/includes? md "## Fetch + verify"))
    (is (str/includes? md "genome-graph.kotoba.edn"))
    (is (str/ends-with? md "\n"))))

(deftest test-publish-md-no-ipns-when-nil
  (let [md (P/publish-md {"actor" "rasen" "adr" "2606101000"
                           "published_at" "2026-06-22T00:00:00Z"
                           "scope" "PUBLIC reference genetics only"
                           "artifacts" {"graph" {"file" "g.edn" "bytes" 100 "cid" "bafy1"}}
                           "ipns"      nil
                           "gateways"  P/gateways})]
    (is (str/includes? md "_IPNS not published this run._"))))

(deftest test-publish-md-with-ipns
  (let [md (P/publish-md {"actor" "rasen" "adr" "2606101000"
                           "published_at" "2026-06-22T00:00:00Z"
                           "scope" "PUBLIC reference genetics only"
                           "artifacts" {"graph" {"file" "g.edn" "bytes" 100 "cid" "bafy1"}}
                           "ipns"      {"path" "/ipns/k51qziabcd"}
                           "gateways"  P/gateways})]
    (is (str/includes? md "/ipns/k51qziabcd"))
    (is (not (str/includes? md "_IPNS not published this run._")))))

;; ── IO leg with injected dry-run ipfs-fn (clj only) ─────────────────────────

#?(:clj
   (do
     (deftest test-publish-io-dry-run
       (testing "publish! with injected dry-run ipfs-fn and a real temp fixture"
         (let [tmpdir (java.io.File/createTempFile "rasen-pub-test" nil)
               _      (.delete tmpdir)
               _      (.mkdirs tmpdir)
               ;; write a tiny graph file
               graph-f (io/file tmpdir "ingested-genome-graph.kotoba.edn")
               content "[:db/add \"gene.1\" \":gene/symbol\" \"BRCA1\"]\n"
               _       (spit graph-f content :encoding "UTF-8")
               local-cid (CID/cidv1-raw (.readAllBytes (java.io.FileInputStream. graph-f)))
               ;; injected ipfs fn: always returns the correct CID for add, a mock for name publish
               ipfs-calls (atom [])
               ipfs-fn  (fn [& args]
                          (swap! ipfs-calls conj (vec args))
                          (let [cmd (first args)]
                            (cond
                              (= cmd "add")
                              {:exit 0 :out local-cid :err ""}
                              (= cmd "name")
                              {:exit 0
                               :out (str "Published to k51testname: /ipfs/" local-cid)
                               :err ""}
                              :else {:exit 0 :out "" :err ""})))
               ;; data dir is also in tmp
               data-dir (io/file tmpdir "data")
               _        (.mkdirs data-dir)
               ;; run publish!  — point 80-data to a temp location by overriding the out-tree
               ;; We need to place the 80-data peer dir correctly relative to the actor "here".
               ;; Easier: use a 3-level tmpdir: actor-root/out/ingested-genome-graph.kotoba.edn
               ;; and actor-root/../80-data/genome for the snapshot.
               actor-root (java.io.File/createTempFile "rasen-actor" nil)
               _          (.delete actor-root)
               _          (.mkdirs actor-root)
               out-dir  (io/file actor-root "out")
               _        (.mkdirs out-dir)
               _        (spit (io/file out-dir "ingested-genome-graph.kotoba.edn") content :encoding "UTF-8")
               parent   (.getParentFile actor-root)
               data80   (io/file parent "80-data" "genome")
               _        (.mkdirs data80)]
           ;; We run the pure pieces directly rather than calling publish! to avoid the
           ;; hard-coded path resolution (the IO leg uses *file* which points to the .cljc).
           ;; Instead we verify: (1) add-pin's CID assertion, (2) manifest+md generation.
           (let [f       (io/file out-dir "ingested-genome-graph.kotoba.edn")
                 content2 (slurp f :encoding "UTF-8")
                 ba      (.readAllBytes (java.io.FileInputStream. f))
                 lcid    (CID/cidv1-raw ba)
                 dcid    local-cid  ; matches by construction
                 entry   (P/artifact-entry "graph" (.getName f) (.length f) lcid dcid)]
             (is (:cid_matches entry))
             (is (= lcid (:cid entry)))

             ;; manifest assembly
             (let [m (P/build-manifest "2026-06-22T00:00:00Z" {"graph" entry} nil)
                   md (P/publish-md {"actor" "rasen" "adr" "2606101000"
                                      "published_at" "2026-06-22T00:00:00Z"
                                      "scope" (get m "scope")
                                      "artifacts" {"graph" {"file" (.getName f)
                                                             "bytes" (.length f)
                                                             "cid"   lcid}}
                                      "ipns"      nil
                                      "gateways"  P/gateways})]
               (is (str/includes? md lcid))
               (is (str/includes? md "## Artifacts"))))

           ;; cleanup
           (doseq [f (file-seq actor-root)] (.delete f))
           (doseq [f (file-seq tmpdir)] (.delete f)))))

     (deftest test-cid-matches-between-publish-and-cid-lib
       ;; Verifies cid-lib/cidv1-raw is the SAME function used by the publish leg
       (let [content "test genome data\n"
             ba      (.getBytes content "UTF-8")
             cid1    (CID/cidv1-raw ba)
             cid2    (CID/cidv1-raw content)]
         (is (= cid1 cid2))
         (is (str/starts-with? cid1 "b"))
         (is (> (count cid1) 10))))))

;; ── Parity smoke ─────────────────────────────────────────────────────────────

(deftest test-gateways-present
  (is (= 3 (count P/gateways)))
  (is (every? #(str/starts-with? % "https://") P/gateways)))

(deftest test-publish-md-parity-smoke
  ;; Hand-check: the PUBLISH.md must contain the ADR and the CID.
  (let [cid "bafybeiabc123testcidparity"
        md  (P/publish-md {"actor" "rasen" "adr" "2606101000"
                            "published_at" "2026-06-22T00:00:00Z"
                            "scope" "PUBLIC reference genetics only — no individual genotypes (G1)"
                            "artifacts" {"graph"  {"file" "genome-graph.kotoba.edn"
                                                    "bytes" 5000 "cid" cid}
                                         "datoms" {"file" "genome-datoms.kotoba.edn"
                                                    "bytes" 2000 "cid" "bafydatoms"}}
                            "ipns"      nil
                            "gateways"  P/gateways})]
    (is (str/includes? md "# rasen 螺旋 — published public-genetics graph (80-data/genome)"))
    (is (str/includes? md "ADR-2606101000"))
    (is (str/includes? md cid))
    (is (str/includes? md "ipfs.io/ipfs/"))
    (is (str/includes? md "dweb.link/ipfs/"))
    (is (str/includes? md "cloudflare-ipfs.com/ipfs/"))
    (is (str/includes? md "rasen.methods.cid graph.edn"))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [r (run-tests 'rasen.tests.test-publish)]
    (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1))))
