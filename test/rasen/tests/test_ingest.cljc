(ns rasen.tests.test-ingest
  "rasen 螺旋 — ingest + content-address tests (ADR-2606101000). 1:1 Clojure port of
  tests/test_ingest.py. Pure stdlib, NETWORK-FREE.

  Exercises the ingest normalisation and the kotoba IPFS content-address on bundled fixtures
  (real MyVariant.info / MyGene / Reactome response shapes) — no network needed. JSON fixtures
  are read with cheshire (json.loads analogue, string-keyed). The live fetch path is covered by
  running `methods/ingest.py` on an operator/mesh node (G7)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [kotoba.lang.text :as str]
            [clojure.set]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [rasen.methods.cid :as cid]
            [rasen.methods.ingest :as ingest]))

(def actor-dir (io/file "."))
(def fix (io/file actor-dir "wire" "fixtures" "myvariant_rs334.json"))
(def go-fix (io/file actor-dir "wire" "fixtures" "mygene_brca1_go.json"))
(def react-fix (io/file actor-dir "wire" "fixtures" "reactome_brca1.json"))

;; data/ingest-sources.edn is now Datomic/Datascript tx-data (manifest/edn-datomize.cljs
;; wrap-map! shape: `[{:db/id -1 :ingest/... ...}]`, non-scalar values pr-str'd to a "blob"
;; string attr) rather than a bare map — unwrap the single entity and reparse any blob attrs
;; this test needs back into maps via the same reader (round-trips: :ingest/population-map /
;; :ingest/clinsig-map were plain string→keyword maps pre-transform and still parse to the
;; same shape post-unblob).
(defn- unblob [v]
  (if (string? v)
    (let [parsed (ingest/read-edn v)]
      (if (coll? parsed) parsed v))
    v))
(def sources (first (ingest/read-edn (slurp (io/file actor-dir "data" "ingest-sources.edn")))))
(def pop-map (unblob (get sources ":ingest/population-map")))
(def clinsig-map (unblob (get sources ":ingest/clinsig-map")))

(defn- read-json [f] (json/parse-string (slurp f)))

(defn- gl [e] (double (get e ":en/grasping-load")))

(deftest test-go-pathways-deduped-weighted-bounded
  "GO ingest: dedupe by GO id (best evidence wins), cap per gene, emit :participates-in."
  (let [hit (read-json go-fix)
        [pw-nodes edges] (ingest/build-gene-pathways hit "gene.brca1" "BP" 6)]
    ;; 8 BP rows, but GO:0006281 and GO:0000724 each appear twice → 6 distinct terms
    (is (= 6 (count pw-nodes)) (str "expected 6 deduped pathways, got " (count pw-nodes)))
    (is (= 6 (count edges)))
    ;; every node is a public GO pathway, every edge is gene→pathway :participates-in
    (doseq [[_ n] pw-nodes]
      (is (and (= ":pathway" (get n ":genome/kind")) (= ":GO" (get n ":pathway/source"))))
      (is (str/starts-with? (get n ":pathway/acc") "GO:")))
    (doseq [e edges]
      (is (and (= "gene.brca1" (get e ":en/from")) (= ":participates-in" (get e ":en/kind"))))
      (is (contains? pw-nodes (get e ":en/to")))
      (is (and (< 0.0 (gl e)) (<= (gl e) 1.0))))
    ;; DNA repair (GO:0006281) kept its BEST evidence (IDA experimental 0.9, not IEA 0.4)
    (let [dna-repair (first (filter #(= "pw.go-0006281" (get % ":en/to")) edges))]
      (is (< (Math/abs (- (gl dna-repair) 0.9)) 1e-9) "best-evidence weight not kept"))))

(deftest test-go-pathways-capped
  "max_per_gene caps the bounded slice (G5 honesty)."
  (let [hit (read-json go-fix)
        [pw-nodes edges] (ingest/build-gene-pathways hit "gene.brca1" "BP" 3)]
    (is (and (= 3 (count pw-nodes)) (= 3 (count edges))))
    ;; the cap keeps the HIGHEST-evidence terms (all returned weights ≥ any dropped term's)
    (let [kept (sort (map gl edges))]
      (is (>= (first kept) 0.7) (str "cap should keep high-evidence terms first, got " kept)))))

(deftest test-reactome-pathways-deduped-topfirst-bounded
  "Reactome: dedupe by stId (top-level/lowest maxDepth wins), cap, emit :participates-in."
  (let [entries (read-json react-fix)
        [pw-nodes edges] (ingest/build-reactome-pathways entries "gene.brca1" 0.7 5)]
    ;; 6 distinct R-HSA stIds (R-HSA-73894 appears twice), 1 row has no stId → 5 capped
    (is (and (= 5 (count pw-nodes)) (= 5 (count edges))))
    (doseq [[pid n] pw-nodes]
      (is (and (= ":pathway" (get n ":genome/kind")) (= ":reactome" (get n ":pathway/source"))))
      (is (and (str/starts-with? (get n ":pathway/acc") "R-HSA-")
               (str/starts-with? pid "pw.react-"))))
    (doseq [e edges]
      (is (and (= "gene.brca1" (get e ":en/from")) (= ":participates-in" (get e ":en/kind"))))
      (is (< (Math/abs (- (gl e) 0.7)) 1e-9)))
    ;; the cap keeps the most TOP-LEVEL pathways: DNA Repair (maxDepth 1) must survive
    (is (contains? pw-nodes "pw.react-r-hsa-73894"))))

(deftest test-reactome-disabled-or-empty-is-safe
  "No entries / empty list → no nodes, no edges (defensive)."
  (doseq [empty [[] nil [{"name" ["x"]}]]]
    (let [[pw-nodes edges] (ingest/build-reactome-pathways empty "gene.x" 0.7 5)]
      (is (and (= {} pw-nodes) (= [] edges))))))

(deftest test-cid-matches-ipfs-vector
  "CIDv1 raw sha2-256 — byte-identical to `ipfs add --cid-version=1 --raw-leaves`."
  ;; vector verified against ipfs 0.41.0 for the UTF-8 bytes of "rasen 螺旋\n"
  (is (= (cid/cidv1-raw "rasen 螺旋\n")
         "bafkreielxn6z4bf6xlhfe3q57dd6tvgjl6yycsvzifm55lhie4ueup3ova"))
  ;; determinism + sensitivity
  (is (= (cid/cidv1-raw "a") (cid/cidv1-raw "a")))
  (is (not= (cid/cidv1-raw "a") (cid/cidv1-raw "b")))
  (is (str/starts-with? (cid/cidv1-raw "") "bafkrei")))  ;; CIDv1/raw/sha2-256 multibase prefix

(deftest test-normalise-variant-disclosed-and-bounded
  (let [hit (read-json fix)
        [vnode gene-id phenos edges] (ingest/normalise-variant "rs334" hit clinsig-map pop-map)]
    (is (and (= ":variant" (get vnode ":genome/kind")) (= "rs334" (get vnode ":variant/rsid"))))
    (is (= "gene.hbb" gene-id))
    ;; "not provided" significance is skipped; "Pathogenic" + "protective" are kept (N3 disclosed)
    (let [clinsigs (set (for [e edges :when (= ":associated-with" (get e ":en/kind"))]
                          (get e ":en/clinsig")))]
      (is (and (contains? clinsigs ":pathogenic") (contains? clinsigs ":protective"))))
    ;; MONDO condition becomes a coded phenotype node
    (is (contains? phenos "ph.mondo-0011382"))
    (is (= "MONDO:0011382" (get-in phenos ["ph.mondo-0011382" ":phenotype/code"])))))

(deftest test-g1-allele-frequency-is-super-population-aggregate-only
  "G1: only the 6 declared SUPER-POPULATION aggregates; NEVER _female/_male or individual."
  (let [hit (read-json fix)
        [_ _ _ edges] (ingest/normalise-variant "rs334" hit clinsig-map pop-map)
        af-edges (filter #(= ":allele-frequency" (get % ":en/kind")) edges)
        allowed-pops (set (for [v (vals pop-map)]
                            (str "pop." (str/lower (if (str/starts-with? v ":") (subs v 1) v)))))]
    (is (seq af-edges) "no aggregate allele-frequency edges produced")
    (doseq [e af-edges]
      (is (contains? allowed-pops (get e ":en/from"))
          (str "non-aggregate population leaked: " (get e ":en/from")))
      (is (and (<= 0.0 (gl e)) (<= (gl e) 1.0))))
    ;; the sex-stratified fixture fields (af_afr_female/_male) MUST NOT have produced edges
    (is (<= (count af-edges) (count pop-map))
        "more freq edges than declared super-populations")))

(deftest test-no-individual-level-fields-anywhere
  (let [hit (read-json fix)
        [vnode _ phenos edges] (ingest/normalise-variant "rs334" hit clinsig-map pop-map)
        forbidden #{":sample/id" ":individual/id" ":patient/id" ":genotype/call"
                    ":sequence/raw" ":family/id"}]
    (doseq [m (concat [vnode] (vals phenos) edges)]
      (let [leaked (clojure.set/intersection (set (keys m)) forbidden)]
        (is (empty? leaked) (str "individual-level field leaked: " leaked))))))

(deftest test-serialise-graph-roundtrips-and-addresses
  "Normalised graph serialises to EDN, reloads, and content-addresses deterministically."
  (let [hit (read-json fix)
        [vnode gene-id phenos edges0] (ingest/normalise-variant "rs334" hit clinsig-map pop-map)
        nodes0 (ingest/population-nodes pop-map)
        nodes1 (assoc nodes0 (get vnode ":genome/id") vnode)
        nodes2 (merge nodes1 phenos)
        nodes (assoc nodes2 gene-id {":genome/id" gene-id ":genome/kind" ":gene"
                                     ":genome/label" "HBB" ":gene/symbol" "HBB"
                                     ":gene/taxon" ":homo-sapiens"
                                     ":genome/sourcing" ":authoritative"})
        edges (filterv #(and (contains? nodes (get % ":en/from"))
                             (contains? nodes (get % ":en/to"))) edges0)
        prov {"sources" [{"url" "fixture://myvariant"}] "counts" {"variants" 1}}
        edn (ingest/serialise-graph nodes edges prov)]
    (is (= edn (ingest/serialise-graph nodes edges prov)) "serialisation not deterministic")
    (let [c1 (cid/cidv1-raw edn)
          c2 (cid/cidv1-raw edn)]
      (is (and (= c1 c2) (str/starts-with? c1 "bafkrei"))))
    ;; reloads through the canonical analyzer loader with no dangling edges
    (let [tmp (java.io.File/createTempFile "rasen-graph" ".edn")]
      (spit tmp edn)
      (let [[n2 e2] (ingest/load tmp)]
        (.delete tmp)
        (is (contains? n2 (get vnode ":genome/id")))
        (doseq [e e2]
          (is (and (contains? n2 (get e ":en/from")) (contains? n2 (get e ":en/to")))
              "dangling edge after roundtrip"))))))

;; The Python __main__ demo runner is intentionally omitted.
