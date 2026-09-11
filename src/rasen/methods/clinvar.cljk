(ns rasen.methods.clinvar
  "clinvar.cljc — rasen 螺旋 FULL-CORPUS ClinVar normalisation surface (ADR-2609062000).

  Where `rasen.methods.ingest` pulls a BOUNDED, REPRESENTATIVE slice through the
  MyGene/MyVariant JSON APIs (one HTTP round trip per gene), this namespace normalises the
  ClinVar bulk release `variant_summary.txt` — the whole public variant corpus — into the same
  genome-ontology graph. Same node/edge shapes, same ':…' string house style, so the two
  producers are interchangeable downstream (analyze / datom-emit / kotoba ledger / publish).

  G1 — PUBLIC reference only, and here that constraint has TEETH. variant_summary carries
       PRECISE, RE-IDENTIFIABLE COORDINATES (Chromosome / Start / Stop / PositionVCF /
       ReferenceAlleleVCF / AlternateAlleleVCF). The genome-ontology permits COARSE cytoband
       ONLY (`:gene/cytoband`, 'NEVER a precise/re-identifiable coordinate'). Those columns are
       therefore not merely unused — `g1-violations` REFUSES any node carrying them, and
       `row->graph` runs that check on everything it emits. A silent omission would be
       indistinguishable from a parser that forgot; a refusal is not.
  G1 — no individual genotypes. variant_summary is an aggregate curated-assertion table: one
       row is a (variant, condition, assembly) assertion, never a person and never a sample.
  G3 — non-adjudicating (N3). ClinicalSignificance and ReviewStatus are copied as DISCLOSED
       curated facts. `:en/grasping-load` is (clinsig weight × review-status confidence) — both
       factors are ClinVar's own published scales, not a rasen verdict about a variant.
  G5 — sourcing honesty. Every record from this corpus is ':authoritative'.

  Pure and network-free by construction: the caller supplies parsed lines. The 442 MB gzip
  stream that produces those lines is host I/O and stays in the G7-gated runner."
  (:require [kotoba.lang.text :as str]))

;; ── G1: columns that must never reach the graph ──────────────────────────────
;; Not a comment. `g1-violations` is applied to every emitted node.
(def coordinate-attrs
  "Node attrs that would carry a precise, re-identifiable genomic coordinate.
  The genome-ontology has no such attr; this set exists so that a future edit which invents
  one is refused rather than silently accepted."
  #{":variant/chromosome" ":variant/start" ":variant/stop" ":variant/position"
    ":variant/ref-allele" ":variant/alt-allele" ":variant/vcf-position"
    ":gene/start" ":gene/stop" ":gene/chromosome"})

(defn g1-violations
  "Return the coordinate-bearing attrs present on `node` (empty when clean).
  Refuses to answer for a non-map: a nil/garbage node is UNVERIFIED, not clean, and must not
  return the same empty-set that a checked-and-clean node returns."
  [node]
  (when-not (map? node)
    (throw (ex-info "g1-violations: not a node map — cannot report clean" {:got (type node)})))
  (into (sorted-set) (filter coordinate-attrs (keys node))))

(defn assert-g1!
  "Throw if `node` carries a precise coordinate; else return it unchanged."
  [node]
  (let [bad (g1-violations node)]
    (when (seq bad)
      (throw (ex-info "G1: precise coordinate refused" {:attrs (vec bad) :id (get node ":genome/id")})))
    node))

;; ── ClinVar published scales → genome-ontology values (DISCLOSED, N3) ────────
(def clinsig-weight
  "Mirror of schema/genome-ontology.edn :clinsig/weight (the DISCLOSED evidence scale)."
  {":pathogenic" 1.0 ":likely-pathogenic" 0.8 ":risk-factor" 0.5 ":drug-response" 0.4
   ":uncertain" 0.3 ":likely-benign" 0.1 ":benign" 0.05 ":protective" 0.1})

(def review-status-confidence
  "ClinVar's own review-status (star) scale → 0..1 confidence. Copied, not judged (N3).
  Keys are lower-cased ReviewStatus strings as they appear in variant_summary."
  {"practice guideline" 1.0
   "reviewed by expert panel" 0.9
   "criteria provided, multiple submitters, no conflicts" 0.75
   "criteria provided, single submitter" 0.55
   "criteria provided, conflicting classifications" 0.45
   "criteria provided, conflicting interpretations" 0.45
   "no assertion criteria provided" 0.25
   "no classification provided" 0.2
   "no classification for the single variant" 0.2
   "no interpretation for the single variant" 0.2
   "no classifications from unflagged records" 0.2})

(def ^:private clinsig-alias
  "Lower-cased ClinVar significance token → genome-ontology clinsig string.
  'conflicting classifications of pathogenicity' maps to ':uncertain' because the ontology's
  enum has no conflicting category; the conflict itself survives in the review-status
  confidence (0.45), so the disagreement is not erased."
  {"pathogenic" ":pathogenic"
   "likely pathogenic" ":likely-pathogenic"
   "pathogenic/likely pathogenic" ":pathogenic"
   "pathogenic, low penetrance" ":pathogenic"
   "likely pathogenic, low penetrance" ":likely-pathogenic"
   "established risk allele" ":risk-factor"
   "likely risk allele" ":risk-factor"
   "uncertain risk allele" ":uncertain"
   "benign" ":benign"
   "likely benign" ":likely-benign"
   "benign/likely benign" ":benign"
   "uncertain significance" ":uncertain"
   "conflicting classifications of pathogenicity" ":uncertain"
   "conflicting interpretations of pathogenicity" ":uncertain"
   "risk factor" ":risk-factor"
   "association" ":risk-factor"
   "drug response" ":drug-response"
   "protective" ":protective"
   "confers sensitivity" ":drug-response"})

(defn clinsig->key
  "ClinVar ClinicalSignificance cell → genome-ontology clinsig string, or nil when the cell
  carries no classification ('not provided', 'other', 'Affects', blank).
  A cell may combine a primary call with modifiers ('Pathogenic; risk factor'); the primary
  token wins and the modifiers are dropped — the ontology's :en/clinsig is single-valued."
  [cell]
  (when (string? cell)
    (some (fn [tok]
            (get clinsig-alias (str/lower (str/trim tok))))
          (str/split (str/trim cell) #";"))))

(def ^:private type-category
  {"single nucleotide variant" ":snv"
   "deletion" ":indel" "insertion" ":indel" "indel" ":indel"
   "duplication" ":indel" "tandem duplication" ":indel"
   "copy number gain" ":cnv" "copy number loss" ":cnv"
   "microsatellite" ":repeat-expansion"
   "inversion" ":structural" "translocation" ":structural"
   "complex" ":structural" "fusion" ":structural"})

(defn type->category
  "ClinVar Type cell → :variant/category string, or nil when unmapped."
  [cell]
  (when (string? cell) (get type-category (str/lower (str/trim cell)))))

(defn review->confidence
  "ClinVar ReviewStatus cell → 0..1. Unknown/blank status is the LOWEST tier, never the
  default-highest: an unrecognised status is unmeasured confidence, not good confidence."
  [cell]
  (get review-status-confidence (str/lower (str/trim (or cell ""))) 0.2))

(defn- round6 [x] #?(:clj (double (/ (Math/round (* 1e6 (double x))) 1e6))
                     :cljs (/ (js/Math.round (* 1e6 x)) 1e6)))

(defn- blank-cell?
  [s] (or (nil? s) (= "" s) (= "-" s) (= "na" (str/lower (str s)))))

(defn- slug
  [s]
  (-> (str/lower (str s))
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+" "")
      (str/replace #"-+$" "")))

;; ── phenotype references ────────────────────────────────────────────────────
(def ^:private db-preference
  "[db id-prefix] in preference order. MONDO's own value already carries the 'MONDO:' prefix
  ('MONDO:MONDO:0013342' is db=MONDO value=MONDO:0013342), so its id-prefix is bare 'ph.' —
  giving 'ph.mondo-0013342', byte-identical to what `rasen.methods.ingest` produces for the
  same condition. A second prefix here would split one condition across two node ids."
  [["mondo" "ph."] ["omim" "ph.omim-"] ["medgen" "ph.medgen-"] ["orphanet" "ph.orphanet-"]])

(defn parse-phenotype-ids
  "One PhenotypeIDS group ('MONDO:MONDO:0013342,MedGen:C3150901,OMIM:613647') → {db value}.
  Each entry is '<db>:<value>' split on the FIRST colon only, because MONDO values themselves
  contain a colon ('MONDO:MONDO:0013342' is db=MONDO value=MONDO:0013342)."
  [group]
  (reduce (fn [m entry]
            (let [i (str/index-of entry ":")]
              (if (and i (pos? i))
                (assoc m (str/lower (subs entry 0 i)) (subs entry (inc i)))
                m)))
          {} (remove str/blank? (str/split (or group "") #","))))

(defn phenotype-node
  "Build [ph-id node] for one (ids-group, name) pair, or nil when the pair names no condition.
  Prefers MONDO > OMIM > MedGen > Orphanet; falls back to a slug of the name."
  [group name]
  (let [ids  (parse-phenotype-ids group)
        nm   (str/trim (or name ""))
        drop? (contains? #{"" "not provided" "not specified" "see cases"} (str/lower nm))
        hit  (some (fn [[db prefix]]
                     (when-let [v (get ids db)]
                       [(str prefix (str/replace (str/lower v) ":" "-"))
                        (if (= db "mondo") v (str (case db "omim" "OMIM" "medgen" "MedGen"
                                                        "orphanet" "Orphanet" db) ":" v))]))
                   db-preference)]
    (cond
      hit (let [[ph-id code] hit]
            [ph-id (cond-> {":genome/id" ph-id ":genome/kind" ":phenotype"
                            ":genome/label" (if drop? code nm)
                            ":genome/sourcing" ":authoritative"}
                     code (assoc ":phenotype/code" code))])
      drop? nil
      :else (let [s (slug nm)
                  ph-id (str "ph." (subs s 0 (min 48 (count s))))]
              [ph-id {":genome/id" ph-id ":genome/kind" ":phenotype"
                      ":genome/label" nm ":genome/sourcing" ":authoritative"}]))))

(defn phenotype-refs
  "PhenotypeIDS + PhenotypeList cells → [[ph-id node] …], index-aligned on '|'."
  [ids-cell list-cell]
  (let [groups (str/split (or ids-cell "") #"\|" -1)
        names  (str/split (or list-cell "") #"\|" -1)
        n      (max (count groups) (count names))]
    (into [] (keep (fn [i] (phenotype-node (nth groups i nil) (nth names i nil)))
                   (range n)))))

;; ── row → graph ─────────────────────────────────────────────────────────────
(def columns
  "The variant_summary columns this normaliser reads. Coordinate columns are absent on purpose
  (G1) — see `coordinate-attrs`."
  ["Type" "GeneSymbol" "HGNC_ID" "ClinicalSignificance" "RS# (dbSNP)" "PhenotypeIDS"
   "PhenotypeList" "Assembly" "Cytogenetic" "ReviewStatus" "VariationID" "Name"])

(defn header-index
  "variant_summary header line → {column-name index}. The leading '#' on '#AlleleID' is stripped."
  [line]
  (into {} (map-indexed (fn [i c] [(str/replace (str/trim c) #"^#" "") i])
                        (str/split (or line "") #"\t" -1))))

(defn- cell [row idx col] (let [i (get idx col)] (when i (str/trim (nth row i "")))))

(defn row->graph
  "One variant_summary row (already tab-split) → {:nodes {id node} :edges [edge …]}, or nil.

  Returns nil — not an empty graph — when the row is skipped, so a caller can tell 'this row
  said nothing' apart from 'this row was never looked at'. Rows are skipped when the assembly
  is not `assembly` (every variant appears once per assembly; taking one de-duplicates) or
  when no usable variant identity exists.

  Emits: the variant node; :located-in edges to each named gene (with the gene node, carrying
  COARSE cytoband only); phenotype nodes; and :associated-with edges carrying the DISCLOSED
  clinsig and (clinsig weight × review confidence) as :en/grasping-load."
  ([row idx] (row->graph row idx "GRCh38"))
  ([row idx assembly]
   (let [asm (cell row idx "Assembly")]
     (when (= asm assembly)
       (let [rs   (cell row idx "RS# (dbSNP)")
             vid* (cell row idx "VariationID")
             rsid (when-not (or (blank-cell? rs) (= "-1" rs)) (str "rs" rs))
             vid  (cond rsid (str "var." rsid)
                        (not (blank-cell? vid*)) (str "var.clinvar-" vid*)
                        :else nil)]
         (when vid
           (let [cyto    (cell row idx "Cytogenetic")
                 label   (or rsid (str "ClinVar " vid*))
                 vnode   (cond-> {":genome/id" vid ":genome/kind" ":variant"
                                  ":genome/label" label ":genome/sourcing" ":authoritative"}
                           rsid (assoc ":variant/rsid" rsid)
                           (type->category (cell row idx "Type"))
                           (assoc ":variant/category" (type->category (cell row idx "Type"))))
                 syms    (->> (str/split (or (cell row idx "GeneSymbol") "") #";")
                              (map str/trim) (remove blank-cell?) distinct)
                 genes   (into {} (map (fn [s]
                                         (let [gid (str "gene." (str/lower s))]
                                           [gid (cond-> {":genome/id" gid ":genome/kind" ":gene"
                                                         ":genome/label" s ":gene/symbol" s
                                                         ":gene/taxon" ":homo-sapiens"
                                                         ":genome/sourcing" ":authoritative"}
                                                  (not (blank-cell? cyto))
                                                  (assoc ":gene/cytoband" cyto))]))
                                       syms))
                 loc-edges (mapv (fn [gid] {":en/from" vid ":en/to" gid ":en/kind" ":located-in"
                                            ":en/grasping-load" 1.0 ":en/sourcing" ":authoritative"})
                                 (keys genes))
                 clinsig (clinsig->key (cell row idx "ClinicalSignificance"))
                 conf    (review->confidence (cell row idx "ReviewStatus"))
                 phenos  (phenotype-refs (cell row idx "PhenotypeIDS") (cell row idx "PhenotypeList"))
                 as-edges (when clinsig
                            (mapv (fn [[ph-id _]]
                                    {":en/from" vid ":en/to" ph-id ":en/kind" ":associated-with"
                                     ":en/clinsig" clinsig
                                     ":en/grasping-load" (round6 (* (get clinsig-weight clinsig 0.3) conf))
                                     ":en/sourcing" ":authoritative"})
                                  phenos))
                 nodes   (merge {vid vnode} genes (into {} phenos))]
             (run! assert-g1! (vals nodes))
             {:nodes nodes :edges (vec (concat loc-edges as-edges))})))))))

(defn merge-graph
  "Fold one row's graph into an accumulator. Node ids are stable, so a later row's node for the
  same id is merged over the earlier one (ClinVar repeats a gene on every one of its variants);
  edges are de-duplicated on their full value."
  [acc g]
  (if (nil? g)
    (update acc :skipped inc)
    (-> acc
        (update :nodes (fn [m] (reduce (fn [m [k v]] (update m k merge v)) m (:nodes g))))
        (update :edges into (:edges g))
        (update :rows inc))))

(def empty-acc {:nodes {} :edges #{} :rows 0 :skipped 0})

(defn rows->graph
  "Normalise a seq of tab-split rows against `idx`. Returns the accumulator, including the
  counts — a caller that reports coverage must be able to say how many rows it actually saw,
  not just how many nodes came out."
  ([rows idx] (rows->graph rows idx "GRCh38"))
  ([rows idx assembly]
   (reduce (fn [acc row] (merge-graph acc (row->graph row idx assembly))) empty-acc rows)))
