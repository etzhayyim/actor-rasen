(ns rasen.methods.ingest
  "rasen 螺旋 — PUBLIC genetics ingest → kotoba EDN/Datom → IPFS content-address (ADR-2606101000).
  1:1 Clojure port of `methods/ingest.py` (the pure, network-free normalisation surface).

  OUTWARD-GATED CELL (G7): the only rasen cell that reaches the network. It pulls a BOUNDED,
  REPRESENTATIVE slice of PUBLIC reference genetics declared in `data/ingest-sources.edn` and
  normalises it into the genome-ontology kotoba graph, then content-addresses the artifact to a
  kotoba IPFS CID.

  G1 — PUBLIC reference only (gene reference models, public variant ids, DISCLOSED ClinVar
       significance, gnomAD SUPER-POPULATION AGGREGATE allele frequencies). No sample / cohort /
       individual / family data — by construction.
  G3 — non-adjudicating. ClinVar clinical-significance is copied as a DISCLOSED fact (N3).
  G5 — sourcing honesty. Bounded slice; every record :authoritative.

  This port covers the deterministic, NETWORK-FREE surface exercised by tests
  (normalisation + EDN serialisation + content-address); the live `urllib`/`ipfs`/`subprocess`
  pipeline (`ingest`/`main`/`_get_json`) is host I/O kept behind #?(:clj …). House style: Python
  ':…' keyword strings stay strings; pure fns; reuses rasen.methods.analyze + rasen.methods.cid."
  (:require [kotoba.lang.text :as str]
            [rasen.methods.analyze :as analyze]
            [rasen.methods.cid :as cidlib]
            #?(:clj [clojure.java.io :as io])))

;; re-export the canonical loader surface (tests call ingest/read-edn + ingest/load)
(def read-edn analyze/read-edn)

(defn load
  "Return [nodes-by-id edges] from a public-genetics EDN graph. (1:1 of analyze.py `load`.)
  Accepts a parsed-forms vector OR (#?(:clj) a file path/File). Mirrors Python's (nodes, edges)."
  [forms-or-path]
  (let [{:keys [nodes edges]}
        (cond
          (sequential? forms-or-path) (analyze/load-graph forms-or-path)
          #?@(:clj [:else (analyze/load-file* forms-or-path)]
              :cljs [:else (throw (js/Error. "load needs parsed forms in cljs"))]))]
    [nodes edges]))

(defn- lstrip-colon-str [s] (if (and (string? s) (str/starts-with? s ":")) (subs s 1) s))

(defn- round6
  "Python round(x, 6) — banker's rounding (HALF_EVEN) to 6 decimal places."
  [x]
  #?(:clj (-> (java.math.BigDecimal/valueOf (double x))
              (.setScale 6 java.math.RoundingMode/HALF_EVEN)
              (.doubleValue))
     :cljs (let [f 1e6] (/ (js/Math.round (* x f)) f))))

(defn- slug
  "Port of _slug: alnum lowered, everything else → '-', strip leading/trailing '-', collapse '--'."
  [s]
  (-> (apply str (map (fn [c]
                        (if #?(:clj (Character/isLetterOrDigit c)
                               :cljs (re-matches #"[a-zA-Z0-9]" (str c)))
                          (str/lower (str c))
                          "-"))
                      s))
      (#(let [s %] ;; strip("-")
          (-> s (str/replace #"^-+" "") (str/replace #"-+$" ""))))
      (str/replace "--" "-")))

;; ── float formatting mirroring Python's `{v:g}` (used by _fmt) ────────────────
(defn- fmt-g
  [v]
  (let [d (double v)]
    (if (and (not #?(:clj (Double/isInfinite d) :cljs (infinite? d)))
             (not #?(:clj (Double/isNaN d) :cljs (js/isNaN d)))
             (== d #?(:clj (Math/rint d) :cljs (js/Math.round d)))
             (< #?(:clj (Math/abs d) :cljs (js/Math.abs d)) 1e15))
      (str (long d))
      (let [s (#?(:clj format :cljs identity) "%.6g" d)]
        (if (str/includes? s ".")
          (-> s (str/replace #"0+$" "") (str/replace #"\.$" ""))
          s)))))

;; ── normalisation ────────────────────────────────────────────────────────────
(defn gene-node-from-mygene
  [hit symbol]
  (let [ens (or (get hit "ensembl") {})
        ens-id (cond
                 (and (map? ens)) (get ens "gene")
                 (and (sequential? ens) (seq ens)) (get (first ens) "gene")
                 :else nil)
        taxid (get hit "taxid")
        taxon (cond (= taxid 9606) ":homo-sapiens"
                    taxid (str ":taxid-" taxid)
                    :else ":homo-sapiens")
        n (cond-> {":genome/id" (str "gene." (str/lower symbol))
                   ":genome/kind" ":gene"
                   ":genome/label" (or (get hit "name") symbol)
                   ":gene/symbol" symbol
                   ":gene/taxon" taxon
                   ":genome/sourcing" ":authoritative"}
            (get hit "map_location") (assoc ":gene/cytoband" (get hit "map_location"))
            ens-id (assoc ":gene/ensembl" ens-id))]
    n))

;; GO evidence code → representative confidence weight (DISCLOSED GO annotation is the fact, N3)
(def go-evidence-weight
  {"EXP" 0.9 "IDA" 0.9 "IPI" 0.9 "IMP" 0.9 "IGI" 0.9 "IEP" 0.9 "HDA" 0.9 "HMP" 0.9
   "TAS" 0.7 "NAS" 0.7 "IC" 0.7
   "IBA" 0.6 "IBD" 0.6
   "ISS" 0.5 "ISA" 0.5 "ISO" 0.5 "ISM" 0.5 "IGC" 0.5 "RCA" 0.5
   "IEA" 0.4})

(defn build-gene-pathways
  "From a MyGene hit's `go.<category>`, return [pathway-nodes edges] (:participates-in).
  PUBLIC GO annotations only. Dedupe by GO id, keep best evidence weight, cap max-per-gene (G5)."
  ([hit gene-id] (build-gene-pathways hit gene-id "BP" 6))
  ([hit gene-id category] (build-gene-pathways hit gene-id category 6))
  ([hit gene-id category max-per-gene]
   (let [go (or (get hit "go") {})
         terms0 (or (get go category) [])
         terms (if (map? terms0) [terms0] terms0)
         ;; best: GO id → [weight term] (best evidence wins; later only overrides if strictly >)
         best (reduce
               (fn [m t]
                 (if-not (map? t)
                   m
                   (let [gid (get t "id")]
                     (if (or (not gid) (not (str/starts-with? (str gid) "GO:")))
                       m
                       (let [w (get go-evidence-weight
                                    (str/upper (str (get t "evidence" ""))) 0.4)
                             term (or (get t "term") gid)]
                         (if (or (not (contains? m gid)) (> w (first (get m gid))))
                           (assoc m gid [w term])
                           m))))))
               {} terms)
         ranked (take max-per-gene
                      (sort-by (fn [[gid [w _]]] [(- (double w)) gid]) best))]
     (reduce
      (fn [[pw-nodes edges] [gid [w term]]]
        (let [pw-id (str "pw." (str/replace (str/lower gid) ":" "-"))]
          [(assoc pw-nodes pw-id {":genome/id" pw-id ":genome/kind" ":pathway"
                                  ":genome/label" term ":pathway/source" ":GO"
                                  ":pathway/acc" gid ":genome/sourcing" ":authoritative"})
           (conj edges {":en/from" gene-id ":en/to" pw-id ":en/kind" ":participates-in"
                        ":en/grasping-load" w ":en/sourcing" ":authoritative"})]))
      [{} []] ranked))))

(defn build-reactome-pathways
  "From Reactome UniProt-mapping `pathways`, return [pathway-nodes edges] (:participates-in).
  Dedupe by stId, prefer TOP-LEVEL (lowest maxDepth) then stId, cap max-per-gene; fixed weight."
  ([entries gene-id] (build-reactome-pathways entries gene-id 0.7 5))
  ([entries gene-id weight max-per-gene]
   (let [entries (cond (map? entries) [entries]
                       (nil? entries) []
                       :else entries)
         best (reduce
               (fn [m p]
                 (if-not (map? p)
                   m
                   (let [st (get p "stId")]
                     (if (or (not st) (not (str/starts-with? (str st) "R-")))
                       m
                       (let [depth (get p "maxDepth" 99)
                             name (or (get p "displayName")
                                      (first (or (get p "name") [st])))]
                         (if (or (not (contains? m st)) (< depth (first (get m st))))
                           (assoc m st [depth name])
                           m))))))
               {} entries)
         ranked (take max-per-gene
                      (sort-by (fn [[st [depth _]]] [depth st]) best))]
     (reduce
      (fn [[pw-nodes edges] [st [_ name]]]
        (let [pw-id (str "pw.react-" (str/lower st))]
          [(assoc pw-nodes pw-id {":genome/id" pw-id ":genome/kind" ":pathway"
                                  ":genome/label" name ":pathway/source" ":reactome"
                                  ":pathway/acc" st ":genome/sourcing" ":authoritative"})
           (conj edges {":en/from" gene-id ":en/to" pw-id ":en/kind" ":participates-in"
                        ":en/grasping-load" (double weight) ":en/sourcing" ":authoritative"})]))
      [{} []] ranked))))

(def ^:private clinsig-load-default
  {":pathogenic" 0.9 ":likely-pathogenic" 0.7 ":risk-factor" 0.5
   ":drug-response" 0.4 ":uncertain" 0.3 ":likely-benign" 0.1
   ":benign" 0.05 ":protective" 0.1})

(defn- as-list [x] (cond (nil? x) [] (sequential? x) x :else [x]))

(defn normalise-variant
  "Return [variant-node gene-id-or-nil phenotype-nodes edges] from a MyVariant hit.
  clinsig-map / pop-map are parsed EDN maps (string keys → ':…' string values)."
  [rsid hit clinsig-map pop-map]
  (let [vid (str "var." rsid)
        vcf (or (get hit "vcf") {})
        ref (get vcf "ref")
        alt (get vcf "alt")
        vnode (cond-> {":genome/id" vid ":genome/kind" ":variant" ":genome/label" rsid
                       ":variant/rsid" rsid ":genome/sourcing" ":authoritative"}
                (and ref alt)
                (assoc ":variant/category"
                       (if (and (= 1 (count ref)) (= 1 (count alt))) ":snv" ":indel")))
        ;; gene linkage from dbsnp.gene.symbol
        dbsnp (or (get hit "dbsnp") {})
        g0 (or (get dbsnp "gene") {})
        g (if (and (sequential? g0) (seq g0)) (first g0) g0)
        gsym (when (map? g) (get g "symbol"))
        gene-id (when gsym (str "gene." (str/lower gsym)))
        located-edges (if gene-id
                        [{":en/from" vid ":en/to" gene-id ":en/kind" ":located-in"
                          ":en/grasping-load" 1.0 ":en/sourcing" ":authoritative"}]
                        [])
        ;; ClinVar significance → :associated-with edges (DISCLOSED; N3)
        clinvar (or (get hit "clinvar") {})
        rcvs (as-list (or (get clinvar "rcv") []))
        clin-step
        (reduce
         (fn [{:keys [phenos edges seen]} rcv]
           (let [sig-raw (str/trim (or (get rcv "clinical_significance") ""))
                 clinsig (get clinsig-map (str/lower sig-raw))]
             (if-not clinsig
               {:phenos phenos :edges edges :seen seen}
               (let [cond0 (or (get rcv "conditions") {})
                     cond1 (if (sequential? cond0) (if (seq cond0) (first cond0) {}) cond0)
                     ids (or (get cond1 "identifiers") {})
                     mondo (get ids "mondo")
                     medgen (get ids "medgen")
                     name (or (get cond1 "name") "unspecified condition")
                     [ph-id code] (cond
                                    mondo [(str "ph." (str/replace (str/lower mondo) ":" "-")) mondo]
                                    medgen [(str "ph.medgen-" medgen) (str "MedGen:" medgen)]
                                    :else [(str "ph." (subs (slug name) 0 (min 48 (count (slug name))))) nil])
                     phenos (if (contains? phenos ph-id)
                              phenos
                              (assoc phenos ph-id
                                     (cond-> {":genome/id" ph-id ":genome/kind" ":phenotype"
                                              ":genome/label" name ":genome/sourcing" ":authoritative"}
                                       code (assoc ":phenotype/code" code))))
                     key [ph-id clinsig]]
                 (if (contains? seen key)
                   {:phenos phenos :edges edges :seen seen}
                   (let [load-default (get clinsig-load-default clinsig 0.3)]
                     {:phenos phenos
                      :edges (conj edges {":en/from" vid ":en/to" ph-id ":en/kind" ":associated-with"
                                          ":en/clinsig" clinsig ":en/grasping-load" load-default
                                          ":en/sourcing" ":authoritative"})
                      :seen (conj seen key)}))))))
         {:phenos {} :edges [] :seen #{}}
         rcvs)
        phenos (:phenos clin-step)
        ;; gnomAD SUPER-POPULATION AGGREGATE allele frequencies (G1: aggregate only)
        af (or (get (or (get hit "gnomad_genome") {}) "af") {})
        freq-edges
        (reduce
         (fn [acc [field popcode]]
           (let [v (get af field)]
             (if (number? v)
               (let [pop-id (str "pop." (str/lower (lstrip-colon-str popcode)))]
                 (conj acc {":en/from" pop-id ":en/to" vid ":en/kind" ":allele-frequency"
                            ":en/grasping-load" (round6 (double v)) ":en/sourcing" ":authoritative"}))
               acc)))
         [] pop-map)]
    [vnode gene-id phenos (vec (concat located-edges (:edges clin-step) freq-edges))]))

(defn population-nodes
  "Build {pop-id node} for the distinct super-population codes in pop-map (AGGREGATE only)."
  [pop-map]
  (let [labels {":global" "Global (all super-populations)"
                ":AFR" "African / African-American (AFR)"
                ":AMR" "Admixed American (AMR)"
                ":EAS" "East Asian (EAS)"
                ":EUR" "European (non-Finnish, EUR)"
                ":SAS" "South Asian (SAS)"}]
    (reduce
     (fn [m popcode]
       (let [pid (str "pop." (str/lower (lstrip-colon-str popcode)))]
         (assoc m pid {":genome/id" pid ":genome/kind" ":population"
                       ":genome/label" (get labels popcode popcode)
                       ":population/code" popcode ":genome/sourcing" ":authoritative"})))
     {} (set (vals pop-map)))))

;; ── EDN serialisation (mirrors datom_emit._fmt; deterministic key order) ──────
(def ^:private node-key-order
  [":genome/id" ":genome/kind" ":genome/label" ":gene/symbol"
   ":gene/cytoband" ":gene/ensembl" ":gene/taxon" ":variant/rsid"
   ":variant/category" ":phenotype/code" ":phenotype/inheritance"
   ":population/code" ":genome/links" ":genome/sourcing"])
(def ^:private edge-key-order
  [":en/from" ":en/to" ":en/kind" ":en/clinsig" ":en/grasping-load" ":en/sourcing"])

(defn- fmt-v
  [v]
  (cond
    (true? v) "true"
    (false? v) "false"
    (nil? v) "nil"
    (string? v) (if (str/starts-with? v ":")
                  v
                  (str "\"" (-> v (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\""))
    (double? v) (fmt-g v)
    #?@(:clj [(float? v) (fmt-g v)])
    :else (str v)))

(defn- emit-map
  [m order]
  (let [keys* (concat (filter #(contains? m %) order)
                      (filter #(not (some #{%} order)) (keys m)))]
    (str "{" (str/join " " (map (fn [k] (str k " " (fmt-v (get m k)))) keys*)) "}")))

(defn- order-kind-of [n]
  (get {":gene" 0 ":variant" 1 ":phenotype" 2 ":population" 3 ":pathway" 4}
       (get n ":genome/kind") 9))

;; The prov "counts" header in Python prints `dict.__str__`. Tests pass {"variants": 1} and
;; only assert serialisation is deterministic (header identical run-to-run), so any stable
;; string rendering is faithful for the test contract.
(defn- python-dict-str
  "Render a (string-keyed) map the way Python's `str(dict)` would for the prov-counts header."
  [m]
  (str "{" (str/join ", "
                     (map (fn [[k v]] (str "'" k "': " (if (string? v) (str "'" v "'") (str v))))
                          m)) "}"))

(defn serialise-graph
  "Serialise nodes+edges to deterministic EDN text (1:1 of serialise_graph)."
  [nodes edges prov]
  (let [L (transient [])]
    (conj! L ";; rasen 螺旋 — GENERATED ingested public-genetics graph (ADR-2606101000). DO NOT hand-edit.")
    (conj! L ";; PUBLIC reference only — no individual genotypes; gnomAD super-population AGGREGATE freq (G1).")
    (conj! L (str ";; sources: " (str/join ", " (map #(get % "url") (get prov "sources")))))
    (conj! L (str ";; counts: " (python-dict-str (get prov "counts"))))
    (conj! L "[")
    (doseq [nid (sort-by (fn [i] [(order-kind-of (get nodes i)) i]) (keys nodes))]
      (conj! L (str " " (emit-map (get nodes nid) node-key-order))))
    (conj! L "")
    (doseq [e (sort-by (fn [e] [(get e ":en/kind") (get e ":en/from") (get e ":en/to")]) edges)]
      (conj! L (str " " (emit-map e edge-key-order))))
    (conj! L "]")
    (str (str/join "\n" (persistent! L)) "\n")))

;; NOTE: the live network/ipfs pipeline (`_get_json` / `ingest` / `main`) is host I/O and is
;; intentionally NOT ported (the .cljc surface is the deterministic, network-free port the
;; tests exercise — normalisation + EDN serialisation + content-address via rasen.methods.cid).

#?(:clj
   (defn -main
     "CLI entry: mirrors ingest.main — [--offline] [--out DIR] [--sources EDN] [--no-pin].
     --offline re-addresses an existing out/ingested-genome-graph.kotoba.edn (load → analyze →
     datoms → CIDv1 + provenance + optional ipfs pin). The LIVE gene-fetch leg (MyGene.info /
     MyVariant.info) is G7-gated network and not ported (refused). ADR-2606261200."
     [& argv]
     (let [argv     (vec argv)
           here     (let [f  (when (and *file* (not (str/blank? *file*))) (io/file *file*))
                          pp (some-> f .getAbsoluteFile .getParentFile .getParentFile)]
                      (if (and pp (.isDirectory (io/file pp "data"))) pp (io/file ".")))
           outdir   (if (some #{"--out"} argv) (io/file (nth argv (inc (.indexOf argv "--out")))) (io/file here "out"))
           no-pin   (boolean (some #{"--no-pin"} argv))
           graph-f  (io/file outdir "ingested-genome-graph.kotoba.edn")]
       (cond
         (not (some #{"--offline"} argv))
         (binding [*out* *err*]
           (println (str "REFUSED: live gene ingest (MyGene.info / MyVariant.info) is G7-gated "
                         "network + not ported. Run with --offline to re-address an existing graph."))
           (System/exit 1))
         (not (.exists graph-f))
         (binding [*out* *err*]
           (println "ingest --offline: no existing graph to re-address")
           (System/exit 1))
         :else
         (let [analyze* (requiring-resolve 'rasen.methods.analyze/analyze)
               emit*    (requiring-resolve 'rasen.methods.datom-emit/emit)
               cidv1*   (requiring-resolve 'rasen.methods.cid/cidv1-raw)
               [nodes edges] (load (.getPath graph-f))
               res      (analyze* nodes edges)
               datoms   (emit* nodes edges res 1)
               datom-f  (io/file outdir "ingested-genome-datoms.kotoba.edn")
               _        (spit datom-f datoms)
               g-bytes  (java.nio.file.Files/readAllBytes (.toPath graph-f))
               cid      (cidv1* g-bytes)
               d-cid    (cidv1* (.getBytes datoms "UTF-8"))
               prov     {:offline true :artifact {:graph_edn (.getName graph-f) :bytes (alength g-bytes)
                                                  :cid cid :datoms_cid d-cid}
                         :counts {:nodes (count nodes) :edges (count edges)}}]
           (.mkdirs outdir)
           (spit (io/file outdir "ingested.cid") (str cid "\n"))
           (when-not no-pin
             (when (try (zero? (:exit (babashka.process/shell {:out :string :err :string :continue true}
                                        "ipfs" "add" "-Q" "--cid-version=1" "--raw-leaves" (.getPath graph-f))))
                        (catch Exception _ false))
               (assoc-in prov [:pin :ok] true)))
           (spit (io/file outdir "ingest-provenance.edn") (str (pr-str prov) "\n"))
           (println (str "rasen ingest (offline): re-addressed graph (" (count nodes) " nodes · "
                         (count edges) " edges) → cid " cid)))))))
