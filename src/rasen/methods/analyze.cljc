(ns rasen.methods.analyze
  "rasen 螺旋 — edge-primary clinical/functional evidence analyzer over the public-genetics graph.
  1:1 Clojure port of `methods/analyze.py` (ADR-2606101000).

  Reads a kotoba-EDN genome graph (:genome/* nodes + :en/* 縁 over the genome-ontology) and
  surfaces — aggregate-first — where integrated CLINICAL / FUNCTIONAL evidence burden
  accumulates over a GENE, routed to CARE & RESEARCH, and where pleiotropy makes that burden
  cascade across phenotypes / pathways.

  CONSTITUTIONAL (read before any change):
    N1 / G2 — edge-primary. evidence/burden lives ONLY on edges (:en/grasping-load weighted by
      DISCLOSED :en/clinsig). A gene's care-priority is the INTEGRAL of its incident
      variant-association + gene-linkage 縁 — computed on READ, never a stored per-gene score.
      There is no :genome/score-of-gene.
    G1 — CARE / RESEARCH map, never an individual-genotype registry or discrimination tool. No
      individual genotypes are read or emitted; population readouts are super-population
      AGGREGATE only; chromosomal location is COARSE (cytoband).
    N3 — non-adjudicating. Clinical-significance categories are DISCLOSED curated facts, never
      rasen verdicts.

  House style: Python ':…' keyword strings stay strings (incl. all :genome/* / :en/* attrs);
  pure fns; file I/O only at edges via clojure.java.io. Portable .cljc."
  (:require [clojure.string :as str]))

;; ── minimal EDN reader (subset: vectors [], maps {}, :keyword, \"string\", num, bool, nil)
;; Mirrors analyze.py's _TOK / _tokens / _atom / _parse faithfully. Keywords are kept as
;; \":ns/name\" strings (NOT clojure keywords) so the whole pipeline stays string-keyed,
;; byte-for-byte the same as the Python port.

(def ^:private tok-re
  ;; _TOK = re.compile(r'[\s,]+|;[^\n]*|(\[|\]|\{|\}|"(?:\\.|[^"\\])*"|[^\s,\[\]{}]+)')
  #"[\s,]+|;[^\n]*|(\[|\]|\{|\}|\"(?:\\.|[^\"\\])*\"|[^\s,\[\]{}]+)")

(defn tokens
  "Significant tokens (group 1 of each tok-re match that captured). Portable: clj uses a JVM
  Matcher; cljs (cherry/ComponentizeJS wasm, ADR-2606261200) uses a native global RegExp .exec
  loop over the same pattern (tok-re is JVM/JS-compatible regex syntax)."
  [s]
  #?(:clj
     (let [m (re-matcher tok-re s)]
       ((fn step []
          (lazy-seq
           (when (.find m)
             (let [t (.group m 1)]
               (if (nil? t) (step) (cons t (step)))))))))
     :cljs
     (let [re  (js/RegExp. (.-source tok-re) "g")
           out #js []]
       (loop [mm (.exec re s)]
         (when mm
           (let [t (aget mm 1)] (when (some? t) (.push out t)))
           (recur (.exec re s))))
       out)))

(defn atom-of
  "Port of _atom: \"…\" → unescaped string; true/false/nil → bool/nil; \":…\" kept as string;
  int → long; else float; else raw string."
  [t]
  (cond
    (str/starts-with? t "\"")
    (-> (subs t 1 (dec (count t)))
        (str/replace "\\\"" "\"")
        (str/replace "\\\\" "\\"))
    (= t "true") true
    (= t "false") false
    (= t "nil") nil
    (str/starts-with? t ":") t
    :else
    (let [as-long (try (Long/parseLong t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
      (if (not= as-long ::nan)
        as-long
        (let [as-dbl (try (Double/parseDouble t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
          (if (not= as-dbl ::nan) as-dbl t))))))

(def ^:private end-marker ::end)

(defn- parse-step
  "Consume one form from the token vector at index i. Returns [value next-i] or
  [end-marker next-i] when a closing ] or } is hit (matching _parse's _END sentinel)."
  [toks i]
  (let [t (nth toks i)
        i (inc i)]
    (cond
      (= t "[")
      (loop [i i, out []]
        (let [[x i] (parse-step toks i)]
          (if (= x end-marker)
            [out i]
            (recur i (conj out x)))))

      (= t "{")
      (loop [i i, out {}]
        (let [[k i] (parse-step toks i)]
          (if (= k end-marker)
            [out i]
            (let [[v i] (parse-step toks i)]
              (recur i (assoc out k v))))))

      (or (= t "]") (= t "}"))
      [end-marker i]

      :else
      [(atom-of t) i])))

(defn read-edn
  "Parse the first top-level form from EDN text (matches read_edn → _parse(_tokens(text)))."
  [text]
  (let [toks (vec (tokens text))]
    (first (parse-step toks 0))))

;; ── disclosed clinical-significance → representative evidence weight (NOT a verdict)
(def clinsig-weight
  {":pathogenic" 1.0 ":likely-pathogenic" 0.8 ":risk-factor" 0.5
   ":drug-response" 0.4 ":uncertain" 0.3 ":vus" 0.3
   ":likely-benign" 0.1 ":benign" 0.05 ":protective" 0.1})

(def assoc-kinds #{":associated-with"})       ;; variant → phenotype (attributed to its gene)
(def link-kinds #{":linked-to"})              ;; gene → phenotype (Mendelian, src IS the gene)
(def pathway-kinds #{":participates-in" ":interacts-with"})
(def located-kinds #{":located-in"})          ;; variant → gene (provenance)
(def freq-kinds #{":allele-frequency"})       ;; population → variant (AGGREGATE only)

(defn load-graph
  "Return {:nodes nodes-by-id :edges edges} from a parsed list of EDN forms.
  (`load` is a clojure.core fn — named load-graph; the host edge reads the file.)"
  [forms]
  (reduce
   (fn [{:keys [nodes edges] :as acc} f]
     (cond
       (not (map? f)) acc
       (contains? f ":genome/id") (assoc-in acc [:nodes (get f ":genome/id")] f)
       (and (contains? f ":en/from") (contains? f ":en/to"))
       (update acc :edges conj f)
       :else acc))
   {:nodes {} :edges []}
   forms))

#?(:clj
   (defn load-file*
     "Read + parse a genome EDN graph file → {:nodes :edges}. File I/O only at this edge."
     [path]
     (load-graph (read-edn (slurp (str path))))))

(defn- ->load
  "float(e.get(':en/grasping-load', 0.0) or 0.0) — coerce to double, 0.0 on nil/false/missing."
  [e]
  (let [v (get e ":en/grasping-load")]
    (if (or (nil? v) (false? v)) 0.0 (double v))))

(defn analyze
  "Edge-primary integrals (computed on read; transient — N1/G2). Returns
   {\"care\" {gene v} \"burden\" {variant v} \"pleiotropy\" {node v}
    \"pop_freq\" {variant [[pop af] …]}}.

   care[gene]  = Σ (variant→phenotype association load × clinsig weight), attributed through
                 the variant's :located-in gene, PLUS Σ (gene→phenotype :linked-to load × wt).
   burden[var] = Σ outbound :associated-with load × clinsig weight (the 取-holding locus).
   pleiotropy  = Σ incident :associated-with / :linked-to / pathway loads (cascade breadth).
   pop_freq    = per variant, (population, AGGREGATE allele frequency) sorted by -freq."
  [nodes edges]
  ;; variant → gene provenance (from :located-in)
  (let [variant-gene (reduce (fn [m e]
                               (if (contains? located-kinds (get e ":en/kind"))
                                 (assoc m (get e ":en/from") (get e ":en/to"))
                                 m))
                             {} edges)]
    (loop [es edges
           care {} burden {} pleiotropy {} pop-freq {}]
      (if (empty? es)
        {"care" care
         "burden" burden
         "pleiotropy" pleiotropy
         "pop_freq" (into {} (map (fn [[k v]]
                                    [k (vec (sort-by (fn [pf] (- (second pf))) v))])
                                  pop-freq))}
        (let [e (first es)
              kind (get e ":en/kind")
              load- (->load e)
              src (get e ":en/from")
              dst (get e ":en/to")]
          (cond
            (contains? assoc-kinds kind)
            (let [w (get clinsig-weight (get e ":en/clinsig") 0.3)
                  gene (get variant-gene src)]
              (recur (rest es)
                     (if gene (update care gene (fnil + 0.0) (* load- w)) care)
                     (update burden src (fnil + 0.0) (* load- w))
                     (-> pleiotropy
                         (update src (fnil + 0.0) load-)
                         (update dst (fnil + 0.0) load-))
                     pop-freq))

            (contains? link-kinds kind)
            (let [w (get clinsig-weight (get e ":en/clinsig") 0.3)]
              (recur (rest es)
                     (update care src (fnil + 0.0) (* load- w))
                     burden
                     (-> pleiotropy
                         (update src (fnil + 0.0) load-)
                         (update dst (fnil + 0.0) load-))
                     pop-freq))

            (contains? pathway-kinds kind)
            (recur (rest es)
                   care burden
                   (-> pleiotropy
                       (update src (fnil + 0.0) load-)
                       (update dst (fnil + 0.0) load-))
                   pop-freq)

            (contains? freq-kinds kind)
            (recur (rest es)
                   care burden pleiotropy
                   (update pop-freq dst (fnil conj []) [src load-]))

            :else
            (recur (rest es) care burden pleiotropy pop-freq)))))))

(defn rank
  "Top-`limit` (id, label, value) rows of d, sorted by (-value, id) — matches _rank."
  ([d nodes] (rank d nodes 20))
  ([d nodes limit]
   (->> (sort-by (fn [[k v]] [(- v) k]) d)
        (take limit)
        (mapv (fn [[nid v]]
                [nid (get-in nodes [nid ":genome/label"] nid) v])))))

;; ── report rendering (matches report_md's f-strings) ────────────────────────

(defn- fmt3 [v] (format "%.3f" (double v)))
(defn- fmt4 [v] (format "%.4f" (double v)))

(defn- lstrip-colon [s] (if (and (string? s) (str/starts-with? s ":")) (subs s 1) s))

(defn- count-kind [nodes k]
  (count (filter #(= k (get % ":genome/kind")) (vals nodes))))

(defn report-md
  "Render the public-genetics care/research-priority report markdown (1:1 with report_md)."
  [nodes edges res]
  (let [n-gene (count-kind nodes ":gene")
        n-var (count-kind nodes ":variant")
        n-ph (count-kind nodes ":phenotype")
        n-pop (count-kind nodes ":population")
        n-pw (count-kind nodes ":pathway")
        auth (count (filter #(= ":authoritative" (get % ":genome/sourcing")) (vals nodes)))
        L (transient [])]
    (conj! L "# rasen 螺旋 — public-genetics care/research-priority report (aggregate-first)\n")
    (conj! L (str "> **G1 — CARE / RESEARCH map, NEVER an individual-genotype registry or "
                  "discrimination tool.** No individual genotypes; population readouts are "
                  "super-population AGGREGATE only; chromosomal location is coarse (cytoband). "
                  "Clinical-significance categories are DISCLOSED, not rasen verdicts (N3). "
                  "Evidence lives only on edges, integrated on read (N1).\n"))
    (conj! L (str "**Graph**: " (count nodes) " nodes (" n-gene " genes · " n-var " variants · "
                  n-ph " phenotypes · " n-pop " populations · " n-pw " pathways) · "
                  (count edges) " 縁 · " auth "/" (count nodes) " :authoritative\n"))

    (conj! L "\n## Gene care-priority — genes bearing the most disclosed clinical/functional burden\n")
    (conj! L (str "_Σ incident variant-association + gene-linkage load × disclosed clinsig weight; "
                  "routed to care & research, never to ranking persons._\n"))
    (conj! L "| rank | gene | symbol | taxon | care-priority |")
    (conj! L "|---:|---|---|---|---:|")
    (doseq [[i [nid label v]] (map-indexed vector (rank (get res "care") nodes))]
      (let [n (get nodes nid {})
            sym (or (get n ":gene/symbol") "—")
            taxon (or (get n ":gene/taxon") "—")]
        (conj! L (str "| " (inc i) " | " label " | " sym " | "
                      (lstrip-colon (str taxon)) " | " (fmt3 v) " |"))))

    (conj! L "\n## Locus burden — variants/loci carrying the most pathogenic-association weight\n")
    (conj! L (str "_Σ outbound association load × disclosed clinsig; the burden-bearing locus "
                  "(the 取-holder). DISCLOSED facts only (N3)._\n"))
    (conj! L "| rank | variant | rsID | burden |")
    (conj! L "|---:|---|---|---:|")
    (doseq [[i [nid label v]] (map-indexed vector (rank (get res "burden") nodes))]
      (let [rsid (or (get-in nodes [nid ":variant/rsid"]) "—")]
        (conj! L (str "| " (inc i) " | " label " | " rsid " | " (fmt3 v) " |"))))

    (conj! L "\n## Pleiotropy / cascade — nodes touching the most phenotypes / pathways\n")
    (conj! L "| rank | node | kind | pleiotropy |")
    (conj! L "|---:|---|---|---:|")
    (doseq [[i [nid label v]] (map-indexed vector (rank (get res "pleiotropy") nodes 12))]
      (let [kind (or (get-in nodes [nid ":genome/kind"]) "—")]
        (conj! L (str "| " (inc i) " | " label " | " (lstrip-colon (str kind)) " | " (fmt3 v) " |"))))

    (conj! L "\n## Population allele frequency — AGGREGATE disclosure (G1: never individual)\n")
    (conj! L (str "_Super-population (gnomAD-scale) frequencies only. These describe populations, "
                  "never persons, and can never reconstruct an individual genotype._\n"))
    (conj! L "| variant | population | aggregate allele freq |")
    (conj! L "|---|:--:|---:|")
    (doseq [vid (sort (keys (get res "pop_freq")))]
      (let [vlabel (get-in nodes [vid ":genome/label"] vid)]
        (doseq [[pop af] (get-in res ["pop_freq" vid])]
          (let [pcode (or (get-in nodes [pop ":population/code"]) pop)]
            (conj! L (str "| " vlabel " | " (lstrip-colon (str pcode)) " | " (fmt4 af) " |"))))))

    (conj! L (str "\n---\n_rasen 螺旋 · ADR-2606101000 · mirror-only · non-adjudicating · "
                  "edge-primary · care/research-routed. Live ingest (ClinVar/gnomAD/Ensembl/"
                  "GWAS-Catalog) is G7/Council-gated._\n"))
    (str/join "\n" (persistent! L))))

#?(:clj
   (defn -main
     "CLI entry: analyze a seed EDN graph → out/care-report.md (file I/O at the edge)."
     [& argv]
     (let [argv (vec argv)
           here (clojure.java.io/file (or (System/getenv "RASEN_ACTOR_DIR") "."))
           seed (if (and (seq argv) (not (str/starts-with? (first argv) "--")))
                  (clojure.java.io/file (first argv))
                  (clojure.java.io/file here "data" "seed-genome-graph.kotoba.edn"))
           outdir (clojure.java.io/file here "out")
           {:keys [nodes edges]} (load-file* seed)
           res (analyze nodes edges)]
       (.mkdirs outdir)
       (spit (clojure.java.io/file outdir "care-report.md") (report-md nodes edges res))
       (println (str "rasen: " (count nodes) " nodes, " (count edges) " 縁 → "
                     (clojure.java.io/file outdir "care-report.md")))
       (when-let [top (first (rank (get res "care") nodes 1))]
         (println (str "  top care-priority gene: " (nth top 1)
                       " (" (fmt3 (nth top 2)) ")")))
       0)))
