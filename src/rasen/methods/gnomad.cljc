(ns rasen.methods.gnomad
  "gnomad.cljc — rasen 螺旋 gnomAD population-frequency extraction (ADR-2609062000).

  gnomAD sites VCFs are the aggregate allele-frequency source the genome-ontology already
  names. This namespace takes VCF records and emits ONLY population nodes and
  :allele-frequency 縁 — never the variant's sequence context.

  TWO G1 CONSTRAINTS THAT ARE NOT TOOLING LIMITS.

  (1) A VCF record identifies a variant by CHROM/POS/REF/ALT — precise, re-identifiable
      coordinates, which the ontology forbids. The only G1-safe identity a record carries is
      its dbSNP rsID, and gnomAD leaves that empty for a large share of records. Those records
      are therefore NOT representable here, and `extract` skips them and COUNTS the skips. This
      caps coverage structurally: measured on a real gnomAD v4.1 chr21 genomes prefix, 24,189
      of 41,171 records (58.8%) carry an rsID. A claim of full gnomAD coverage would be false
      by construction, and the counts are returned so it cannot be made by accident.

  (2) gnomAD publishes AF_XX and AF_XY — sex-stratified frequencies — alongside the
      super-population ones. The ontology's :population/code admits super-populations only, so
      those are excluded by an explicit ALLOW-LIST rather than by a prefix match. An extractor
      that matched `AF_` would sweep them in, and the resulting edges would look exactly like
      legitimate ones.

  Frequencies are copied, never derived: :en/grasping-load on an :allele-frequency 縁 IS the
  aggregate frequency (N1/N3)."
  (:require [clojure.string :as str]
            [rasen.methods.clinvar :as cv]))

(def af-population
  "ALLOW-LIST: gnomAD INFO key → genome-ontology :population/code. Deliberately explicit.
  Same mapping as `data/ingest-sources.edn` uses for the bounded MyVariant slice, so the two
  producers put the same populations on the same edges.
  Everything absent here — AF_XX / AF_XY (sex-stratified), AF_fin / AF_asj / AF_ami / AF_mid /
  AF_remaining (finer than a super-population), and every *_XX / *_XY variant of them — has no
  target in the ontology and is not emitted."
  {"AF" ":global" "AF_afr" ":AFR" "AF_amr" ":AMR"
   "AF_eas" ":EAS" "AF_nfe" ":EUR" "AF_sas" ":SAS"})

(def excluded-af-keys
  "Keys a prefix match would wrongly include. Named so the exclusion is testable rather than
  implied by the allow-list's absence."
  #{"AF_XX" "AF_XY" "AF_afr_XX" "AF_afr_XY" "AF_amr_XX" "AF_amr_XY"
    "AF_eas_XX" "AF_eas_XY" "AF_nfe_XX" "AF_nfe_XY" "AF_sas_XX" "AF_sas_XY"
    "AF_fin" "AF_asj" "AF_ami" "AF_mid" "AF_remaining"})

(defn parse-info
  "VCF INFO column → {key value}. Flag entries (no '=') map to true."
  [info]
  (reduce (fn [m entry]
            (let [i (str/index-of entry "=")]
              (if i (assoc m (subs entry 0 i) (subs entry (inc i))) (assoc m entry true))))
          {} (remove str/blank? (str/split (or info "") #";"))))

(defn parse-af
  "An AF cell → a double in [0,1], or nil. gnomAD writes multi-allelic values comma-separated;
  the sites files are split so the first value is this record's. Refuses a value outside [0,1]
  rather than passing it through: an allele frequency that is not a frequency would become a
  :en/grasping-load that the care/burden integrals then read as a weight."
  [cell]
  (when (string? cell)
    (let [head (first (str/split cell #","))]
      (try
        (let [d #?(:clj (Double/parseDouble head) :cljs (js/parseFloat head))]
          (when (and (not #?(:clj (Double/isNaN d) :cljs (js/isNaN d)))
                     (<= 0.0 d 1.0))
            d))
        (catch #?(:clj Exception :cljs :default) _ nil)))))

(defn rsid-of
  "The G1-safe identity of a VCF record: its dbSNP rsID, or nil. gnomAD writes '.' for absent,
  and may carry several ids separated by ';' — the first rs-prefixed one is taken."
  [id-cell]
  (when (and (string? id-cell) (not= "." (str/trim id-cell)))
    (some #(when (str/starts-with? % "rs") %)
          (map str/trim (str/split id-cell #";")))))

(defn population-node [popcode]
  (let [labels {":global" "Global (all super-populations)"
                ":AFR" "African / African-American (AFR)"
                ":AMR" "Admixed American (AMR)"
                ":EAS" "East Asian (EAS)"
                ":EUR" "European (non-Finnish, EUR)"
                ":SAS" "South Asian (SAS)"}
        pid (str "pop." (str/lower-case (subs popcode 1)))]
    [pid {":genome/id" pid ":genome/kind" ":population" ":genome/label" (get labels popcode popcode)
          ":population/code" popcode ":genome/sourcing" ":authoritative"}]))

(defn record->graph
  "A tab-split gnomAD sites record → {:nodes {} :edges []}, or nil when the record carries no
  rsID (not representable under G1 — see the namespace docstring).

  Emits the variant node with its rsID only. No CHROM, POS, REF or ALT reaches the graph, and
  the result is passed through the same `assert-g1!` the ClinVar producer uses."
  [fields]
  (let [id-cell (nth fields 2 nil)
        rsid (rsid-of id-cell)]
    (when rsid
      (let [info (parse-info (nth fields 7 nil))
            vid (str "var." rsid)
            vnode {":genome/id" vid ":genome/kind" ":variant" ":genome/label" rsid
                   ":variant/rsid" rsid ":genome/sourcing" ":authoritative"}
            hits (keep (fn [[k popcode]]
                         (when-let [af (parse-af (get info k))]
                           [popcode af]))
                       af-population)
            [pnodes edges]
            (reduce (fn [[ns es] [popcode af]]
                      (let [[pid pnode] (population-node popcode)]
                        [(assoc ns pid pnode)
                         (conj es {":en/from" pid ":en/to" vid ":en/kind" ":allele-frequency"
                                   ":en/grasping-load" af ":en/sourcing" ":authoritative"})]))
                    [{} []] hits)
            nodes (assoc pnodes vid vnode)]
        (run! cv/assert-g1! (vals nodes))
        {:nodes nodes :edges edges}))))

(def empty-acc {:nodes {} :edges [] :records 0 :with-rsid 0 :without-rsid 0 :edges-emitted 0})

(defn extract
  "Fold gnomAD records into a graph, reporting coverage honestly.

  :records is every data record seen; :with-rsid and :without-rsid partition it. A caller that
  wants to state gnomAD coverage must use these — the node count alone cannot distinguish
  'gnomAD has few variants here' from 'most records were not representable'."
  [records]
  (reduce (fn [acc fields]
            (let [acc (update acc :records inc)
                  g (record->graph fields)]
              (if (nil? g)
                (update acc :without-rsid inc)
                (-> acc
                    (update :with-rsid inc)
                    (update :nodes (fn [m] (reduce (fn [m [k v]] (update m k merge v)) m (:nodes g))))
                    (update :edges into (:edges g))
                    (update :edges-emitted + (count (:edges g)))))))
          empty-acc records))

(defn data-line? [line] (and (string? line) (not (str/starts-with? line "#")) (seq (str/trim line))))

(defn coverage
  "The share of records that were representable, or nil when nothing was read.
  nil rather than 1.0 or 0.0: an extraction that saw no records has no coverage to report, and
  either number would be read as a measurement."
  [{:keys [records with-rsid]}]
  (when (pos? records) (double (/ with-rsid records))))
