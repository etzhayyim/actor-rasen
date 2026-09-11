(ns rasen.methods.coverage-report
  "rasen 螺旋 — public-genetics COVERAGE report (ADR-2606101000). 1:1 Clojure port of
  `methods/coverage_report.py`.

  Honest coverage measurement of the genome graph: how much of the target space the seed
  covers — by external denominator (genes, ClinVar variants, dbSNP), by inheritance mode, by
  clinical-significance spread, by taxon, by population-aggregate, and by pathway — and a gap
  map naming what is thin/missing. NOT a completeness claim: coverage of *all* genes/variants
  is ~0 by design (a bounded :representative seed).

  G1 — CARE / RESEARCH map, NEVER an individual-genotype registry: aggregate af_* only, coarse
  cytoband never precise coords. PUBLIC reference data only — no individual genotypes surfaced.

  Pure fns; reuses rasen.methods.analyze for the loader. Portable .cljc."
  (:require [kotoba.lang.text :as str]
            [rasen.methods.analyze :as analyze]))

;; honest external denominators
(def gene-denom
  [["Human protein-coding genes (~)" 20000]
   ["HGNC named genes (~)" 43000]])
(def variant-denom
  [["ClinVar submitted variants (~)" 3500000]
   ["ClinVar P/LP variants (~)" 800000]
   ["dbSNP variants (~)" 1100000000]])

(def inheritance [":AD" ":AR" ":XL" ":mitochondrial" ":complex" ":somatic" ":trait"])
(def clinsig [":pathogenic" ":likely-pathogenic" ":risk-factor" ":drug-response"
              ":uncertain" ":likely-benign" ":benign" ":protective"])
(def taxa [":homo-sapiens" ":loxodonta" ":oryza-sativa" ":bacteria"])
(def pops [":global" ":AFR" ":AMR" ":EAS" ":EUR" ":SAS"])
(def pathway-src [":GO" ":reactome" ":kegg"])
(def THIN 2) ;; a bucket with < THIN members is flagged thin

(defn- lstrip-colon [s] (if (and (string? s) (str/starts-with? s ":")) (subs s 1) s))

(defn- counter
  "Counter(seq) → map value->count, mirroring collections.Counter (nil keys allowed)."
  [coll]
  (reduce (fn [m v] (update m v (fnil inc 0))) {} coll))

(defn- comma
  "Python f'{n:,}' — group integer digits with commas (no fraction here)."
  [n]
  (let [s (str (long n))
        neg (str/starts-with? s "-")
        digits (if neg (subs s 1) s)
        rev (reverse (vec digits))
        grouped (->> rev
                     (partition-all 3)
                     (map #(apply str (reverse %)))
                     reverse
                     (str/join ","))]
    (str (when neg "-") grouped)))

(defn- sci2
  "Python f'{x:.2e}' — scientific notation, 2 fraction digits, lowercase e, signed 2+ exp.
  Java %.2e gives e.g. \"8.00e-04\" already matching Python's two-digit signed exponent."
  [x]
  ;; clj %.2e == cljs Number.toExponential(2) (both: mantissa.2digits 'e' signed-2+digit exp).
  #?(:clj  (format "%.2e" (double x))
     :cljs (.toExponential (js/Number x) 2)))

(defn report
  "Render the public-genetics coverage-report markdown (1:1 with coverage_report.report)."
  [nodes edges]
  (let [vals* (vals nodes)
        genes (filter #(= ":gene" (get % ":genome/kind")) vals*)
        variants (filter #(= ":variant" (get % ":genome/kind")) vals*)
        phenos (filter #(= ":phenotype" (get % ":genome/kind")) vals*)
        pops* (filter #(= ":population" (get % ":genome/kind")) vals*)
        pathways (filter #(= ":pathway" (get % ":genome/kind")) vals*)
        inh-c (counter (map #(get % ":phenotype/inheritance") phenos))
        taxon-c (counter (map #(get % ":gene/taxon") genes))
        pop-c (counter (map #(get % ":population/code") pops*))
        pw-c (counter (map #(get % ":pathway/source") pathways))
        clinsig-c (counter (keep #(get % ":en/clinsig") edges))
        n-gene (count genes)
        n-var (count variants)
        L (transient [])]
    (conj! L "# rasen 螺旋 — public-genetics coverage report\n")
    (conj! L (str "> Honest denominator: coverage of all genes/variants is ~0 by design (bounded "
                  "seed). This names the clinically-actionable backbone covered and the next-wave "
                  "gaps. PUBLIC reference data only — no individual genotypes (G1).\n"))
    (conj! L (str "**Seed**: " n-gene " genes · " n-var " variants · " (count phenos) " "
                  "phenotypes · " (count pops*) " populations · " (count pathways) " pathways · "
                  (count edges) " 縁\n"))

    (conj! L "\n## Gene coverage vs denominators\n")
    (conj! L "| denominator | count | seed | fraction |")
    (conj! L "|---|---:|---:|---:|")
    (doseq [[name denom] gene-denom]
      (conj! L (str "| " name " | " (comma denom) " | " n-gene " | "
                    (sci2 (/ (double n-gene) denom)) " |")))

    (conj! L "\n## Variant coverage vs denominators\n")
    (conj! L "| denominator | count | seed | fraction |")
    (conj! L "|---|---:|---:|---:|")
    (doseq [[name denom] variant-denom]
      (conj! L (str "| " name " | " (comma denom) " | " n-var " | "
                    (sci2 (/ (double n-var) denom)) " |")))

    (conj! L "\n## Clinical-significance spread (DISCLOSED facts, not verdicts)\n")
    (conj! L "| category | edges |")
    (conj! L "|:--:|---:|")
    (doseq [cat clinsig]
      (conj! L (str "| " (lstrip-colon cat) " | " (get clinsig-c cat 0) " |")))

    (letfn [(bucket [title ks cnt]
              (conj! L (str "\n## " title "\n"))
              (conj! L "| bucket | count | status |")
              (conj! L "|---|---:|:--|")
              (doseq [k ks]
                (let [c (get cnt k 0)
                      status (cond (= c 0) "— **MISSING**"
                                   (< c THIN) "⚠ thin"
                                   :else "ok")]
                  (conj! L (str "| " (lstrip-colon k) " | " c " | " status " |")))))]
      (bucket "Inheritance-mode coverage" inheritance inh-c)
      (bucket "Taxon coverage (life is broader than humans)" taxa taxon-c)
      (bucket "Population-aggregate coverage" pops pop-c)
      (bucket "Pathway-source coverage" pathway-src pw-c))

    (let [missing (concat
                   (for [b inheritance :when (= 0 (get inh-c b 0))] (lstrip-colon b))
                   (for [t taxa :when (= 0 (get taxon-c t 0))] (lstrip-colon t))
                   (for [p pops :when (= 0 (get pop-c p 0))] (lstrip-colon p))
                   (for [s pathway-src :when (= 0 (get pw-c s 0))] (lstrip-colon s)))]
      (conj! L "\n## Gap map — next-wave targets\n")
      (if (seq missing)
        (conj! L (str "Missing buckets: " (str/join ", " missing) "."))
        (conj! L "No fully-missing buckets in the tracked spines (thin buckets still listed above).")))
    (conj! L "\n---\n_rasen 螺旋 · ADR-2606101000 · coverage honesty (G5)._\n")
    (str/join "\n" (persistent! L))))

#?(:clj
   (defn -main
     "CLI entry: render coverage-report.md from a seed EDN graph."
     [& argv]
     (let [argv (vec argv)
           here (clojure.java.io/file (or (System/getenv "RASEN_ACTOR_DIR") "."))
           seed (if (and (seq argv) (not (str/starts-with? (first argv) "--")))
                  (clojure.java.io/file (first argv))
                  (clojure.java.io/file here "data" "seed-genome-graph.kotoba.edn"))
           outdir (if (some #{"--out"} argv)
                    (clojure.java.io/file (nth argv (inc (.indexOf argv "--out"))))
                    (clojure.java.io/file here "out"))
           {:keys [nodes edges]} (analyze/load-file* seed)]
       (.mkdirs outdir)
       (spit (clojure.java.io/file outdir "coverage-report.md") (report nodes edges))
       (println (str "rasen coverage → " (clojure.java.io/file outdir "coverage-report.md")))
       0)))
