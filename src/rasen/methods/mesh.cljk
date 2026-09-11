;; mesh.clj — rasen 螺旋 KOTOBA Mesh entry component (Clojure / kotoba-clj).
;;
;; The mesh-hosting face of actor:rasen (public-genetics KG mirror). Observatory
;; on-kse pattern (ADR-2606230001 §4): observes variant→gene / gene→phenotype
;; association edges as Datom assertions and derives clinical-evidence concentration
;; via Datalog, routed to CARE & RESEARCH. The full ClinVar/GO/Reactome ingest
;; stays in the actor's existing methods.
;;
;; Posture: G1 CARE/RESEARCH map, NEVER an individual-genotype registry — population
;; AGGREGATE only (no individual/sample/family genotype, coarse cytoband only);
;; non-adjudicating (disclosed ClinVar/GO significance, never re-judged).
;; host-imports: kqe-assert! / kqe-query → kotoba:kais/kqe (needs cap/kqe)
(ns rasen)

(defn observe []
  ;; observe — public reference associations (variant/gene → phenotype), aggregate.
  (kqe-assert! "rasen" "variant-a" "associates" "phenotype-x")
  (kqe-assert! "rasen" "gene-b" "associates" "phenotype-x")
  (kqe-assert! "rasen" "gene-c" "associates" "pathway-y")
  ;; derive — evidence concentration → care/research priority (Datalog).
  (kqe-query "care-research(?p) :- associates(?p)."))

(defn run [ctx] (observe))
(defn on-kse [topic payload] (observe))
