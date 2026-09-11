#!/usr/bin/env bb
;; rasen 螺旋 — validation of the MyGene.info → gene-node ingest mapping.
;; Run: bb test
(ns rasen.methods.test-gene-node
  "Validation of gene-node-from-mygene — the ingest mapping that turns a MyGene.info hit into a
  rasen genome graph node. It had no test. Pins the field mapping, the TWO Ensembl record shapes
  (MyGene returns the `ensembl` field as either a single map or a sequential of maps), the taxon
  resolution (9606 → :homo-sapiens, any other taxid → :taxid-N, missing → :homo-sapiens default),
  and that the optional cytoband/ensembl fields appear only when their source field is present —
  so a regression that mis-keyed a field or broke the Ensembl-shape handling is caught."
  (:require [rasen.methods.ingest :as ing]
            [clojure.test :refer [deftest is run-tests]]))

(deftest maps-a-full-mygene-hit-to-a-gene-node
  (let [n (ing/gene-node-from-mygene
           {"name" "BRCA1 DNA repair associated" "taxid" 9606
            "ensembl" {"gene" "ENSG00000012048"} "map_location" "17q21.31"} "BRCA1")]
    (is (= "gene.brca1" (get n ":genome/id")) "id = gene.<lowercased symbol>")
    (is (= ":gene" (get n ":genome/kind")))
    (is (= "BRCA1 DNA repair associated" (get n ":genome/label")) "label = the hit's name")
    (is (= "BRCA1" (get n ":gene/symbol")))
    (is (= ":homo-sapiens" (get n ":gene/taxon")))
    (is (= ":authoritative" (get n ":genome/sourcing")))
    (is (= "ENSG00000012048" (get n ":gene/ensembl")))
    (is (= "17q21.31" (get n ":gene/cytoband")))))

(deftest label-falls-back-to-symbol-when-name-absent
  (is (= "TP53" (get (ing/gene-node-from-mygene {"taxid" 9606} "TP53") ":genome/label"))))

(deftest handles-both-ensembl-record-shapes
  ;; MyGene returns `ensembl` as a single map OR a sequential of maps
  (is (= "ENSG1" (get (ing/gene-node-from-mygene {"ensembl" {"gene" "ENSG1"}} "x") ":gene/ensembl")))
  (is (= "ENSG2" (get (ing/gene-node-from-mygene {"ensembl" [{"gene" "ENSG2"} {"gene" "ENSG3"}]} "x") ":gene/ensembl"))
      "a sequential ensembl takes the first record's gene id"))

(deftest resolves-taxon-from-taxid
  (is (= ":homo-sapiens" (get (ing/gene-node-from-mygene {"taxid" 9606} "x") ":gene/taxon")))
  (is (= ":taxid-10090" (get (ing/gene-node-from-mygene {"taxid" 10090} "x") ":gene/taxon")) "non-human taxid → :taxid-N")
  (is (= ":homo-sapiens" (get (ing/gene-node-from-mygene {} "x") ":gene/taxon")) "missing taxid defaults to human"))

(deftest optional-fields-appear-only-when-sourced
  (let [bare (ing/gene-node-from-mygene {"taxid" 9606} "x")]
    (is (not (contains? bare ":gene/ensembl")) "no ensembl source → no :gene/ensembl key")
    (is (not (contains? bare ":gene/cytoband")) "no map_location → no :gene/cytoband key"))
  (is (= "gene.tp53" (get (ing/gene-node-from-mygene {} "TP53") ":genome/id")) "the id is always lowercased"))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (let [{:keys [fail error]} (run-tests 'rasen.methods.test-gene-node)]
       (System/exit (if (zero? (+ fail error)) 0 1)))))
