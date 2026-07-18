#!/usr/bin/env bb
;; rasen 螺旋 — autonomous heartbeat: load the genome graph → append GROUND datoms.
(ns rasen.methods.autorun
  "autorun.cljc — rasen 螺旋 deterministic heartbeat (ADR-2606101000, inochi/tate pattern).

  One beat: load the public-genetics seed graph, build the GROUND EAVT datoms (gene/variant/
  phenotype/population/pathway NODES + their :en/* 縁), and APPEND them as ONE content-addressed
  transaction to the append-only GENOME LEDGER (kotoba.cljc). prev-cid chaining keeps the ledger
  tamper-evident + resume-safe.

  GROUND ONLY: care-priority / locus-burden / pleiotropy readouts are DERIVED (computed on read,
  N1/G2) and are NEVER persisted. Deterministic by construction: the caller supplies tx-id +
  as-of (no wall clock); nodes emit in EDN-read order (node-order) + edges content-stable →
  resume-safe. IDEMPOTENT-BY-CONTENT: a beat whose ground datoms equal the previous beat's is a
  NO-OP. No-server-key: appends to a local file only, no network I/O. G1: a CARE/RESEARCH map —
  aggregate genetics only, never an individual-genotype registry."
  (:require [clojure.string :as str]
            [rasen.methods.datom-emit :as de]
            [rasen.methods.kotoba :as k]
            #?(:clj [clojure.java.io :as io])))

(defn- node-order [nodes]
  ;; de/load-file* attaches :rasen.methods.datom-emit/node-order; fall back to (keys nodes)
  (or (:rasen.methods.datom-emit/node-order (meta nodes)) (keys nodes)))

(defn- strip-colon [s] (if (str/starts-with? s ":") (subs s 1) s))

(defn ground-datoms-from
  "Build the durable GROUND EAVT datoms from a loaded {:nodes :edges} graph, in the
  canonical [\":db/add\" e a v] form. Derived readouts excluded by design (N1/G2)."
  [{:keys [nodes edges]}]
  (let [out (transient [])]
    (doseq [nid (node-order nodes)]
      (let [n (get nodes nid)]
        (doseq [a de/node-attrs :when (and (contains? n a) (some? (get n a)))]
          (conj! out (k/add nid a (get n a))))))
    (doseq [e edges]
      (let [eid (str "en." (get e ":en/from") "."
                     (strip-colon (str (get e ":en/kind"))) "." (get e ":en/to"))]
        (doseq [a de/edge-attrs :when (and (contains? e a) (some? (get e a)))]
          (conj! out (k/add eid a (get e a))))))
    (persistent! out)))

#?(:clj
   (def ^:private here-dir
     (io/file ".")))

#?(:clj
   (defn default-seed-path []
     (str (io/file here-dir "data" "seed-genome-graph.kotoba.edn"))))

#?(:clj
   (defn ground-datoms
     ([] (ground-datoms (default-seed-path)))
     ([seed-path] (ground-datoms-from (de/load-file* seed-path)))))

(defn beat
  "Run one heartbeat. opts:
     :datoms   ground datoms (optional; defaults to (ground-datoms))
     :tx-id    deterministic tx id (required)
     :as-of    deterministic as-of stamp (required)
     :log-path genome-ledger path (required)
   IDEMPOTENT-BY-CONTENT: if the new ground datoms equal the last beat's datoms,
   the beat is a NO-OP — nothing is appended.
   Returns {:head <cid> :count <n> :appended <bool> :reason <kw|nil>}."
  [{:keys [datoms tx-id as-of log-path]}]
  (let [ds (or datoms (ground-datoms))
        prev (k/head-cid log-path)
        last-ds (let [txs (k/read-log log-path)]
                  (when (seq txs) (get (last txs) ":tx/datoms")))
        unchanged? (= ds last-ds)
        base {:count (count ds)}]
    (if unchanged?
      (assoc base :head prev :appended false :reason :no-change)
      (let [tx (k/make-tx ds tx-id as-of prev)
            head (k/append-tx tx log-path)]
        (assoc base :head head :appended true :reason nil)))))

#?(:clj
   (defn -main [& args]
     (let [log-path (or (first args)
                        (str (io/file here-dir "data" "persisted" "rasen.genome.kotoba.edn")))
           r (beat {:tx-id "rasen-beat-manual" :as-of "manual" :log-path log-path})]
       (println (str "genome ledger head=" (:head r)
                     " datoms=" (:count r)
                     " appended=" (:appended r)
                     (when (:reason r) (str " (" (name (:reason r)) ")"))))
       (println (str "chain=" (k/verify-chain log-path))))))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (apply -main *command-line-args*)))
