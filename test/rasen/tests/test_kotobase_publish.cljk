(ns rasen.tests.test-kotobase-publish
  "Tests for the ledger → kotobase.net projection (ADR-2609062000).

  The failure worth engineering against is a watermark that advances on something that was not
  an acknowledgement. That leaves a hole in the projection which no later run will fill,
  because every later run starts after it."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.kotoba :as k]
            [rasen.methods.kotobase-publish :as pub]
            [rasen.methods.ledger-shards :as ls]))

(defn- tmp-root []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "rasen-pub-" (System/nanoTime)))
    (.mkdirs)))
(defn- rm-r [f] (when (.isDirectory f) (run! rm-r (.listFiles f))) (.delete f))

(defn- seed-ledger! [root n]
  (let [txs (loop [i 0 prev "" out []]
              (if (= i n) out
                  (let [ds [(k/add (str "gene.g" i) ":genome/kind" ":gene")
                            (k/add (str "gene.g" i) ":gene/symbol" (str "G" i))]
                        tx (k/make-tx ds (str "tx" i) "test" prev)]
                    (recur (inc i) (get tx ":tx/cid") (conj out tx)))))]
    (ls/append-txs! root txs {:max-txs 2 :max-datoms 100000})
    txs))

(def ack {:commit "b-ok"})

;; ── pure conversion ─────────────────────────────────────────────────────────
(deftest house-strings-become-real-keywords
  (is (= :genome/kind (pub/kw-or-string ":genome/kind")))
  (is (= :gene (pub/kw-or-string ":gene")))
  (is (= "BRCA1" (pub/kw-or-string "BRCA1")) "a plain string stays a string")
  (is (= 0.75 (pub/kw-or-string 0.75)))
  (is (= ":" (pub/kw-or-string ":")) "a bare colon is not a keyword"))

(deftest datom-conversion-and-its-refusal
  (is (= [:db/add "gene.brca1" :gene/symbol "BRCA1"]
         (pub/datom->tx-form [":db/add" "gene.brca1" ":gene/symbol" "BRCA1"] identity)))
  (is (= [:db/add [:genome/id "gene.brca1"] :gene/symbol "BRCA1"]
         (pub/datom->tx-form [":db/add" "gene.brca1" ":gene/symbol" "BRCA1"]
                             (fn [e] [:genome/id e])))
      "entity-fn is where a target's identity convention goes")
  (doseq [bad [[":db/retract" "e" ":a" "v"] [":db/add" "e" ":a"] nil "nope"]]
    (is (thrown? clojure.lang.ExceptionInfo (pub/datom->tx-form bad identity))
        "anything that is not a ledger :db/add is refused, never dropped")))

(deftest acknowledgement-is-recognised-not-assumed
  (testing "only a result that says it succeeded advances anything"
    (is (true? (pub/acknowledged? {:commit "b1"})))
    (is (true? (pub/acknowledged? {"db-after" {}})))
    (is (true? (pub/acknowledged? {:ok true})))
    (is (false? (pub/acknowledged? nil)) "nil is the transport saying nothing")
    (is (false? (pub/acknowledged? {})) "an empty map claims nothing")
    (is (false? (pub/acknowledged? "202 Accepted")) "a string is not an acknowledgement")
    (is (false? (pub/acknowledged? {:error "Unauthorized"})))
    (is (false? (pub/acknowledged? {:commit "b1" :error "partial"}))
        "an error alongside a commit is still an error")))

(deftest a-watermark-ahead-of-the-ledger-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (pub/plan 3 5))
      "the projection cannot have seen more transactions than exist")
  (is (= {:from-tx 3 :pending 2} (pub/plan 5 3))))

;; ── publishing ──────────────────────────────────────────────────────────────
(deftest a-full-pass-publishes-every-transaction-once
  (let [root (tmp-root) sent (atom [])]
    (try
      (seed-ledger! root 5)
      (let [r (pub/publish! root {:transact-fn (fn [edn] (swap! sent conj edn) ack)})]
        (is (= :published (:status r)))
        (is (= 5 (:published-tx r)))
        (is (= 5 (count @sent)))
        (is (= 5 (:transactions-attempted r)))
        (is (re-find #"\[:db/add \"gene.g0\" :genome/kind :gene\]" (first @sent))
            "house strings arrive as real keywords"))
      (let [again (pub/publish! root {:transact-fn (fn [_] (throw (Exception. "must not send")))})]
        (is (= :up-to-date (:status again)) "a second pass sends nothing"))
      (finally (rm-r root)))))

(deftest an-unacknowledged-write-stops-the-run-and-does-not-advance-past-it
  (testing "this is the hole: advancing here would skip a transaction forever"
    (let [root (tmp-root) n (atom 0)]
      (try
        (seed-ledger! root 6)
        (let [r (pub/publish! root {:transact-fn (fn [_] (swap! n inc)
                                                  (if (<= @n 2) ack nil))})]
          (is (= :stopped (:status r)))
          (is (= 2 (:published-tx r)) "the watermark stands at the last ACKNOWLEDGED tx")
          (is (= 3 (:transactions-attempted r)) "three were handed to the transport")
          (is (= 2 (:transactions-acknowledged r)) "only two came back acknowledged")
          (is (nil? (:unacknowledged r))))
        (let [resumed (pub/publish! root {:transact-fn (fn [_] ack)})]
          (is (= :published (:status resumed)))
          (is (= 6 (:published-tx resumed)))
          (is (= 4 (:transactions-acknowledged resumed))
              "the run resumes at the unacknowledged transaction, not after it"))
        (finally (rm-r root))))))

(deftest a-401-body-is-not-an-acknowledgement
  (let [root (tmp-root)]
    (try
      (seed-ledger! root 3)
      (let [r (pub/publish! root {:transact-fn (fn [_] {:error "Unauthorized"})})]
        (is (= :stopped (:status r)))
        (is (= 0 (:published-tx r)) "nothing was projected")
        (is (= {:error "Unauthorized"} (:unacknowledged r)) "and the run says what came back"))
      (finally (rm-r root)))))

(deftest an-unverifiable-ledger-is-never-projected
  (testing "projecting datoms no shard accounts for would end the rebuildability"
    (let [root (tmp-root)]
      (try
        (seed-ledger! root 6)
        (.delete (io/file root "shards" (ls/shard-name 1)))
        (let [r (pub/publish! root {:transact-fn (fn [_] (throw (Exception. "must not send")))})]
          (is (= :ledger-unverified (:status r)))
          (is (= :shard-missing (:reason (:verify r)))))
        (finally (rm-r root))))))

(deftest a-bounded-pass-publishes-only-its-limit
  (let [root (tmp-root)]
    (try
      (seed-ledger! root 8)
      (let [r (pub/publish! root {:transact-fn (fn [_] ack) :limit 3})]
        (is (= :published (:status r)))
        (is (= 3 (:published-tx r)))
        (is (< (:published-tx r) (:ledger-tx r)) "and it does not claim to be caught up"))
      (finally (rm-r root)))))

(deftest no-transact-fn-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (pub/publish! (tmp-root) {}))))
