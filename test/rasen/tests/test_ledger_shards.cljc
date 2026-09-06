(ns rasen.tests.test-ledger-shards
  "Tests for the sharded genome ledger (ADR-2609062000).

  The point of sharding is not that it splits files — that part is trivial and cannot fail
  interestingly. The point is that a ledger spread over many files can be INCOMPLETE, and an
  incomplete chain verifies perfectly if you only walk the shards you happen to find. Most of
  what is asserted here is that `verify!` refuses in that situation, with a reason that names
  what was wrong."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.kotoba :as k]
            [rasen.methods.ledger-shards :as ls]))

(defn- tmp-root []
  (let [f (io/file (System/getProperty "java.io.tmpdir")
                   (str "rasen-shard-test-" (System/nanoTime)))]
    (.mkdirs f) f))

(defn- rm-r [f]
  (when (.isDirectory f) (run! rm-r (.listFiles f)))
  (.delete f))

(defn- chain
  "n txs, each carrying `per` datoms, correctly prev-cid chained from genesis."
  ([n] (chain n 2))
  ([n per]
   (loop [i 0 prev "" out []]
     (if (= i n) out
         (let [datoms (vec (for [j (range per)]
                             (k/add (str "gene.g" i "-" j) ":genome/kind" ":gene")))
               tx (k/make-tx datoms (str "tx" i) "test" prev)]
           (recur (inc i) (get tx ":tx/cid") (conj out tx)))))))

;; ── the index round-trips through the repo's own reader ─────────────────────
(deftest index-round-trips-through-the-minimal-reader
  (testing "pr-str would emit #:index{…}, which the minimal reader turns into a string"
    (let [root (tmp-root)]
      (try
        (ls/append-txs! root (chain 3))
        (let [ix (ls/read-index root)]
          (is (map? ix) "the index must read back as a map, not as \"#:index\"")
          (is (= 3 (get ix ":index/tx-count")))
          (is (= 1 (get ix ":index/version")))
          (is (vector? (get ix ":index/shards"))))
        (finally (rm-r root))))))

(deftest index-file-is-real-edn
  (testing "run_tests.clj reads every .edn in the tree with clojure.edn"
    (let [root (tmp-root)]
      (try
        (ls/append-txs! root (chain 2))
        (let [text (slurp (io/file root "rasen.genome.shards.edn"))
              parsed (clojure.edn/read-string (str/join "\n" (remove #(str/starts-with? % ";;")
                                                                    (str/split-lines text))))]
          (is (map? parsed))
          (is (= 2 (:index/tx-count parsed)) "clojure.edn sees real keywords"))
        (finally (rm-r root))))))

;; ── sharding preserves the chain exactly ────────────────────────────────────
(deftest rolling-produces-several-shards-and-one-chain
  (let [root (tmp-root)]
    (try
      (let [txs (chain 10 3)
            ix (ls/append-txs! root txs {:max-txs 3 :max-datoms 1000000})
            v (ls/verify! root)]
        (is (= 4 (count (get ix ":index/shards"))) "10 txs at 3 per shard")
        (is (:ok v))
        (is (= 10 (:tx-count v)))
        (is (= (get (last txs) ":tx/cid") (get ix ":index/head-cid"))
            "the head is the last tx's cid — unchanged by where it was stored"))
      (finally (rm-r root)))))

(deftest datom-bound-also-rolls
  (let [root (tmp-root)]
    (try
      (let [ix (ls/append-txs! root (chain 6 5) {:max-txs 1000 :max-datoms 10})]
        (is (< 1 (count (get ix ":index/shards"))) "30 datoms at 10 per shard must roll"))
      (finally (rm-r root)))))

(deftest head-cid-is-read-from-the-index-not-the-shards
  (let [root (tmp-root)]
    (try
      (let [txs (chain 5)]
        (ls/append-txs! root txs {:max-txs 2 :max-datoms 1000000})
        (is (= (get (last txs) ":tx/cid") (ls/head-cid root))))
      (finally (rm-r root)))))

;; ── the refusals: an incomplete ledger must never look ok ───────────────────
(deftest no-index-is-unverified-not-ok-and-not-empty
  (let [root (tmp-root)]
    (try
      (let [v (ls/verify! root)]
        (is (false? (:ok v)))
        (is (= :no-index (:reason v)) "an absent index is UNVERIFIED, never a clean empty ledger"))
      (is (nil? (ls/head-cid root)) "no index means no answer, not \"\"")
      (finally (rm-r root)))))

(deftest a-missing-shard-is-refused-not-silently-skipped
  (let [root (tmp-root)]
    (try
      (ls/append-txs! root (chain 9) {:max-txs 3 :max-datoms 1000000})
      (is (:ok (ls/verify! root)) "control: intact ledger verifies")
      (let [victim (io/file root "shards" (ls/shard-name 1))]
        (is (.exists victim))
        (.delete victim))
      (let [v (ls/verify! root)]
        (is (false? (:ok v)) "a ledger with a hole in it is not ok")
        (is (= :shard-missing (:reason v)) "refused for the missing shard, not for something else")
        (is (= 1 (:seq v)) "and it names which shard"))
      (finally (rm-r root)))))

(deftest dropping-the-last-shard-is-refused
  (testing "the remaining chain links perfectly — only the index knows it is short"
    (let [root (tmp-root)]
      (try
        (ls/append-txs! root (chain 9) {:max-txs 3 :max-datoms 1000000})
        (.delete (io/file root "shards" (ls/shard-name 2)))
        (let [v (ls/verify! root)]
          (is (false? (:ok v)))
          (is (= :shard-missing (:reason v))))
        (finally (rm-r root))))))

(deftest a-shard-with-txs-cut-off-its-end-hits-the-count-floor
  (testing "every shard is present and every link holds; the ledger is merely SHORT.
            This is the case the chain alone cannot see, and the only thing that catches it
            is the index saying how many txs must be there."
    (let [root (tmp-root)]
      (try
        (ls/append-txs! root (chain 9) {:max-txs 3 :max-datoms 1000000})
        (is (:ok (ls/verify! root)) "control: intact ledger verifies")
        (let [f (io/file root "shards" (ls/shard-name 2))
              kept (butlast (str/split-lines (slurp f)))]
          (spit f (str (str/join "\n" kept) "\n")))
        (let [v (ls/verify! root)]
          (is (false? (:ok v)) "a short ledger is not ok, even though nothing is inconsistent")
          (is (= :count-mismatch (:reason v)) "caught by the floor, not by a broken link")
          (is (= 8 (:read v)))
          (is (= 9 (:claimed v))))
        (finally (rm-r root))))))

(deftest a-tampered-datom-breaks-the-chain-at-a-named-position
  (let [root (tmp-root)]
    (try
      (ls/append-txs! root (chain 6) {:max-txs 2 :max-datoms 1000000})
      (is (:ok (ls/verify! root)) "control: intact ledger verifies")
      (let [f (io/file root "shards" (ls/shard-name 1))
            text (slurp f)]
        (spit f (str/replace text "gene.g2-0" "gene.TAMPERED")))
      (let [v (ls/verify! root)]
        (is (false? (:ok v)))
        (is (= :chain-broken (:reason v)) "refused as a broken chain, not as a missing shard")
        (is (= 1 (:shard v)))
        (is (= 2 (:at v)) "the global tx number of the first tx that does not follow"))
      (finally (rm-r root)))))

(deftest appending-a-run-that-does-not-continue-the-head-is-refused
  (let [root (tmp-root)]
    (try
      (ls/append-txs! root (chain 3))
      (let [e (try (ls/append-txs! root (chain 2)) nil (catch Exception e e))]
        (is (some? e))
        (is (= "append would break the chain" (ex-message e))
            "refused for the stated reason — a fresh genesis run does not continue this head"))
      (is (:ok (ls/verify! root)) "and the ledger is untouched by the refusal")
      (finally (rm-r root)))))

;; ── index self-consistency ──────────────────────────────────────────────────
(deftest index-violations-refuses-to-report-clean-for-a-non-index
  (doseq [bad [nil "shards" 7 {} {:index/version 1}]]
    (is (thrown? Exception (ls/index-violations bad))
        "an absent or unrecognisable index is UNVERIFIED, never clean")))

(deftest index-violations-finds-a-gap-in-the-shard-sequence
  (let [ix {":index/version" 1 ":index/tx-count" 2 ":index/datom-count" 2 ":index/head-cid" "b2"
            ":index/shards" [{":shard/seq" 0 ":shard/tx-count" 1 ":shard/datom-count" 1
                              ":shard/prev-cid" "" ":shard/first-cid" "b1" ":shard/last-cid" "b1"}
                             {":shard/seq" 2 ":shard/tx-count" 1 ":shard/datom-count" 1
                              ":shard/prev-cid" "b1" ":shard/first-cid" "b2" ":shard/last-cid" "b2"}]}
        vs (ls/index-violations ix)]
    (is (= 1 (count vs)))
    (is (= :shard/out-of-sequence (:violation (first vs))))))

(deftest index-violations-finds-a-claimed-count-that-its-shards-do-not-sum-to
  (let [ix {":index/version" 1 ":index/tx-count" 99 ":index/datom-count" 1 ":index/head-cid" "b1"
            ":index/shards" [{":shard/seq" 0 ":shard/tx-count" 1 ":shard/datom-count" 1
                              ":shard/prev-cid" "" ":shard/first-cid" "b1" ":shard/last-cid" "b1"}]}]
    (is (= [:count/tx-mismatch] (mapv :violation (ls/index-violations ix))))))

(deftest a-self-contradicting-index-is-refused-before-any-shard-is-read
  (let [root (tmp-root)]
    (try
      (ls/append-txs! root (chain 3))
      (ls/write-index! root (assoc (ls/read-index root) ":index/tx-count" 99))
      (let [v (ls/verify! root)]
        (is (false? (:ok v)))
        (is (= :index-invalid (:reason v)))
        (is (seq (:violations v)) "and it says what contradicts what"))
      (finally (rm-r root)))))

;; ── the index is a projection ───────────────────────────────────────────────
(deftest reindex-rebuilds-the-same-index-from-the-shards-alone
  (let [root (tmp-root)]
    (try
      (let [before (ls/append-txs! root (chain 8) {:max-txs 3 :max-datoms 1000000})]
        (.delete (io/file root "rasen.genome.shards.edn"))
        (is (nil? (ls/read-index root)) "control: the index is gone")
        (let [after (ls/reindex! root)]
          (is (= before after) "the index is derivable from the shards — it is not canonical")
          (is (:ok (ls/verify! root)))))
      (finally (rm-r root)))))

(deftest reindex-refuses-to-synthesise-an-index-for-an-empty-directory
  (let [root (tmp-root)]
    (try
      (let [e (try (ls/reindex! root) nil (catch Exception e e))]
        (is (some? e))
        (is (str/includes? (ex-message e) "no shard 0")
            "an empty directory is not an empty-but-valid ledger"))
      (finally (rm-r root)))))

;; ── migration off the single-file ledger ────────────────────────────────────
(deftest migrate-preserves-every-cid-and-the-head
  (let [root (tmp-root)
        log (io/file root "single.kotoba.edn")
        dst (io/file root "sharded")]
    (try
      (let [txs (chain 7 4)]
        (doseq [tx txs] (k/append-tx tx (str log)))
        (let [src-head (k/head-cid (str log))
              r (ls/migrate! (str log) dst {:max-txs 2 :max-datoms 1000000})]
          (is (:ok r))
          (is (= 7 (:tx-count r)))
          (is (= 4 (:shards r)))
          (is (= src-head (:head r)) "sharding does not touch the chain")
          (is (:ok (ls/verify! dst)))
          (let [flat (mapcat #(ls/read-shard dst %) (range 4))]
            (is (= (mapv #(get % ":tx/cid") txs) (mapv #(get % ":tx/cid") flat))
                "same txs, same order, same cids"))))
      (finally (rm-r root)))))

(deftest migrate-refuses-to-overwrite-an-existing-index
  (let [root (tmp-root)
        log (io/file root "single.kotoba.edn")
        dst (io/file root "sharded")]
    (try
      (doseq [tx (chain 2)] (k/append-tx tx (str log)))
      (ls/migrate! (str log) dst)
      (let [e (try (ls/migrate! (str log) dst) nil (catch Exception e e))]
        (is (some? e))
        (is (str/includes? (ex-message e) "refusing to overwrite")))
      (finally (rm-r root)))))

(deftest migrate-refuses-an-empty-source
  (let [root (tmp-root)]
    (try
      (let [e (try (ls/migrate! (str (io/file root "nothing.edn")) (io/file root "d")) nil
                   (catch Exception e e))]
        (is (some? e))
        (is (str/includes? (ex-message e) "nothing to migrate")
            "an absent source must not produce an empty sharded ledger"))
      (finally (rm-r root)))))
