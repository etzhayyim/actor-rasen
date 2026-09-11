(ns rasen.tests.test-shard-archive
  "Tests for sealed-shard offload (ADR-2609062000).

  Almost everything here is about one failure: deleting a local shard on an upload that did not
  actually store the bytes. There is no recovery from it — the shard is canonical, and the
  ledger finds out at the next verify, when there is nothing left to restore from."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [rasen.methods.kotoba :as k]
            [rasen.methods.ledger-shards :as ls]
            [rasen.methods.shard-archive :as ar]))

(defn- tmp-root []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "rasen-ar-" (System/nanoTime)))
    (.mkdirs)))
(defn- rm-r [f] (when (.isDirectory f) (run! rm-r (.listFiles f))) (.delete f))

(defn- seed! [root n]
  (let [txs (loop [i 0 prev "" out []]
              (if (= i n) out
                  (let [ds [(k/add (str "gene.g" i) ":genome/kind" ":gene")]
                        tx (k/make-tx ds (str "tx" i) "t" prev)]
                    (recur (inc i) (get tx ":tx/cid") (conj out tx)))))]
    (ls/append-txs! root txs {:max-txs 1 :max-datoms 100000})))

(defn- memory-store
  "An object store in a map. `mangle` optionally corrupts what comes back out."
  ([] (memory-store {}))
  ([{:keys [put-fails? readback-nil? mangle]}]
   (let [m (atom {})]
     [m (fn [op k & [path]]
          (case op
            :put (when-not put-fails?
                   (swap! m assoc k (slurp (io/file path))) true)
            :get (let [v (get @m k)]
                   (cond readback-nil? nil
                         (nil? v) nil
                         :else (do (spit (io/file path) (if mangle (mangle v) v)) true)))
            :head (when-let [v (get @m k)] {:bytes (count v)})))])))

;; ── pure ────────────────────────────────────────────────────────────────────
(deftest only-shards-that-are-closed-to-appends-are-sealed
  (is (= [] (ar/sealed-seqs {":index/shards" []})) "nothing yet")
  (is (= [] (ar/sealed-seqs {":index/shards" [{":shard/seq" 0}]}))
      "the only shard is still open for appends")
  (is (= [0 1] (ar/sealed-seqs {":index/shards" [{":shard/seq" 0} {":shard/seq" 1}
                                                 {":shard/seq" 2}]}))
      "every shard but the last"))

(deftest object-keys-are-namespaced-by-ledger
  (is (= "rasen-genome/shards/rasen.genome.00003.kotoba.edn" (ar/object-key "rasen-genome" 3)))
  (is (not= (ar/object-key "a" 3) (ar/object-key "b" 3))
      "two ledgers in one bucket must not collide on shard number"))

;; ── the happy path ──────────────────────────────────────────────────────────
(deftest archiving-frees-the-local-copy-only-after-reading-it-back
  (let [root (tmp-root) [store store-fn] (memory-store)]
    (try
      (seed! root 4)
      (let [r (ar/archive! root {:store-fn store-fn})]
        (is (= [0 1 2] (:archived r)) "three sealed shards; the fourth is still open")
        (is (pos? (:freed-bytes r)))
        (is (empty? (:failed r)))
        (is (= 3 (count @store)))
        (doseq [n [0 1 2]]
          (is (not (.exists (io/file root "shards" (ls/shard-name n)))) "local copy freed"))
        (is (.exists (io/file root "shards" (ls/shard-name 3))) "the open shard is untouched"))
      (finally (rm-r root)))))

(deftest archiving-is-idempotent
  (let [root (tmp-root) [_ store-fn] (memory-store)]
    (try
      (seed! root 4)
      (ar/archive! root {:store-fn store-fn})
      (let [again (ar/archive! root {:store-fn store-fn})]
        (is (empty? (:archived again)) "already-archived shards are not re-uploaded")
        (is (empty? (:failed again)) "and they are not failures either"))
      (finally (rm-r root)))))

(deftest keep-local-still-verifies
  (let [root (tmp-root) [_ store-fn] (memory-store)]
    (try
      (seed! root 3)
      (let [r (ar/archive! root {:store-fn store-fn :keep-local? true})]
        (is (= [0 1] (:archived r)))
        (is (zero? (:freed-bytes r)))
        (is (.exists (io/file root "shards" (ls/shard-name 0))) "kept"))
      (finally (rm-r root)))))

;; ── the refusals ────────────────────────────────────────────────────────────
(deftest a-failed-put-never-deletes-the-local-shard
  (let [root (tmp-root) [_ store-fn] (memory-store {:put-fails? true})]
    (try
      (seed! root 4)
      (let [r (ar/archive! root {:store-fn store-fn})]
        (is (empty? (:archived r)))
        (is (= [:put-failed :put-failed :put-failed] (map :reason (:failed r))))
        (is (zero? (:freed-bytes r)))
        (doseq [n [0 1 2]]
          (is (.exists (io/file root "shards" (ls/shard-name n)))
              "the canonical bytes are still here")))
      (is (:ok (ls/verify! root)) "and the ledger still verifies")
      (finally (rm-r root)))))

(deftest a-put-that-cannot-be-read-back-never-deletes-the-local-shard
  (testing "the PUT succeeded; the store simply does not hand it back"
    (let [root (tmp-root) [_ store-fn] (memory-store {:readback-nil? true})]
      (try
        (seed! root 3)
        (let [r (ar/archive! root {:store-fn store-fn})]
          (is (empty? (:archived r)))
          (is (= [:readback-failed :readback-failed] (map :reason (:failed r))))
          (is (.exists (io/file root "shards" (ls/shard-name 0)))))
        (finally (rm-r root))))))

(deftest bytes-that-come-back-different-are-a-refusal-not-a-success
  (testing "a store that truncates or rewrites is the case a status code cannot see"
    (let [root (tmp-root)
          [_ store-fn] (memory-store {:mangle (fn [v] (subs v 0 (max 1 (- (count v) 5))))})]
      (try
        (seed! root 3)
        (let [r (ar/archive! root {:store-fn store-fn})
              f (first (:failed r))]
          (is (empty? (:archived r)))
          (is (= :sha-mismatch (:reason f)))
          (is (not= (:local f) (:store f)))
          (is (.exists (io/file root "shards" (ls/shard-name 0)))
              "nothing is deleted on a mismatch"))
        (finally (rm-r root))))))

(deftest archiving-without-an-index-is-refused
  (let [root (tmp-root)]
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (ar/archive! root {:store-fn (fn [& _] true)}))
          "no index means nothing is known to be sealed")
      (finally (rm-r root)))))

(deftest no-store-fn-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (ar/archive! (tmp-root) {}))))

;; ── restoring ───────────────────────────────────────────────────────────────
(deftest a-freed-shard-comes-back-and-the-ledger-verifies-again
  (let [root (tmp-root) [_ store-fn] (memory-store)]
    (try
      (seed! root 5)
      (ar/archive! root {:store-fn store-fn})
      (let [v (ls/verify! root)]
        (is (false? (:ok v)) "with shards freed, a plain verify cannot read the whole chain")
        (is (= :shard-missing (:reason v))))
      (is (= [0 1 2 3] (ar/restore-all! root {:store-fn store-fn})))
      (is (:ok (ls/verify! root)) "restored, the chain verifies exactly as before")
      (finally (rm-r root)))))

(deftest a-store-that-returns-different-bytes-cannot-satisfy-a-restore
  (let [root (tmp-root) [store store-fn] (memory-store)]
    (try
      (seed! root 3)
      (ar/archive! root {:store-fn store-fn})
      (swap! store update (ar/object-key "rasen-genome" 0) #(str % "\n;; tampered"))
      (let [e (try (ar/ensure-local! root 0 {:store-fn store-fn}) nil
                   (catch clojure.lang.ExceptionInfo ex ex))]
        (is (some? e))
        (is (re-find #"do not match the manifest" (ex-message e)))
        (is (not (.exists (io/file root "shards" (ls/shard-name 0))))
            "the bad bytes are not left on disk to be verified later"))
      (finally (rm-r root)))))

(deftest a-shard-that-is-neither-local-nor-archived-is-refused
  (let [root (tmp-root)]
    (try
      (seed! root 2)
      (is (thrown? clojure.lang.ExceptionInfo (ar/ensure-local! root 9 {:store-fn (fn [& _] true)}))
          "not on disk and not in the manifest is UNVERIFIED, not empty")
      (finally (rm-r root)))))

(deftest ensure-local-on-a-present-shard-does-not-touch-the-store
  (let [root (tmp-root)]
    (try
      (seed! root 2)
      (is (= :present (ar/ensure-local! root 0 {:store-fn (fn [& _] (throw (Exception. "no")))})))
      (finally (rm-r root)))))
