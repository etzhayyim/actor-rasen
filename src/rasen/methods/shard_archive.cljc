(ns rasen.methods.shard-archive
  "shard_archive.cljc — rasen 螺旋 sealed-shard offload to an object store (ADR-2609062000).

  The ledger outgrows the machine that writes it. ClinVar alone normalises to ~6.2 GB of
  shards; the host that measured that had 2.3 GB free. A sealed shard is immutable — the
  shard index records its tx range and the chain fixes its bytes — so it can live in an object
  store and be fetched back when something needs to read it.

  THE ONE THING THIS MUST NEVER DO IS DELETE A SHARD IT ONLY BELIEVES IT UPLOADED. A PUT that
  returns 200 for a body that was never stored, a partial write, a bucket that silently drops
  it — each ends with the local copy removed and the canonical bytes gone, and the ledger only
  discovers it at the next verify, by which time there is nothing to restore from. So
  `archive!` reads the object BACK and compares sha256 with what it uploaded, and deletes the
  local file only after that comparison. An upload it cannot confirm leaves the local shard
  exactly where it was and reports why.

  CARRIES NO CLIENT AND NO CREDENTIAL. Everything goes through an injected `store-fn`:
    (store-fn :put  key local-path) -> truthy on success
    (store-fn :get  key local-path) -> truthy on success
    (store-fn :head key)            -> {:bytes n} or nil
  A host wires that to R2/S3/B2. Only the LAST shard is ever open for appends, so `archive!`
  refuses to touch it — uploading a shard that is still growing would store a prefix and call
  it the shard."
  (:require [kotoba.lang.text :as str]
            [rasen.methods.kotoba :as k]
            [rasen.methods.ledger-shards :as ls]
            #?(:clj [clojure.java.io :as io])))

(defn object-key
  "Where a shard lives in the store. Prefixed by the ledger name so one bucket can hold more
  than one ledger without their shard numbers colliding."
  [ledger seq-n]
  (str ledger "/shards/" (ls/shard-name seq-n)))

(defn sealed-seqs
  "Shard numbers that are closed to appends: every shard except the last. Returns [] for an
  index with fewer than two shards — there is nothing sealed yet, which is different from
  nothing to do and is why the caller gets an empty list rather than an error."
  [index]
  (let [shards (vec (get index ":index/shards"))]
    (if (< (count shards) 2) [] (mapv #(get % ":shard/seq") (butlast shards)))))

#?(:clj
   (do
     (defn- manifest-path [root] (io/file root "shards.archive.edn"))
     (defn- shard-file [root seq-n] (io/file root "shards" (ls/shard-name seq-n)))

     (defn sha256-of [path]
       (let [md (java.security.MessageDigest/getInstance "SHA-256")
             buf (byte-array 65536)]
         (with-open [in (io/input-stream (io/file (str path)))]
           (loop [] (let [n (.read in buf)] (when (pos? n) (.update md buf 0 n) (recur)))))
         (apply str (map #(format "%02x" (bit-and (int %) 0xff)) (.digest md)))))

     (defn read-manifest
       "Which shards are in the store, or nil when nothing has been archived. nil is
       UNVERIFIED — it does not mean the store is empty, only that this ledger has no record."
       [root]
       (let [f (manifest-path root)] (when (.exists f) (k/parse-edn (slurp f)))))

     (defn write-manifest! [root m]
       (let [f (manifest-path root)]
         (when-let [p (.getParentFile f)] (.mkdirs p))
         (spit f (str ";; rasen 螺旋 — archived shard manifest. Generated. Records only shards whose\n"
                      ";; bytes were read BACK from the store and matched; never what was merely sent.\n"
                      "{:archive/ledger " (pr-str (get m ":archive/ledger"))
                      "\n :archive/shards ["
                      (str/join "\n                   "
                                (map (fn [e] (str "{:shard/seq " (get e ":shard/seq")
                                                  " :shard/key " (pr-str (get e ":shard/key"))
                                                  " :shard/sha256 " (pr-str (get e ":shard/sha256"))
                                                  " :shard/bytes " (get e ":shard/bytes") "}"))
                                     (get m ":archive/shards")))
                      "]}\n"))
         m))

     (defn archive!
       "Upload every sealed, not-yet-archived shard and free its local copy.

       For each shard: sha256 the local file, PUT it, GET it back to a temp file, sha256 that,
       and only if the two match record it and delete the local file. Returns
       {:archived [seqs] :freed-bytes n :failed [{seq reason}]}.

       `:keep-local?` skips the delete — the verification still runs, so a caller can prove the
       store has the bytes without giving up the local copy."
       [root {:keys [store-fn ledger keep-local?] :or {ledger "rasen-genome"}}]
       (when-not store-fn (throw (ex-info "archive!: no :store-fn" {})))
       (let [ix (ls/read-index root)]
         (when-not ix (throw (ex-info "archive!: no shard index — nothing is sealed" {})))
         (let [m0 (or (read-manifest root) {":archive/ledger" ledger ":archive/shards" []})
               done (set (map #(get % ":shard/seq") (get m0 ":archive/shards")))
               todo (remove done (sealed-seqs ix))]
           (loop [ss (seq todo), entries (vec (get m0 ":archive/shards")),
                  ok [], failed [], freed 0]
             (if-not ss
               (do (write-manifest! root {":archive/ledger" ledger ":archive/shards" entries})
                   {:archived ok :freed-bytes freed :failed failed})
               (let [n (first ss)
                     f (shard-file root n)
                     k (object-key ledger n)]
                 (if-not (.exists f)
                   (recur (next ss) entries ok (conj failed {:seq n :reason :local-missing}) freed)
                   (let [local-sha (sha256-of f)
                         size (.length f)
                         put-ok (store-fn :put k (str f))
                         back (io/file (System/getProperty "java.io.tmpdir")
                                       (str "rasen-verify-" n "-" (System/nanoTime)))
                         got (when put-ok (store-fn :get k (str back)))
                         back-sha (when (and got (.exists back)) (sha256-of back))]
                     (.delete back)
                     (cond
                       (not put-ok)
                       (recur (next ss) entries ok (conj failed {:seq n :reason :put-failed}) freed)
                       (nil? back-sha)
                       (recur (next ss) entries ok (conj failed {:seq n :reason :readback-failed}) freed)
                       (not= local-sha back-sha)
                       (recur (next ss) entries ok
                              (conj failed {:seq n :reason :sha-mismatch
                                            :local local-sha :store back-sha}) freed)
                       :else
                       (do (when-not keep-local? (.delete f))
                           (recur (next ss)
                                  (conj entries {":shard/seq" n ":shard/key" k
                                                 ":shard/sha256" local-sha ":shard/bytes" size})
                                  (conj ok n) failed
                                  (if keep-local? freed (+ freed size)))))))))))))

     (defn ensure-local!
       "Fetch shard `seq-n` back from the store if it is not on disk. Returns :present,
       :restored, or throws. Verifies the restored bytes against the manifest sha — a store
       that hands back different bytes must not be allowed to satisfy a verify."
       [root seq-n {:keys [store-fn ledger] :or {ledger "rasen-genome"}}]
       (let [f (shard-file root seq-n)]
         (if (.exists f)
           :present
           (let [m (read-manifest root)
                 e (first (filter #(= seq-n (get % ":shard/seq")) (get m ":archive/shards")))]
             (when-not e
               (throw (ex-info "ensure-local!: shard is neither on disk nor in the manifest"
                               {:seq seq-n})))
             (when-not store-fn (throw (ex-info "ensure-local!: no :store-fn" {:seq seq-n})))
             (when-let [p (.getParentFile f)] (.mkdirs p))
             (when-not (store-fn :get (get e ":shard/key") (str f))
               (throw (ex-info "ensure-local!: store did not return the shard"
                               {:seq seq-n :key (get e ":shard/key")})))
             (let [sha (sha256-of f)]
               (when (not= sha (get e ":shard/sha256"))
                 (.delete f)
                 (throw (ex-info "ensure-local!: restored bytes do not match the manifest"
                                 {:seq seq-n :expected (get e ":shard/sha256") :got sha}))))
             :restored))))

     (defn restore-all!
       "Bring every archived shard back to disk (for a full verify). Returns the seqs restored."
       [root opts]
       (let [m (read-manifest root)]
         (vec (keep (fn [e]
                      (let [n (get e ":shard/seq")]
                        (when (= :restored (ensure-local! root n opts)) n)))
                    (get m ":archive/shards")))))))
