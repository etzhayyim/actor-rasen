(ns rasen.methods.ledger-shards
  "ledger_shards.cljc — rasen 螺旋 SHARDED genome ledger (ADR-2609062000).

  `rasen.methods.kotoba` writes the append-only, prev-cid-chained commit-DAG to ONE file and
  reads it with `slurp`. That is correct and was sufficient for a bounded seed (583 datoms).
  It cannot hold the full public corpus: ClinVar alone normalises to tens of millions of
  datoms, and every read of the log would materialise all of them.

  Sharding does NOT change the chain. A tx CID is sha256 over (datoms, prev-cid) and nothing
  else — not the file it lives in, not its offset. Splitting the same linear sequence across
  files therefore preserves every CID exactly, which `migrate!` asserts rather than assumes.

  THE FAILURE THIS GUARDS AGAINST. Once the log is many files, the cheap way to verify is to
  walk the files you can find. A missing shard then produces a SHORTER chain that verifies
  perfectly — 'could not read shard 7' returns the same ok as 'read every shard and all
  linked'. So the index is not an optimisation here, it is the evidence floor: it records how
  many shards and txs must exist, `verify!` refuses to report ok unless it saw exactly that
  many, and a gap in the shard sequence is a distinct refusal reason from a broken link.

  The index is a PROJECTION — it can be deleted and rebuilt by scanning the shards in order
  (`reindex!`). The shards are canonical. Nothing derived is stored in them."
  (:require [clojure.string :as str]
            [rasen.methods.kotoba :as k]
            #?(:clj [clojure.java.io :as io])))

(def index-version 1)

(def default-capacity
  "Roll to a new shard when EITHER bound is exceeded. A tx is never split across shards, so a
  single oversized tx makes a shard that exceeds :max-datoms — that is correct, and the
  invariant checks are written to permit it rather than to report the ledger broken."
  {:max-txs 1000 :max-datoms 200000})

(defn- edn-scalar
  "Serialise an index scalar. In-memory keys are the house ':…' STRINGS; on disk they must be
  real EDN keywords, because `run_tests.clj` reads every .edn in the tree with `clojure.edn`
  and because a quoted key would read back as a different value than it was written from.
  `pr-str` cannot be used for the index: it collapses same-namespace maps to `#:index{…}`,
  which the minimal reader in `rasen.methods.kotoba` parses to the string \"#:index\" —
  silently, producing an index that is not a map at all."
  [v]
  (cond
    (nil? v) "nil"
    (and (string? v) (str/starts-with? v ":")) v
    (string? v) (str \" (str/replace (str/replace v "\\" "\\\\") "\"" "\\\"") \")
    :else (str v)))

(def ^:private index-key-order
  [":index/version" ":index/tx-count" ":index/datom-count" ":index/head-cid" ":index/shards"])
(def ^:private shard-key-order
  [":shard/seq" ":shard/path" ":shard/tx-count" ":shard/datom-count" ":shard/prev-cid"
   ":shard/first-cid" ":shard/last-cid"])

(defn- entry->edn [e]
  (str "{" (str/join " " (mapcat (fn [k] (when (contains? e k) [k (edn-scalar (get e k))]))
                                 shard-key-order)) "}"))

(defn index->edn
  "Deterministic EDN text for a shard index — key order fixed, so an unchanged index produces
  an unchanged file and a diff means the ledger actually moved."
  [index]
  (str "{" (str/join "\n "
                     (keep (fn [k]
                             (when (contains? index k)
                               (if (= k ":index/shards")
                                 (str k " [" (str/join "\n              "
                                                       (map entry->edn (get index k))) "]")
                                 (str k " " (edn-scalar (get index k))))))
                           index-key-order))
       "}"))

(defn shard-name [seq-n] (str "rasen.genome." (str/join (take-last 5 (str "00000" seq-n))) ".kotoba.edn"))

;; ── pure: what a well-formed index says ─────────────────────────────────────
(defn index-violations
  "Return a vector of violations in `index` (empty when internally consistent).

  Refuses to answer for a non-index: nil, a non-map, or a map with no :index/shards key is
  UNVERIFIED, and returning 'no violations' for it would make an absent index look like a
  clean one. Callers get an exception, never an empty vector."
  [index]
  (when-not (and (map? index) (contains? index ":index/shards"))
    (throw (ex-info "index-violations: not a shard index — cannot report clean"
                    {:got (if (map? index) (vec (keys index)) (type index))})))
  (let [shards (vec (get index ":index/shards"))
        v (transient [])]
    (when (not= index-version (get index ":index/version"))
      (conj! v {:violation :version/unknown :got (get index ":index/version")}))
    ;; contiguity: seq numbers must be 0,1,2,… with no gap. A gap is how a lost shard hides.
    (doseq [[i s] (map-indexed vector shards)]
      (when (not= i (get s ":shard/seq"))
        (conj! v {:violation :shard/out-of-sequence :at i :got (get s ":shard/seq")})))
    ;; the links between shards: shard n's prev-cid is shard n-1's last-cid
    (doseq [[a b] (partition 2 1 shards)]
      (when (not= (get a ":shard/last-cid") (get b ":shard/prev-cid"))
        (conj! v {:violation :shard/link-broken :between [(get a ":shard/seq") (get b ":shard/seq")]})))
    (when (and (seq shards) (not= "" (get (first shards) ":shard/prev-cid")))
      (conj! v {:violation :shard/first-not-genesis :got (get (first shards) ":shard/prev-cid")}))
    ;; the totals the index claims must equal the totals its own shards sum to
    (let [tx-sum (reduce + 0 (map #(or (get % ":shard/tx-count") 0) shards))
          dt-sum (reduce + 0 (map #(or (get % ":shard/datom-count") 0) shards))]
      (when (not= tx-sum (get index ":index/tx-count"))
        (conj! v {:violation :count/tx-mismatch :claimed (get index ":index/tx-count") :summed tx-sum}))
      (when (not= dt-sum (get index ":index/datom-count"))
        (conj! v {:violation :count/datom-mismatch :claimed (get index ":index/datom-count") :summed dt-sum})))
    (when (and (seq shards) (not= (get (last shards) ":shard/last-cid") (get index ":index/head-cid")))
      (conj! v {:violation :head/mismatch :claimed (get index ":index/head-cid")
                :summed (get (last shards) ":shard/last-cid")}))
    (persistent! v)))

(defn empty-index [] {":index/version" index-version ":index/tx-count" 0 ":index/datom-count" 0
                      ":index/head-cid" "" ":index/shards" []})

(defn shard-full?
  "Should a shard holding `txs`/`datoms` accept no more? Rolling happens BEFORE a tx is
  written, so a shard at capacity is full even if the incoming tx is tiny."
  ([tx-count datom-count] (shard-full? tx-count datom-count default-capacity))
  ([tx-count datom-count {:keys [max-txs max-datoms]}]
   (or (>= tx-count max-txs) (>= datom-count max-datoms))))

(defn summarise-shard
  "Pure: [seq-n prev-cid txs] → the index entry for a shard holding `txs` in order."
  [seq-n prev-cid txs]
  {":shard/seq" seq-n
   ":shard/path" (str "shards/" (shard-name seq-n))
   ":shard/tx-count" (count txs)
   ":shard/datom-count" (reduce + 0 (map #(count (get % ":tx/datoms")) txs))
   ":shard/prev-cid" prev-cid
   ":shard/first-cid" (get (first txs) ":tx/cid")
   ":shard/last-cid" (get (last txs) ":tx/cid")})

#?(:clj
   (do
     (def ^:private header
       (str ";; rasen 螺旋 — GENOME LEDGER SHARD (append-only, content-addressed EAVT commit-DAG). "
            "Generated; DO NOT hand-edit. One shard of a chain that continues across files — the "
            "shard index carries how many must exist. PUBLIC reference genetics + aggregate af "
            "only, never an individual-genotype registry. ADR-2606101000 / ADR-2609062000.\n"))

     (defn- index-path [root] (io/file root "rasen.genome.shards.edn"))
     (defn- shard-path [root seq-n] (io/file root "shards" (shard-name seq-n)))

     (defn read-index
       "Read the shard index, or nil when there is none. nil means UNVERIFIED — a caller must
       not read it as an empty ledger; `verify!` refuses on nil rather than reporting ok."
       [root]
       (let [f (index-path root)]
         (when (.exists f) (k/parse-edn (slurp f)))))

     (defn write-index! [root index]
       (let [f (index-path root)]
         (when-let [p (.getParentFile f)] (.mkdirs p))
         (spit f (str ";; rasen 螺旋 — GENOME LEDGER SHARD INDEX. Generated projection; rebuildable\n"
                      ";; from the shards by `reindex!`. The shards are canonical, this file is not.\n"
                      (index->edn index) "\n"))
         index))

     (defn read-shard
       "Read one shard's txs. Throws when the shard the index promised is absent — an
       unreadable shard must never look like an empty one."
       [root seq-n]
       (let [f (shard-path root seq-n)]
         (when-not (.exists f)
           (throw (ex-info "shard missing" {:seq seq-n :path (str f)})))
         (->> (str/split-lines (slurp f))
              (map str/trim)
              (remove #(or (empty? %) (str/starts-with? % ";")))
              (mapv k/parse-edn))))

     (defn head-cid
       "The chain head, from the index — O(1), no shard is read. Returns nil when there is no
       index (UNVERIFIED), and \"\" only for an index that genuinely holds no tx."
       [root]
       (when-let [ix (read-index root)] (get ix ":index/head-cid")))

     (defn append-txs!
       "Append `txs` (already chained, in order) to the sharded ledger under `root`, rolling
       shards at `capacity`. Returns the updated index.

       Refuses when the incoming run does not continue the existing head: appending a tx whose
       :tx/prev is not the current head would write a ledger that can never verify, and the
       write is the last moment at which that is cheap to say."
       ([root txs] (append-txs! root txs default-capacity))
       ([root txs capacity]
        (let [ix0 (or (read-index root) (empty-index))]
          (when (seq txs)
            (let [expect (get ix0 ":index/head-cid")
                  got (get (first txs) ":tx/prev")]
              (when (not= expect got)
                (throw (ex-info "append would break the chain"
                                {:head expect :incoming-prev got})))))
          (loop [ix ix0, remaining (seq txs)]
            (if-not remaining
              (write-index! root ix)
              (let [shards (vec (get ix ":index/shards"))
                    last-s (peek shards)
                    roll? (or (nil? last-s)
                              (shard-full? (get last-s ":shard/tx-count") (get last-s ":shard/datom-count") capacity))
                    seq-n (if roll? (count shards) (get last-s ":shard/seq"))
                    prev-cid (if roll? (get ix ":index/head-cid") (get last-s ":shard/prev-cid"))
                    f (shard-path root seq-n)
                    _ (when-let [p (.getParentFile f)] (.mkdirs p))
                    _ (when-not (.exists f) (spit f header))
                    tx (first remaining)
                    _ (spit f (str (k/tx->edn tx) "\n") :append true)
                    n-datoms (count (get tx ":tx/datoms"))
                    entry (if roll?
                            {":shard/seq" seq-n ":shard/path" (str "shards/" (shard-name seq-n))
                             ":shard/tx-count" 1 ":shard/datom-count" n-datoms
                             ":shard/prev-cid" prev-cid
                             ":shard/first-cid" (get tx ":tx/cid") ":shard/last-cid" (get tx ":tx/cid")}
                            (-> last-s
                                (update ":shard/tx-count" inc)
                                (update ":shard/datom-count" + n-datoms)
                                (assoc ":shard/last-cid" (get tx ":tx/cid"))))
                    shards' (if roll? (conj shards entry) (assoc shards (dec (count shards)) entry))]
                (recur (assoc ix ":index/shards" shards'
                              ":index/tx-count" (inc (get ix ":index/tx-count"))
                              ":index/datom-count" (+ (get ix ":index/datom-count") n-datoms)
                              ":index/head-cid" (get tx ":tx/cid"))
                       (next remaining))))))))

     (defn verify!
       "Walk the whole sharded ledger and report whether it is a single unbroken chain.

       Never reports ok for a ledger it could not read in full. The distinct outcomes are:
         :no-index        — nothing to verify; UNVERIFIED, not empty and not ok
         :index-invalid   — the index contradicts itself (violations attached)
         :shard-missing   — the index promised a shard that is not on disk
         :chain-broken    — a tx's cid or prev does not follow (:at gives the global tx number)
         :count-mismatch  — the chain read is not as long as the index says it must be
         :ok
       Shards are read one at a time; the whole corpus is never held at once."
       [root]
       (let [ix (read-index root)]
         (cond
           (nil? ix) {:ok false :reason :no-index}
           (seq (index-violations ix)) {:ok false :reason :index-invalid
                                        :violations (index-violations ix)}
           :else
           (let [n-shards (count (get ix ":index/shards"))]
             (loop [i 0, prev "", seen 0]
               (if (= i n-shards)
                 (if (= seen (get ix ":index/tx-count"))
                   {:ok true :tx-count seen :shards n-shards :head prev}
                   {:ok false :reason :count-mismatch :read seen :claimed (get ix ":index/tx-count")})
                 (let [txs (try (read-shard root i)
                                (catch clojure.lang.ExceptionInfo _ ::missing))]
                   (if (= txs ::missing)
                     {:ok false :reason :shard-missing :seq i}
                     (let [step (reduce (fn [{:keys [prev n]} tx]
                                          (let [expect (k/tx-cid (get tx ":tx/datoms") prev)]
                                            (if (or (not= (get tx ":tx/cid") expect)
                                                    (not= (get tx ":tx/prev") prev))
                                              (reduced {:broken-at n})
                                              {:prev (get tx ":tx/cid") :n (inc n)})))
                                        {:prev prev :n seen} txs)]
                       (if (:broken-at step)
                         {:ok false :reason :chain-broken :at (:broken-at step) :shard i}
                         (recur (inc i) (:prev step) (:n step)))))))))))) 

     (defn reindex!
       "Rebuild the index by walking the shards on disk, from shard 0 upward until one is
       absent. Refuses when shard 0 itself is absent: an empty directory must not produce an
       index claiming an empty-but-valid ledger."
       [root]
       (when-not (.exists (shard-path root 0))
         (throw (ex-info "reindex: no shard 0 — refusing to synthesise an empty index"
                         {:root (str root)})))
       (loop [i 0, prev "", entries [], tx-n 0, dt-n 0]
         (if-not (.exists (shard-path root i))
           (write-index! root {":index/version" index-version
                               ":index/tx-count" tx-n ":index/datom-count" dt-n
                               ":index/head-cid" prev ":index/shards" entries})
           (let [txs (read-shard root i)
                 e (summarise-shard i prev txs)]
             (recur (inc i) (get e ":shard/last-cid") (conj entries e)
                    (+ tx-n (get e ":shard/tx-count")) (+ dt-n (get e ":shard/datom-count")))))))

     (defn migrate!
       "Split an existing single-file ledger into shards under `root`, preserving every CID.

       Asserts the result rather than trusting it: the migrated head must equal the source
       head, and the migrated chain must verify. A migration that silently produced a
       different chain would be indistinguishable from a successful one."
       ([log-path root] (migrate! log-path root default-capacity))
       ([log-path root capacity]
        (let [src (k/read-log log-path)
              src-head (if (seq src) (get (last src) ":tx/cid") "")]
          (when (.exists (index-path root))
            (throw (ex-info "migrate: an index already exists — refusing to overwrite"
                            {:index (str (index-path root))})))
          (when (empty? src)
            (throw (ex-info "migrate: source ledger has no tx — nothing to migrate"
                            {:log (str log-path)})))
          (let [ix (append-txs! root src capacity)
                v (verify! root)]
            (when-not (:ok v)
              (throw (ex-info "migrate: migrated ledger does not verify" {:result v})))
            (when (not= src-head (get ix ":index/head-cid"))
              (throw (ex-info "migrate: head changed" {:was src-head :now (get ix ":index/head-cid")})))
            {:ok true :tx-count (get ix ":index/tx-count") :datom-count (get ix ":index/datom-count")
             :shards (count (get ix ":index/shards")) :head src-head}))))))
