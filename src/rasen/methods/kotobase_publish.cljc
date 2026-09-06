(ns rasen.methods.kotobase-publish
  "kotobase_publish.cljc — rasen 螺旋 ledger → kotobase.net PROJECTION (ADR-2609062000).

  The ledger is canonical. kotobase.net carries the same datoms so they can be reached with
  Datalog, and it is deletable and rebuildable from the shards — the superproject's projection
  rule, and the reason nothing here writes back into the ledger.

  CARRIES NO TRANSPORT AND NO CREDENTIAL. Every write goes through an injected
  `(fn [tx-edn] -> result)`. `kotoba-lang/kotobase-client` is the canonical CACAO/Biscuit
  client for the tenant datom plane and is what a host should wire this to; this namespace
  exists so that rasen — which runs under bb, while that client is ClojureScript — does not
  grow a second one.

  WHAT THE WATERMARK MUST NOT DO. The obvious shape is: publish a batch, advance the
  watermark, repeat. Then a transport that returns nil, or an HTTP 202 that was never applied,
  advances the watermark exactly as far as a real acknowledgement does, and the next run starts
  after datoms that were never stored — a projection with a hole that no later run will fill.
  So `publish!` advances only on an acknowledgement it recognises, treats anything else as a
  stop, and reports how far it actually got.

  ENTITY IDENTITY IS THE TARGET'S PROPERTY, NOT OURS. Ledger entity ids are content-stable
  strings (`gene.brca1`, `en.var.rs334.located-in.gene.hbb`). Whether a target upserts on such
  an id or mints a fresh entity per transaction is a property of that target, and this
  namespace cannot verify it without a live endpoint. `publish!` therefore does not claim
  idempotence; `:entity-fn` is the seam where a host maps ids to whatever its target requires
  (a lookup ref, for instance). Measured 2026-09-06: unauthenticated POST /api/transact returns
  401, so this has NOT been checked against the live plane."
  (:require [clojure.string :as str]
            [rasen.methods.kotoba :as k]
            [rasen.methods.ledger-shards :as ls]
            #?(:clj [clojure.java.io :as io])))

;; ── pure: ledger datom → kotobase tx-data ───────────────────────────────────
(defn kw-or-string
  "House ':…' strings become real EDN keywords; everything else is left alone.
  The ledger stores `\":genome/kind\"` and `\":gene\"` as strings (the Python-derived
  convention); tx-data for a Datalog target needs the keywords those strings denote."
  [v]
  (if (and (string? v) (str/starts-with? v ":") (> (count v) 1))
    (keyword (subs v 1))
    v))

(defn datom->tx-form
  "One ledger datom [\":db/add\" e a v] → [:db/add e' a v'] with `entity-fn` applied to e.
  Refuses anything that is not a 4-element :db/add: the ledger holds only :db/add, so another
  shape means the caller handed us something that is not a ledger datom, and quietly dropping
  it would put a hole in the projection."
  [datom entity-fn]
  (when-not (and (sequential? datom) (= 4 (count datom)) (= ":db/add" (first datom)))
    (throw (ex-info "datom->tx-form: not a ledger :db/add datom" {:datom datom})))
  (let [[_ e a v] datom]
    [:db/add (entity-fn e) (kw-or-string a) (kw-or-string v)]))

(defn tx->tx-data
  "One ledger transaction → the tx-data vector for a Datalog target."
  ([tx] (tx->tx-data tx identity))
  ([tx entity-fn] (mapv #(datom->tx-form % entity-fn) (get tx ":tx/datoms"))))

(defn tx-data->edn
  "tx-data → the EDN text a transact call takes. `pr-str` is safe here (unlike for the shard
  index) because tx-data is a vector of vectors, never a same-namespace map."
  [tx-data]
  (pr-str tx-data))

(defn acknowledged?
  "Did the transport acknowledge the write? True only for a map-like result that says so.
  nil, false, an exception value, a bare string, or a map carrying :error are all NOT
  acknowledgements — the point of this predicate is that only a recognised success advances
  the watermark, so anything unrecognised must fall through to false."
  [result]
  (boolean
   (and (map? result)
        (not (contains? result :error))
        (not (contains? result "error"))
        (or (contains? result :commit) (contains? result "commit")
            (contains? result :db-after) (contains? result "db-after")
            (contains? result :tx-data) (contains? result "tx-data")
            (true? (:ok result))))))

(defn plan
  "Which ledger transactions still need publishing, given a watermark.
  Returns {:from-tx n :pending m}. A watermark ahead of the ledger is a contradiction — the
  projection cannot have seen more than exists — and is refused rather than clamped."
  [ledger-tx-count published-tx-count]
  (when (> published-tx-count ledger-tx-count)
    (throw (ex-info "plan: watermark is ahead of the ledger"
                    {:published published-tx-count :ledger ledger-tx-count})))
  {:from-tx published-tx-count :pending (- ledger-tx-count published-tx-count)})

#?(:clj
   (do
     (defn- watermark-path [root] (io/file root "kotobase.publish.edn"))

     (defn read-watermark
       "The projection's progress, or nil when there is none. nil is UNVERIFIED — it means no
       run has recorded anything here, not that the projection is empty and correct."
       [root]
       (let [f (watermark-path root)] (when (.exists f) (k/parse-edn (slurp f)))))

     (defn write-watermark! [root wm]
       (let [f (watermark-path root)]
         (when-let [p (.getParentFile f)] (.mkdirs p))
         (spit f (str ";; rasen 螺旋 — kotobase.net projection watermark. Generated; progress only.\n"
                      ";; The ledger is canonical; this records how much of it the projection has\n"
                      ";; ACKNOWLEDGED, never how much was sent.\n"
                      "{:published/tx-count " (get wm ":published/tx-count")
                      "\n :published/head " (pr-str (get wm ":published/head"))
                      "\n :published/ledger-head " (pr-str (get wm ":published/ledger-head")) "}\n"))
         wm))

     (defn publish!
       "Project the sharded ledger under `root` onto a Datalog target through `transact-fn`.

       opts:
         :transact-fn  (fn [tx-edn] -> result)   required
         :entity-fn    ledger entity id -> target entity id (default identity)
         :limit        stop after n transactions

       Returns {:status :published|:up-to-date|:stopped|:ledger-unverified, …}.

       The ledger is verified BEFORE anything is sent. Projecting a ledger that does not
       verify would put datoms on the target that no shard can account for, and the projection
       would then no longer be rebuildable — which is the one property that makes it safe to
       call a projection at all."
       [root {:keys [transact-fn entity-fn limit] :or {entity-fn identity}}]
       (when-not transact-fn (throw (ex-info "publish!: no :transact-fn" {})))
       (let [v (ls/verify! root)]
         (if-not (:ok v)
           {:status :ledger-unverified :verify v}
           (let [ix (ls/read-index root)
                 ledger-n (get ix ":index/tx-count")
                 wm (read-watermark root)
                 done (or (get wm ":published/tx-count") 0)
                 {:keys [from-tx pending]} (plan ledger-n done)]
             (if (zero? pending)
               {:status :up-to-date :published-tx done :ledger-tx ledger-n}
               (let [shards (get ix ":index/shards")
                     todo (cond->> (drop from-tx (mapcat #(ls/read-shard root (get % ":shard/seq"))
                                                         shards))
                            limit (take limit))]
                 ;; `attempted` counts writes handed to the transport; `acknowledged` counts the
                 ;; ones it confirmed. They differ by exactly the write that stopped the run, and
                 ;; reporting only one of them would hide which.
                 (loop [ts (seq todo), n done, attempted 0, acked 0,
                        last-head (get wm ":published/head")]
                   (if-not ts
                     (do (write-watermark! root {":published/tx-count" n
                                                 ":published/head" last-head
                                                 ":published/ledger-head" (get ix ":index/head-cid")})
                         {:status :published :published-tx n :ledger-tx ledger-n
                          :transactions-attempted attempted :transactions-acknowledged acked})
                     (let [tx (first ts)
                           result (transact-fn (tx-data->edn (tx->tx-data tx entity-fn)))]
                       (if (acknowledged? result)
                         (recur (next ts) (inc n) (inc attempted) (inc acked) (get tx ":tx/cid"))
                         (do (write-watermark! root {":published/tx-count" n
                                                     ":published/head" last-head
                                                     ":published/ledger-head" (get ix ":index/head-cid")})
                             {:status :stopped :published-tx n :ledger-tx ledger-n
                              :transactions-attempted (inc attempted)
                              :transactions-acknowledged acked
                              :stopped-at (get tx ":tx/cid")
                              :unacknowledged result})))))))))))))
