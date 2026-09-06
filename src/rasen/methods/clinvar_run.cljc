(ns rasen.methods.clinvar-run
  "clinvar_run.cljc — rasen 螺旋 G7 STREAMING runner for the ClinVar bulk release (ADR-2609062000).

  OUTWARD-GATED CELL (G7). `rasen.methods.clinvar` is the pure normaliser and takes parsed
  lines; this is the host I/O that produces them: a gzip stream (an HTTP body or a local file)
  is read line by line, normalised in batches, and appended to the sharded ledger as
  content-addressed transactions. The corpus is never held in memory — the whole point of the
  exercise is a 442 MB compressed input that expands to millions of rows.

  THE FAILURE THIS GUARDS AGAINST. A download that stops early decompresses cleanly up to the
  cut and then simply ends. Nothing in the loop distinguishes 'the file ended' from 'the
  connection ended', so a truncated corpus is written, the ledger verifies, and the counts look
  like a smaller-than-expected release rather than a failure. So `run!` refuses to report
  :complete unless it can show the stream ended where the source said it would: gzip's own
  trailer must validate (a truncated member raises rather than returning EOF), and when the
  caller supplies :expect-bytes the compressed bytes consumed must equal it. A run that cannot
  demonstrate either returns :truncated, never :complete.

  Resume is by row count against a source fingerprint. A checkpoint from a DIFFERENT release
  is refused rather than resumed: rows are not stable identifiers across releases, so resuming
  release B at release A's offset would silently skip real rows."
  (:require [clojure.string :as str]
            [rasen.methods.autorun :as auto]
            [rasen.methods.clinvar :as cv]
            [rasen.methods.kotoba :as k]
            [rasen.methods.ledger-shards :as ls]
            #?(:clj [clojure.java.io :as io])))

(def default-batch
  "Datoms per transaction. A tx is never split across shards, so this also bounds the smallest
  shard the ledger can have."
  {:max-datoms 20000})

;; ── pure: batching and progress ─────────────────────────────────────────────
(defn batch-full? [n-datoms {:keys [max-datoms]}] (>= n-datoms max-datoms))

(defn carry-kinds
  "Node kinds whose datoms are emitted once and then suppressed on later batches. Genes,
  phenotypes and populations repeat on nearly every row; variants do not, and a repeated
  :db/add of an identical [e a v] is a no-op on read anyway — the suppression is for volume,
  never for correctness."
  [] #{":gene" ":phenotype" ":population" ":pathway"})

(defn suppress-seen
  "Drop nodes already emitted in an earlier batch. Returns [graph' seen']."
  [graph seen]
  (let [kinds (carry-kinds)
        [nodes' seen']
        (reduce (fn [[m s] [nid n]]
                  (cond
                    (not (contains? kinds (get n ":genome/kind"))) [(assoc m nid n) s]
                    (contains? s nid) [m s]
                    :else [(assoc m nid n) (conj s nid)]))
                [{} seen] (:nodes graph))]
    [(assoc graph :nodes nodes') seen']))

(defn progress-report
  "The counts a run must be able to show. Kept separate from the run so a caller can assert on
  it without performing I/O."
  [{:keys [rows-read rows-kept rows-skipped datoms txs bytes-read]}]
  {:rows/read rows-read :rows/kept rows-kept :rows/skipped rows-skipped
   :ledger/datoms datoms :ledger/txs txs :source/bytes bytes-read})

(defn checkpoint-usable?
  "May `cp` be resumed against a source fingerprinted `fp`? Only when it is the same release.
  A checkpoint with no fingerprint is NOT usable: it was written before fingerprints existed
  and there is no way to tell which release it counted."
  [cp fp]
  (boolean (and (map? cp) (seq (get cp ":source/fingerprint"))
                (= (get cp ":source/fingerprint") fp))))

#?(:clj
   (do
     (defn- checkpoint-path [root] (io/file root "clinvar.checkpoint.edn"))

     (defn read-checkpoint [root]
       (let [f (checkpoint-path root)] (when (.exists f) (k/parse-edn (slurp f)))))

     (defn write-checkpoint! [root cp]
       (let [f (checkpoint-path root)]
         (when-let [p (.getParentFile f)] (.mkdirs p))
         (spit f (str ";; rasen 螺旋 — ClinVar streaming checkpoint. Generated; resume state only.\n"
                      "{:source/fingerprint " (pr-str (get cp ":source/fingerprint"))
                      "\n :rows/read " (get cp ":rows/read")
                      "\n :rows/kept " (get cp ":rows/kept")
                      "\n :rows/skipped " (get cp ":rows/skipped")
                      "\n :run/status " (get cp ":run/status") "}\n"))
         cp))

     (defn fetch!
       "Download `url` to `dest`. Returns {:path :bytes :declared :size-verified?}.

       Deliberately separate from `run!`: a full release takes long enough that holding one
       HTTP connection across the whole normalisation is its own failure mode.

       `:declared-bytes` is supplied by the CALLER, not read from a response header. babashka
       — which is what runs this repository — does not permit the HttpURLConnection header
       methods, and a size check that silently does nothing on the runtime we actually use
       would be worse than none. When no size is declared, :size-verified? is false and the
       caller must treat completeness as UNMEASURED rather than as verified."
       [url dest & {:keys [declared-bytes]}]
       (let [f (io/file dest)]
         (when-let [pp (.getParentFile f)] (.mkdirs pp))
         (with-open [in (io/input-stream (str url)) out (io/output-stream f)]
           (io/copy in out))
         (let [got (.length f)]
           (when (and declared-bytes (not= declared-bytes got))
             (throw (ex-info "fetch!: short read — the body is not the size that was declared"
                             {:reason :short-read :declared declared-bytes :got got :url (str url)})))
           {:path (str f) :bytes got :declared declared-bytes
            :size-verified? (boolean declared-bytes)})))

     (defn fingerprint-of-file
       "Content fingerprint of a downloaded release: \"sha256:<hex>\".

       Content, not Last-Modified. A resume offset counts ROWS, and rows are only meaningful
       against the exact bytes they were counted in; a server timestamp can repeat, be absent,
       or change without the content changing. Streams the file — a release is hundreds of
       megabytes."
       [path]
       (let [md (java.security.MessageDigest/getInstance "SHA-256")
             buf (byte-array 65536)]
         (with-open [in (io/input-stream (io/file (str path)))]
           (loop []
             (let [n (.read in buf)]
               (when (pos? n) (.update md buf 0 n) (recur)))))
         (str "sha256:" (apply str (map #(format "%02x" (bit-and (int %) 0xff)) (.digest md))))))

     (defn open-gzip-lines
       "A reader over a gzipped LOCAL file. A URL is refused: `run!` must be able to say how
       many bytes it consumed, and the honest way to know that is to have the file."
       [src]
       (when (re-find #"^https?://" (str src))
         (throw (ex-info "open-gzip-lines: fetch! the release to a local file first"
                         {:source (str src)})))
       (let [f (io/file (str src))]
         (when-not (.exists f)
           (throw (ex-info "open-gzip-lines: no such source" {:source (str src)})))
         [(io/reader (java.util.zip.GZIPInputStream. (io/input-stream f) 65536) :encoding "UTF-8")
          (.length f)]))

     (defn run!
       "Stream ClinVar into the sharded ledger under `root`.

       opts:
         :source        LOCAL path of variant_summary.txt.gz (use `fetch!` first)
         :fingerprint   opaque string identifying the release (Last-Modified + size).
                        Required for resume; without it a previous checkpoint is refused.
         :expect-bytes  compressed size the release declared. When given, a source file of a
                        different size returns :truncated.
         :assembly      default \"GRCh38\"
         :batch         {:max-datoms n}
         :limit         stop after n data rows (bounded runs and tests)
         :resume?       continue from the checkpoint when it matches :fingerprint

       Returns {:status :complete|:truncated|:limited, …counts}. :complete is the only status
       that asserts the whole release was seen."
       [root {:keys [source fingerprint expect-bytes assembly batch limit resume?]
              :or {assembly "GRCh38" batch default-batch}}]
       (when-not source (throw (ex-info "run!: no :source" {})))
       (let [cp (read-checkpoint root)
             skip-rows (if (and resume? (checkpoint-usable? cp fingerprint))
                         (get cp ":rows/read") 0)]
         (when (and resume? cp (pos? (or (get cp ":rows/read") 0))
                    (not (checkpoint-usable? cp fingerprint)))
           (throw (ex-info "run!: checkpoint is from a different release — refusing to resume"
                           {:checkpoint (get cp ":source/fingerprint") :now fingerprint})))
         (let [[rdr src-bytes] (open-gzip-lines source)]
           (try
             (with-open [r rdr]
               (let [lines (line-seq r)
                     idx (cv/header-index (first lines))
                     head (ls/head-cid root)
                     st (atom {:rows-read 0 :rows-kept 0 :rows-skipped 0
                               :datoms 0 :txs 0 :prev (or head "") :seen #{}
                               :pending [] :pending-n 0 :limited? false})
                     flush!
                     (fn [force?]
                       (let [{:keys [pending pending-n prev txs datoms]} @st]
                         (when (and (seq pending) (or force? (batch-full? pending-n batch)))
                           (let [tx (k/make-tx (vec pending) (str "clinvar-" txs) "stream" prev)]
                             (ls/append-txs! root [tx])
                             (swap! st assoc :prev (get tx ":tx/cid") :pending [] :pending-n 0
                                    :txs (inc txs) :datoms (+ datoms pending-n))))))]
                 (doseq [line (rest lines)
                         :while (not (:limited? @st))]
                   (let [n (:rows-read @st)]
                     (swap! st update :rows-read inc)
                     (when (>= n skip-rows)
                       (let [row (str/split line #"\t" -1)
                             g (cv/row->graph row idx assembly)]
                         (if (nil? g)
                           (swap! st update :rows-skipped inc)
                           (let [[g' seen'] (suppress-seen g (:seen @st))
                                 ds (auto/ground-datoms-from
                                     {:nodes (:nodes g') :edges (:edges g')})]
                             (swap! st (fn [s] (-> s
                                                   (update :rows-kept inc)
                                                   (assoc :seen seen')
                                                   (update :pending into ds)
                                                   (update :pending-n + (count ds)))))
                             (flush! false))))
                       (when (and limit (>= (inc (:rows-kept @st)) limit))
                         (swap! st assoc :limited? true)))))
                 (flush! true)
                 (let [{:keys [rows-read rows-kept rows-skipped datoms txs limited?]} @st
                       bytes-read src-bytes
                       status (cond
                                limited? :limited
                                (and expect-bytes (not= expect-bytes bytes-read)) :truncated
                                :else :complete)
                       result (assoc (progress-report {:rows-read rows-read :rows-kept rows-kept
                                                       :rows-skipped rows-skipped :datoms datoms
                                                       :txs txs :bytes-read bytes-read})
                                     :status status
                                     :head (ls/head-cid root))]
                   (write-checkpoint! root {":source/fingerprint" (or fingerprint "")
                                            ":rows/read" rows-read ":rows/kept" rows-kept
                                            ":rows/skipped" rows-skipped ":run/status" status})
                   (when (and expect-bytes (not= expect-bytes bytes-read))
                     (assoc result :expected-bytes expect-bytes))
                   result)))
             (catch java.io.EOFException e
               ;; gzip refused its own trailer: the source was cut mid-member.
               (write-checkpoint! root {":source/fingerprint" (or fingerprint "")
                                        ":rows/read" 0 ":rows/kept" 0 ":rows/skipped" 0
                                        ":run/status" :truncated})
               (throw (ex-info "run!: source stream ended mid-gzip — refusing to report a corpus"
                               {:reason :truncated} e)))
             (catch java.util.zip.ZipException e
               (throw (ex-info "run!: gzip stream is corrupt — refusing to report a corpus"
                               {:reason :corrupt} e)))))))))
