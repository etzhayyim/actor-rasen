(ns rasen.tools.build
  "rasen 螺旋 — cljc-native WASM component build (babashka). ADR-2606261200 supersedes the
  componentize-py path: the actor is built from methods/*.cljc (the shared runtime) via

      methods/*.cljc → cherry(JS) → ComponentizeJS(jco) → WASI Component

  (the Clojure analogue of componentize-py; cherry's cljs.core ≈ CPython, StarlingMonkey the
  embedded JS engine). The WIT world (wasm/wit/world.wit) is UNCHANGED — only the impl language
  under the analyze/datoms/coverage contract changed. OPERATOR STEP. Requires node/npx
  (cherry-cljs, esbuild, @bytecodealliance/jco), wasm-tools, ipfs. Returns the built CID string."
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def ^:private random-shim
  ;; ComponentizeJS pre-initializes the module with Wizer; cljs.core's init must not reach the
  ;; wasi:random import during the snapshot. The actor's analyze/datoms/coverage are fully
  ;; deterministic, so a pure-JS deterministic random is correct here (ADR-2606261200).
  (str "const __s=new Uint8Array([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]);"
       "if(!globalThis.crypto)globalThis.crypto={};"
       "globalThis.crypto.getRandomValues=(a)=>{const u=new Uint8Array(a.buffer||a);"
       "for(let i=0;i<u.length;i++)u[i]=__s[i%16];return a;};"
       "Math.random=()=>0.42;\n"))

(def actor-dir (-> *file* fs/parent fs/parent str))

(defn build
  "Run the full cljc-native build; returns the CID (string). actor-dir defaults to the actor root."
  ([] (build actor-dir))
  ([actor-dir]
   (let [wasm      (fs/file actor-dir "wasm")
         methods   (fs/file actor-dir "methods")
         seed-file (fs/file actor-dir "data" "seed-genome-graph.kotoba.edn")
         bdir      (fs/file wasm "build")
         astage    (fs/file bdir "rasen")
         opts      {:dir (str bdir) :inherit true}]
     ;; 1. stage the cljc ns tree: rasen.methods.* + rasen.app + the embedded seed (rasen.seed)
     (fs/delete-tree bdir)
     (fs/create-dirs (fs/file astage "methods"))
     (doseq [m ["analyze" "datom_emit" "coverage_report"]]
       (fs/copy (fs/file methods (str m ".cljc"))
                (fs/file astage "methods" (str m ".cljc")) {:replace-existing true}))
     (fs/copy (fs/file wasm "app.cljs") (fs/file astage "app.cljs") {:replace-existing true})
     (spit (fs/file astage "seed.cljs")
           (str "(ns rasen.seed)\n(def edn " (json/generate-string (slurp seed-file)) ")\n"))
     ;; 2. cherry compile each ns → ESM. cherry bundles cljs.core, so the methods' full
     ;;    clojure.core runs unchanged; install cherry-cljs so esbuild resolves its imports.
     (shell opts "npm" "i" "--no-save" "--no-fund" "--no-audit" "cherry-cljs")
     (doseq [f ["rasen/methods/analyze.cljc" "rasen/methods/datom_emit.cljc"
                "rasen/methods/coverage_report.cljc" "rasen/seed.cljs" "rasen/app.cljs"]]
       (shell opts "npx" "-y" "cherry-cljs" "compile" f))
     ;; 3. esbuild bundle (cherry emits dotted-ns module specifiers → alias each to its .mjs)
     (shell opts "npx" "-y" "esbuild" "rasen/app.mjs" "--bundle" "--format=esm"
            "--outfile=app.bundle.js"
            "--alias:rasen.methods.analyze=./rasen/methods/analyze.mjs"
            "--alias:rasen.methods.datom-emit=./rasen/methods/datom_emit.mjs"
            "--alias:rasen.methods.coverage-report=./rasen/methods/coverage_report.mjs"
            "--alias:rasen.seed=./rasen/seed.mjs")
     ;; 4. offline sanity — the three exports must run before we componentize.
     (shell opts "node" "--input-type=module" "-e"
            (str "import * as m from './app.bundle.js';"
                 "const r=JSON.parse(m.analyze());"
                 "if(!(r.care&&r.burden&&r.pleiotropy)) throw new Error('analyze shape: '+JSON.stringify(r));"
                 "m.datoms(1); m.coverage();"
                 "console.log('cljs sanity OK — care',r.care.length,'burden',r.burden.length,'pleiotropy',r.pleiotropy.length);"))
     ;; 5. prepend the Wizer-safe deterministic random shim, then componentize against the
     ;;    UNCHANGED WIT world (analyze/datoms/coverage).
     (spit (fs/file bdir "app.shimmed.js")
           (str random-shim (slurp (fs/file bdir "app.bundle.js"))))
     (shell opts "npx" "-y" "@bytecodealliance/jco" "componentize" "app.shimmed.js"
            "--wit" "../wit/world.wit" "--world-name" "rasen-actor" "-o" "../rasen-actor.wasm")
     (shell {:dir (str wasm) :inherit true} "wasm-tools" "validate" "rasen-actor.wasm")
     ;; 6. content-address (CIDv1, matches `ipfs add --cid-version=1`)
     (let [cid  (-> (shell {:dir (str wasm) :out :string}
                           "ipfs" "add" "-Q" "--only-hash" "--cid-version=1" "rasen-actor.wasm")
                    :out str/trim)
           size (fs/size (fs/file wasm "rasen-actor.wasm"))]
       (println (format "rasen-actor.wasm  %d bytes  CID=%s" size cid))
       (println "cljc-native (cherry + ComponentizeJS, ADR-2606261200). If the CID changed, re-record")
       (println "  the pinned wasmCid in the actor profile/did json at operator publish time.")
       cid))))

(defn -main [& _]
  (build)
  (System/exit 0))
