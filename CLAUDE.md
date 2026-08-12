# actor-rasen repository rules

- This is an independent flat-path west repository.
- EDN is canonical for identity, manifests, dependencies, schema, state, and
  generated provenance. JSON is allowed only in `wire/` for protocol fixtures.
- Keep Clojure/ClojureScript implementation in `src/`, tests in `test/`, and
  wasmCloud CLJS/WIT sources in `wasm/`.
- Do not reintroduce Go, TinyGo, Python wasm glue, shell launchers, or JSON-LD.
- Run `bb test` before publishing changes.
- Public network ingest and IPFS/IPNS publication are G7 operator-gated.
