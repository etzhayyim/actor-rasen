# rasen 螺旋

Public and aggregate genetics graph actor. This repository is the standalone
`com-etzhayyim-rasen` west project; EDN is canonical and JSON is permitted only
as external wire fixtures under `wire/`.

```bash
bb test
bb -m rasen.methods.analyze
bb -m rasen.methods.datom-emit
bb -m rasen.methods.coverage-report
bb -m rasen.methods.ingest --offline --no-pin
```

Canonical actor metadata is in `actor.edn`, `identity.edn`, `manifest.edn`, and
`dependencies.edn`. The actor-owned vocabulary is
`schema/genome-ontology.edn`; executable code is under `src/`, tests under
`test/`, and the CLJS wasmCloud component under `wasm/`.

Network ingest and publication remain outward-gated. The actor contains no
individual genotype registry and persists only public aggregate reference data.
