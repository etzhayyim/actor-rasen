# rasen 螺旋

Public and aggregate genetics graph actor. This repository is the standalone
`actor-rasen` west project; EDN is canonical and JSON is permitted only
as external wire fixtures under `wire/`.

Canonical repository: `etzhayyim/actor-rasen`.

Within the Tamaki artificial organism, rasen is the public aggregate-genetics
observation organ. It may integrate reference evidence and population-level
frequencies, but it cannot store individual genotypes, diagnose or rate a
person, or support insurance, employment, forensic, or eugenic decisions.
Network ingest and publication remain Council- and operator-gated.

```bash
kbb -M:test
kbb -m rasen.methods.analyze
kbb -m rasen.methods.datom-emit
kbb -m rasen.methods.coverage-report
kbb -m rasen.methods.ingest --offline --no-pin
```

Canonical actor metadata is in `actor.edn`, `identity.edn`, `manifest.edn`, and
`dependencies.edn`. The actor-owned vocabulary is
`schema/genome-ontology.edn`; executable code is under `src/`, tests under
`test/`, and the CLJS wasmCloud component under `wasm/`.

Network ingest and publication remain outward-gated. The actor contains no
individual genotype registry and persists only public aggregate reference data.
