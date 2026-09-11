# rasen 螺旋 — kotoba cljc-native WASM actor (cherry+ComponentizeJS (ADR-2606261200))

Design for running rasen's analyzer as a **kotoba cljc-native WASM actor** under the "one Worker, many
WASM actors" model (ADR-2606014500 / 2606014600), exactly as inochi 命 does. The only
first-party Cloudflare Worker is `etzhayyim.com` (identity / `did.json`); the actor itself is
a **content-addressed WASM component** fetched from IPFS and run **locally** (browser via
ameno, or a donated mesh node via e7m-wasm-runner) — there is **no per-actor server**
(no-server-key).

## Why pywasm fits rasen

rasen's methods are **pure-stdlib Python (no numpy)** precisely so they compile to a WASM
Component via **cherry+ComponentizeJS (ADR-2606261200)** (the watatsuna pattern, ADR-2606014600). The edge-primary
care-priority is a graph integral over `:en/grasping-load` × disclosed `:en/clinsig` — no
native BLAS, no heavy deps. The same code runs:

- as a CLI cell (`kbb -m rasen.methods.analyze`) on a mesh node, and
- in-WASM in the browser (ameno) with **zero server trust** — the reader recomputes the
  component CID and compares it to the DID-doc CID before executing.

## Component ABI (WIT sketch)

```wit
package etzhayyim:rasen@0.1.0;

world rasen-actor {
  /// run the analyzer over an embedded :representative public-genetics graph
  /// (G1: no individual genotypes, aggregate frequencies only).
  /// returns JSON: { care:[{id,symbol,taxon,score}], burden:[...], pleiotropy:[...] }
  export analyze: func() -> string;

  /// emit the kotoba Datom log (EAVT) for the embedded graph as EDN text.
  export datoms: func(tx: u32) -> string;

  /// honest coverage report (markdown).
  export coverage: func() -> string;
}
```

`analyze.py` / `datom_emit.py` / `coverage_report.py` become the three export bodies; the
embedded seed is bundled read-only into the component (no filesystem at runtime).

## Build & verify (target)

```bash
cherry+ComponentizeJS (ADR-2606261200) -w rasen-actor componentize actor -o dist/rasen.wasm
ipfs add --cid-version=1 --raw-leaves dist/rasen.wasm > dist/rasen.cid
node ../../tsumugi/wasm/loader/verify.mjs dist/rasen.wasm   # reuse headless CID-verify path
```

The resulting CID is advertised in the actor's `did.json` as an `EtzhayyimWasmComponent`
service (`ipfs://<cid>`), issued dynamically by the apex Worker (ADR-2606013800) from
`:actor/wasm-cid` in the kotoba `actors-v1` graph.

## Trust model

- **No server key.** The component is read-only; it never signs. Identity is the actor's own
  `did:key` + the content-addressed DID doc (ADR-2606015600).
- **Integrity before execution.** ameno / e7m recompute the CID and refuse on mismatch.
- **G1 holds in WASM too.** The embedded seed carries **no individual genotypes** and only
  population-aggregate frequencies; the component cannot leak what it does not contain.

## Status

**Build-ready.** The world (`wit/world.wit`), the export bodies (`app.py`), the build script
(`build.sh` — embed seed → cherry+ComponentizeJS (ADR-2606261200) → CID → DID-doc `EtzhayyimWasmComponent` service),
and CI coverage (`tests/test_wasm.cljc`, 4 tests) are all in place; the export bodies run in dev
mode (`python3 app.py {analyze|datoms|coverage}`). The only remaining step is the
**cherry+ComponentizeJS (ADR-2606261200) build itself** (operator/build-env toolchain — not a runtime dep), which
emits `dist/rasen.wasm` + `dist/rasen.cid` + `did-service.json` for advertisement in the actor
`did.json` (ADR-2606013800/2606014500). G1 holds in WASM — the component embeds only the
bounded PUBLIC seed.
