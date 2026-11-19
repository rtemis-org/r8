# Conformance corpus

Language-neutral test cases that every R8 host runs: the R package, `rtemis-r8`'s own tests, and the Python and web bindings. A host passes when it produces exactly the expected result for every case of every operation it implements. Behavior that only one host has, such as the conversion of an R integer for `type_double()` at construction, is tested in that host's own suite.

## Files

- `corpus.schema.json`: the JSON Schema (draft 2020-12) of a suite file. A runner validates each file against it before running the cases.
- `<area>/<suite>.json`: one suite per file, grouped by area (`declare/`, `decode/`, `canonical/`, and `hash/`).
- `tools/reference-codec.mjs`: an independent JavaScript implementation of the canonical form and the content hash, written from the specification in `rust/crates/rtemis-r8/src/codec.rs`, which writes the expected results of the `canonical` and `hash` cases (`node conformance/tools/reference-codec.mjs <suite>...`).

A suite's `ir` is in canonical form, as `IrDocument::to_json()` writes it, compared as RFC 8785 text: the declaration hash reads that form. `R8_UPDATE_CORPUS=1 cargo test -p rtemis-r8 --all-features --test conformance`, run in `rust/`, rewrites it.

## Suite

```json
{
  "$corpus": 1,
  "description": "What the suite covers.",
  "ir": { "$ir": 1, "namespace": "ns", "types": [] },
  "cases": [ { "id": "duplicate-field", "op": "declare", "input": {}, "expect": {} } ]
}
```

- `$corpus`: the format version, 1.
- `ir`: an IR document, or an array of IR documents, one per namespace, that the value operations resolve types in; a suite of `declare` cases omits it.
- `cases`: each with a unique kebab-case `id`, an optional `description`, an `op`, an `input`, and an `expect`.

## Operations

| `op` | `input` | `type` | `expect` |
|---|---|---|---|
| `declare` | an IR document | none | `{"errors": [...]}`, or `{"parse_error": true}` when the document does not read |
| `decode` | a value's JSON document | the qualified type | `{"valid": true}`, or `{"errors": [...]}` |
| `canonical` | a value's JSON document | the qualified type | `{"json": "<exact text>"}`: the RFC 8785 canonical form, compared byte for byte |
| `hash` | a value's JSON document | the qualified type | `{"hash": "sha256:<hex>"}`; `options.hash_key` selects HMAC |

An error is `{"code": "...", "path": "...", "rule": "..."}`: the stable code, a JSON Pointer into `input`, and, for a rule violation, the rule ID. Messages are not compared. A host reports every error, and a runner compares the reported and expected lists as multisets, after sorting both by path, then code, then rule; an empty list means the input passes.

A host that does not implement an operation yet skips its cases and reports the number skipped. Operations are added to the table only; a new `op` does not change the meaning of an existing case.
