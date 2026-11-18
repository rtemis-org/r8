// reference-codec.mjs
// An independent implementation of R8's canonical JSON form, canonical binary
// form, declaration hash, and content hash, written from the specification in
// crates/r8-core/src/codec.rs. It fills the `expect` of every `canonical` and
// `hash` case of the suites given, which the Rust runner then compares with
// its own results.
//
// Run: node conformance/tools/reference-codec.mjs conformance/canonical/*.json conformance/hash/*.json

import { readFileSync, writeFileSync } from "node:fs";
import { createHash } from "node:crypto";

// RFC 8785: JSON.stringify for scalars, members sorted by UTF-16 code units.
const jcs = (v) => {
  if (Array.isArray(v)) return "[" + v.map(jcs).join(",") + "]";
  if (v !== null && typeof v === "object") {
    return "{" + Object.keys(v).sort().map((k) => JSON.stringify(k) + ":" + jcs(v[k])).join(",") + "}";
  }
  return JSON.stringify(v);
};
const sha256 = (bytes) => "sha256:" + createHash("sha256").update(bytes).digest("hex");

class Bytes {
  constructor() { this.parts = []; }
  raw(arr) { this.parts.push(Buffer.from(arr)); }
  ascii(s) { this.parts.push(Buffer.from(s, "ascii")); }
  u8(x) { this.raw([x]); }
  u32(x) { const b = Buffer.alloc(4); b.writeUInt32LE(x); this.parts.push(b); }
  i32(x) { const b = Buffer.alloc(4); b.writeInt32LE(x); this.parts.push(b); }
  u64(x) { const b = Buffer.alloc(8); b.writeBigUInt64LE(BigInt(x)); this.parts.push(b); }
  i64(x) { const b = Buffer.alloc(8); b.writeBigInt64LE(BigInt(x)); this.parts.push(b); }
  f64(x) {
    const b = Buffer.alloc(8);
    if (Number.isNaN(x)) b.writeBigUInt64LE(0x7ff8000000000000n);
    else b.writeDoubleLE(x === 0 ? 0 : x);
    this.parts.push(b);
  }
  str(s) { const u = Buffer.from(s, "utf8"); this.u64(u.length); this.parts.push(u); }
  done() { return Buffer.concat(this.parts); }
}

const days = (s) => Math.round(Date.UTC(+s.slice(0, 4), +s.slice(5, 7) - 1, +s.slice(8, 10)) / 86400000);
const micros = (s) => {
  const secs = days(s) * 86400 + +s.slice(11, 13) * 3600 + +s.slice(14, 16) * 60 + +s.slice(17, 19);
  return BigInt(secs) * 1000000n + BigInt(+s.slice(20, 26));
};
const num = (v) => (v === "Infinity" ? Infinity : v === "-Infinity" ? -Infinity : v === "NaN" ? NaN : v);

const unwrap = (t) => (t.kind === "option" ? t.inner : t);
const TAG = { logical: 1, integer: 2, double: 3, character: 4, date: 5, posixct: 6, factor: 7 };
const tagOf = (t) => TAG[unwrap(t).kind] ?? 0;

function bitmap(out, items) {
  const b = new Array(Math.ceil(items.length / 8)).fill(0);
  items.forEach((x, i) => { if (x !== null) b[i >> 3] |= 1 << (i & 7); });
  out.raw(b);
}

function value(out, tag, v) {
  const present = v !== null;
  switch (tag) {
    case 1: out.u8(present && v === true ? 1 : 0); break;
    case 2: out.i32(present ? v : 0); break;
    case 3: out.f64(present ? num(v) : 0); break;
    case 4: out.str(present ? v : ""); break;
    case 5: out.i32(present ? days(v) : 0); break;
    case 6: out.i64(present ? micros(v) : 0n); break;
  }
}

function dense(element, v) {
  const out = new Bytes(), tag = tagOf(element);
  out.ascii("R8M1"); out.u8(tag); out.u32(v.shape.length);
  v.shape.forEach((d) => out.u64(d));
  bitmap(out, v.data);
  v.data.forEach((x) => value(out, tag, x));
  v.shape.forEach((_, i) => {
    const names = v.dimnames?.[i];
    if (names) { out.u8(1); names.forEach((n) => out.str(n)); } else out.u8(0);
  });
  return out.done();
}

function sparse(element, v) {
  const out = new Bytes(), tag = tagOf(element);
  out.ascii("R8S1"); out.u8(tag); out.u64(v.shape[0]); out.u64(v.shape[1]); out.u64(v.data.length);
  v.indptr.forEach((x) => out.u64(x)); v.indices.forEach((x) => out.u64(x));
  v.data.forEach((x) => value(out, tag, x));
  return out.done();
}

function table(columns, v) {
  const out = new Bytes();
  const names = Object.keys(v).filter((k) => !k.startsWith("$"))
    .sort((a, b) => Buffer.compare(Buffer.from(a, "utf8"), Buffer.from(b, "utf8")));
  out.ascii("R8T1"); out.u64(v.$length); out.u32(names.length);
  for (const name of names) {
    const t = columns.find((c) => c.name === name)?.type ?? v.$columns?.[name] ?? { kind: "json" };
    const tag = tagOf(t), cells = v[name];
    out.str(name); out.u8(tag);
    if (tag === 7) {
      const declared = unwrap(t).levels;
      const levels = Array.isArray(cells) ? declared : cells.levels;
      const codes = Array.isArray(cells) ? cells.map((x) => (x === null ? null : declared.indexOf(x) + 1)) : cells.codes;
      out.u64(levels.length); levels.forEach((l) => out.str(l));
      bitmap(out, codes);
      codes.forEach((c) => out.i32(c ?? 0));
      continue;
    }
    bitmap(out, cells);
    cells.forEach((x) => value(out, tag, x));
  }
  return out.done();
}

function makeLookup(docs) {
  return (q) => {
    const [ns, name] = q.split("::");
    return docs.find((d) => d.namespace === ns)?.types?.find((t) => t.name === name);
  };
}

function normalizer(find, digest) {
  const fields = (defs, v) => Object.fromEntries(defs.map((f) => {
    const x = v?.[f.name];
    return [f.name, x === undefined || x === null ? (f.default !== undefined ? val(f.type, f.default) : null) : val(f.type, x)];
  }));
  const named = (q, v) => {
    const t = find(q);
    if (t.kind === "struct") return fields(t.fields, v);
    if (t.variants.every((x) => x.shape === "unit")) return v;
    const tag = t.tag ?? "type", variant = t.variants.find((x) => x.name === v[tag]);
    const defs = variant.shape === "struct" ? variant.fields : variant.shape === "newtype" ? find(variant.of).fields : [];
    return { ...fields(defs, v), [tag]: v[tag] };
  };
  const val = (t, v) => {
    if (v === null) return null;
    switch (t.kind) {
      case "option": return val(t.inner, v);
      case "matrix": case "array": return digest ? { $digest: sha256(dense(t.element, v)) } : v;
      case "sparse_matrix": return digest ? { $digest: sha256(sparse(t.element, v)) } : v;
      case "table": return digest ? { $digest: sha256(table(t.columns, v)) } : v;
      case "vec": return v.map((x) => val(t.element, x));
      case "hashmap": return Object.fromEntries(Object.entries(v).map(([k, x]) => [k, val(t.value, x)]));
      case "ordered_map": return v.map((p) => ({ name: p.name, value: val(t.value, p.value) }));
      case "named": return named(t.name, v);
      default: return v;
    }
  };
  return named;
}

// Declaration hash: every reachable named type, descriptions emptied and
// extensions removed, in the IR's canonical form.
function declarationHash(q, find) {
  const seen = {};
  const strip = (fs) => fs.forEach((f) => {
    if ("description" in f) f.description = "";
    delete f.extensions;
    const t = f.type.kind === "option" ? f.type.inner : f.type;
    if (t.kind === "table") strip(t.columns);
  });
  const reach = (name) => {
    if (name in seen) return;
    const t = find(name);
    if (!t) { seen[name] = null; return; }
    const c = structuredClone(t);
    if ("description" in c) c.description = "";
    delete c.extensions;
    if (c.kind === "struct") strip(c.fields);
    else c.variants.forEach((x) => { if ("description" in x) x.description = ""; if (x.shape === "struct") strip(x.fields); });
    seen[name] = c;
    const refs = [];
    const walk = (ty) => {
      switch (ty.kind) {
        case "named": refs.push(ty.name); break;
        case "matrix_of": case "table_of": refs.push(ty.of); break;
        case "option": walk(ty.inner); break;
        case "vec": case "matrix": case "array": case "sparse_matrix": walk(ty.element); break;
        case "hashmap": case "ordered_map": walk(ty.value); break;
        case "table": ty.columns.forEach((col) => walk(col.type)); break;
      }
    };
    if (t.kind === "struct") t.fields.forEach((f) => walk(f.type));
    else t.variants.forEach((x) => { if (x.shape === "struct") x.fields.forEach((f) => walk(f.type)); if (x.shape === "newtype") refs.push(x.of); });
    refs.forEach(reach);
  };
  reach(q);
  return sha256(jcs(seen));
}

for (const path of process.argv.slice(2)) {
  const suite = JSON.parse(readFileSync(path, "utf8"));
  const docs = Array.isArray(suite.ir) ? suite.ir : [suite.ir];
  const find = makeLookup(docs);
  for (const c of suite.cases) {
    if (c.op === "canonical") c.expect = { json: jcs(normalizer(find, false)(c.type, c.input)) };
    if (c.op === "hash") {
      const obj = { type: c.type, declaration: declarationHash(c.type, find), value: normalizer(find, true)(c.type, c.input) };
      c.expect = { hash: sha256(jcs(obj)) };
    }
  }
  writeFileSync(path, JSON.stringify(suite, null, 2) + "\n");
  console.log(path, suite.cases.length, "cases");
}
