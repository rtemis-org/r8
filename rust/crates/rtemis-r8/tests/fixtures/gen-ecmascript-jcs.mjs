// Run: node tests/fixtures/gen-ecmascript-jcs.mjs tests/fixtures/ecmascript-jcs.json
// Generates expected RFC 8785 output with JavaScript, whose JSON.stringify and
// Number.prototype.toString RFC 8785 adopts.
import { writeFileSync } from "node:fs";
const out = process.argv[2];
const buf = new DataView(new ArrayBuffer(8));
const bits = (x) => { buf.setFloat64(0, x); return buf.getBigUint64(0).toString(16).padStart(16, "0"); };
const edge = [0, -0, 1, -1, 0.1, 0.5, 1/3, 1e21, 1e20, 123456789012345680000, 1e-7, 1e-6, 0.000001, 5e-324, -5e-324,
  1.7976931348623157e308, 2.2250738585072014e-308, 9007199254740992, 9007199254740993, 4.5e15, 333333333.3333333,
  295147905179352830000, 1e22, 1.5e-9, 100, 1e2, 12345.6789, 0.30000000000000004, 2**-1074, 2**53 - 1];
let seed = 42n;
const rand64 = () => { seed = (seed * 6364136223846793005n + 1442695040888963407n) & ((1n << 64n) - 1n); return seed; };
const numbers = edge.map((x) => [bits(x), String(x)]);
for (let i = 0; i < 2000; i++) {
  buf.setBigUint64(0, rand64());
  const x = buf.getFloat64(0);
  if (Number.isFinite(x)) numbers.push([bits(x), String(x)]);
}
const canon = (v) => {
  if (Array.isArray(v)) return "[" + v.map(canon).join(",") + "]";
  if (v !== null && typeof v === "object") {
    const keys = Object.keys(v).sort(); // UTF-16 code unit order
    return "{" + keys.map((k) => JSON.stringify(k) + ":" + canon(v[k])).join(",") + "}";
  }
  return JSON.stringify(v);
};
const docs = [
  {"\u{10000}": 1, "\u{fffd}": 2, "a": 3, "B": 4, "": 5, "é": 6},
  {"s": "tab\tnewline\nquote\"backslash\\ctrl\u0001\u001f del\u007f é   \u{1F600}"},
  {"n": [1, 1.0, -0, 1e21, 0.1, 1e-7], "nested": {"z": null, "y": true, "x": [false, {"b": 2, "a": 1}]}},
];
writeFileSync(out, JSON.stringify({ numbers, documents: docs.map((d) => [JSON.stringify(d), canon(d)]) }, null, 0) + "\n");
console.log(numbers.length, "numbers,", docs.length, "documents");
