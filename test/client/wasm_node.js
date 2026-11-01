// Programs compiled by wasm_of_ocaml need WebAssembly GC: Node.js 22 or later.
"use strict";
const major = Number(process.versions.node.split(".")[0]);
if (major < 22) {
  console.error(
    `The WebAssembly tests need Node.js 22 or later, not ${process.version}.`,
  );
  process.exit(1);
}
