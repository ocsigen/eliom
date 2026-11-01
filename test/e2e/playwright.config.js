// Browser tests of Eliom. Build the application with
// `dune build @test/e2e/app/all`, then run `npx playwright test` here.
// E2E_BROWSERS lists the browsers (chromium by default; firefox, webkit).
// Each browser runs the tests with the JavaScript client program (server on
// port E2E_PORT, 8473 by default), and with the WebAssembly one (project
// <browser>-wasm, server on port E2E_PORT + 1). A server keeps its logs and
// persistent data in server-data/js or server-data/wasm, emptied when it
// starts.
"use strict";
const path = require("path");
const { defineConfig, devices } = require("@playwright/test");

const port = Number(process.env.E2E_PORT || 8473);
// DUNE_BUILD_DIR is relative to the root of the repository, as for dune
const app = path.join(
  path.resolve(__dirname, "..", "..", process.env.DUNE_BUILD_DIR || "_build"),
  "default",
  "test",
  "e2e",
  "app",
);
const data = path.join(__dirname, "server-data");
const browsers = {
  chromium: "Desktop Chrome",
  firefox: "Desktop Firefox",
  webkit: "Desktop Safari",
};
const backends = [
  { suffix: "", server: "js", backend: "js_of_ocaml", port },
  { suffix: "-wasm", server: "wasm", backend: "wasm_of_ocaml", port: port + 1 },
];

const device = (name) => {
  if (!Object.hasOwn(browsers, name))
    throw new Error(
      `E2E_BROWSERS: unknown browser ${JSON.stringify(name)} (${Object.keys(browsers).join(", ")})`,
    );
  return devices[browsers[name]];
};

// The arguments, quoted for the shell
const sh = (...args) =>
  args.map((s) => `'${String(s).replace(/'/g, `'\\''`)}'`).join(" ");

module.exports = defineConfig({
  testDir: ".",
  forbidOnly: !!process.env.CI,
  retries: 0,
  projects: (process.env.E2E_BROWSERS || "chromium")
    .split(",")
    .flatMap((name) =>
      backends.map((b) => ({
        name: name + b.suffix,
        use: {
          ...device(name),
          baseURL: `http://127.0.0.1:${b.port}`,
          // Lets the tests run the garbage collector (window.gc)
          ...(name === "chromium"
            ? { launchOptions: { args: ["--js-flags=--expose-gc"] } }
            : {}),
        },
        metadata: { backend: b.backend },
      })),
    ),
  webServer: backends.map((b) => {
    const dir = path.join(data, b.server);
    return {
      command: `rm -rf ${sh(dir)} && mkdir -p ${sh(dir)} && exec ${sh(path.join(app, "server.exe"), b.port, path.join(app, "client"), dir, b.server)}`,
      url: `http://127.0.0.1:${b.port}/`,
      reuseExistingServer: false,
    };
  }),
});
