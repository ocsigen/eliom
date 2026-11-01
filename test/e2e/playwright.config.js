// Browser tests of Eliom. Build the application with
// `dune build @test/e2e/app/all`, then run `npx playwright test` here.
// E2E_BROWSERS lists the browsers (chromium by default; firefox, webkit).
// The server keeps its logs and persistent data in server-data/, emptied when
// it starts.
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
  use: { baseURL: `http://127.0.0.1:${port}` },
  projects: (process.env.E2E_BROWSERS || "chromium")
    .split(",")
    .map((name) => ({
      name,
      use: {
        ...device(name),
        // Lets the tests run the garbage collector (window.gc)
        ...(name === "chromium"
          ? { launchOptions: { args: ["--js-flags=--expose-gc"] } }
          : {}),
      },
    })),
  webServer: {
    command: `rm -rf ${sh(data)} && mkdir -p ${sh(data)} && exec ${sh(path.join(app, "server.exe"), port, path.join(app, "static"), data)}`,
    url: `http://127.0.0.1:${port}/`,
    reuseExistingServer: false,
  },
});
