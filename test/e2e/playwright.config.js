// Browser tests of Eliom. Build the application with
// `dune build @test/e2e/app/all`, then run `npx playwright test` here.
// E2E_BROWSERS lists the browsers (chromium by default; firefox, webkit).
"use strict";
const path = require("path");
const { defineConfig, devices } = require("@playwright/test");

const port = Number(process.env.E2E_PORT || 8473);
const app = path.join(
  __dirname,
  "..",
  "..",
  process.env.DUNE_BUILD_DIR || "_build",
  "default",
  "test",
  "e2e",
  "app",
);
const browsers = {
  chromium: "Desktop Chrome",
  firefox: "Desktop Firefox",
  webkit: "Desktop Safari",
};

module.exports = defineConfig({
  testDir: ".",
  forbidOnly: !!process.env.CI,
  retries: 0,
  use: { baseURL: `http://127.0.0.1:${port}` },
  projects: (process.env.E2E_BROWSERS || "chromium")
    .split(",")
    .map((name) => ({ name, use: { ...devices[browsers[name]] } })),
  webServer: {
    // A new data directory for each run
    command: `d=$(mktemp -d) && exec ${path.join(app, "server.exe")} ${port} ${path.join(app, "static")} "$d"`,
    url: `http://127.0.0.1:${port}/`,
    reuseExistingServer: false,
  },
});
