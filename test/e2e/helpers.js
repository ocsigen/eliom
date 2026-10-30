// Helpers of the browser tests
"use strict";

// Opens [path] in [page] and waits until the client program has handled the
// loaded page: before, a link or a form is handled natively by the browser.
async function open(page, path) {
  await page.goto(path);
  await page.waitForFunction(() => window.e2eReady === true);
}

// The number of documents loaded in [page] since the call: a page loaded
// through an XHR does not count
function countLoads(page) {
  const loads = { count: 0 };
  page.on("load", () => loads.count++);
  return loads;
}

module.exports = { open, countLoads };
