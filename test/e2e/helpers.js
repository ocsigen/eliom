// Helpers of the browser tests
"use strict";

// Waits until the client program has handled the document loaded in [page]
// (not a page changed through an XHR, which keeps the flag of the previous
// one): before, a link or a form is handled natively by the browser, and the
// events of the page are not handled.
async function ready(page) {
  await page.waitForFunction(() => window.e2eReady === true);
}

// Opens [path] in [page] and waits until the client program has handled it
async function open(page, path) {
  await page.goto(path);
  await ready(page);
}

// Runs the garbage collector of [page] when the browser lets the tests do it
// (Chromium, see playwright.config.js): a React value that nothing keeps
// alive then stops being updated.
async function collectGarbage(page) {
  await page.evaluate(() => window.gc?.());
}

// Marks the document shown in [page], for [sameDocument]
async function markDocument(page) {
  await page.evaluate(() => {
    window.e2eDocument = true;
  });
}

// Whether [page] still shows the document marked by [markDocument]: a new
// document does not have the mark, a page loaded through an XHR keeps it
function sameDocument(page) {
  return page.evaluate(() => window.e2eDocument === true);
}

module.exports = { ready, open, collectGarbage, markDocument, sameDocument };
