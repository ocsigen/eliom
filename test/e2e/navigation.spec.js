// Navigation: the links of the application change the page without loading a
// new document; history and reload work.
"use strict";
const { test, expect } = require("@playwright/test");

// Marks the document shown in [page], for [sameDocument]
async function markDocument(page) {
  await page.evaluate(() => {
    window.e2eDocument = true;
  });
}

// Whether [page] still shows the document marked by [markDocument]: a new
// document does not have the mark
function sameDocument(page) {
  return page.evaluate(() => window.e2eDocument === true);
}

// Clicks the button of the main page and checks that its counter changes:
// the client program handles the events of the page
async function checkIncrement(page) {
  const counter = page.locator("#counter");
  const before = Number(await counter.textContent());
  await page.click("#increment");
  await expect(counter).toHaveText(String(before + 1));
}

// The page comes from the server through an XHR, and replaces the content of the
// document (Client.set_content); the code of the new page runs at load.
test("the links of the application do not load a document", async ({
  page,
}) => {
  await page.goto("/");
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await markDocument(page);
  await page.click("#to-two");
  await expect(page).toHaveURL(/\/two$/);
  await expect(page.locator("#title")).toHaveText("two");
  await expect(page.locator("#two-onload")).toHaveText("ran at load");
  await page.click("#to-main");
  await expect(page).toHaveURL(/\/$/);
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await checkIncrement(page);
  expect(await sameDocument(page)).toBe(true);
});

test("back and forward", async ({ page }) => {
  await page.goto("/");
  await markDocument(page);
  await page.click("#to-two");
  await expect(page.locator("#title")).toHaveText("two");
  await page.goBack();
  await expect(page).toHaveURL(/\/$/);
  await expect(page.locator("#title")).toHaveText("main");
  expect(await sameDocument(page)).toBe(true);
  await checkIncrement(page);
  await page.goForward();
  await expect(page).toHaveURL(/\/two$/);
  await expect(page.locator("#title")).toHaveText("two");
  expect(await sameDocument(page)).toBe(true);
});

test("reload loads the page again", async ({ page }) => {
  await page.goto("/");
  await page.click("#to-two");
  await expect(page.locator("#title")).toHaveText("two");
  await markDocument(page);
  await page.reload();
  await expect(page.locator("#title")).toHaveText("two");
  await expect(page.locator("#two-onload")).toHaveText("ran at load");
  expect(await sameDocument(page)).toBe(false);
});

// A general check: the main page opens no Comet channel, unlike os_template
// where an answer of Comet made the page reload itself
test("the page does not reload itself", async ({ page }) => {
  await page.goto("/");
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await markDocument(page);
  await page.waitForTimeout(2000);
  expect(await sameDocument(page)).toBe(true);
});

test("a link to a page outside the application loads it", async ({ page }) => {
  await page.goto("/");
  await markDocument(page);
  await page.click("#to-server");
  await expect(page).toHaveURL(/\/server$/);
  await expect(page.locator("#title")).toHaveText("server");
  expect(await sameDocument(page)).toBe(false);
  await page.goBack();
  await expect(page.locator("#title")).toHaveText("main");
  await checkIncrement(page);
});
