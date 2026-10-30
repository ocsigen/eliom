// Navigation: the links of the application change the page without loading a
// new document; history and reload work.
"use strict";
const { test, expect } = require("@playwright/test");

// The number of documents loaded in [page] since the call
function countLoads(page) {
  const loads = { count: 0 };
  page.on("load", () => loads.count++);
  return loads;
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
// document (Client.set_content).
test("a link of the application does not load a document", async ({
  page,
}) => {
  const loads = countLoads(page);
  await page.goto("/");
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await page.click("#to-two");
  await expect(page).toHaveURL(/\/two$/);
  await expect(page.locator("#title")).toHaveText("two");
  await expect(page.locator("#two-onload")).toHaveText("ran at load");
  expect(loads.count).toBe(1);
});

test("back and forward", async ({ page }) => {
  const loads = countLoads(page);
  await page.goto("/");
  await page.click("#to-two");
  await expect(page.locator("#title")).toHaveText("two");
  await page.goBack();
  await expect(page).toHaveURL(/\/$/);
  await expect(page.locator("#title")).toHaveText("main");
  await checkIncrement(page);
  await page.goForward();
  await expect(page).toHaveURL(/\/two$/);
  await expect(page.locator("#title")).toHaveText("two");
  expect(loads.count).toBe(1);
});

test("reload loads the page again", async ({ page }) => {
  const loads = countLoads(page);
  await page.goto("/");
  await page.click("#to-two");
  await expect(page.locator("#title")).toHaveText("two");
  await page.reload();
  await expect(page.locator("#title")).toHaveText("two");
  await expect(page.locator("#two-onload")).toHaveText("ran at load");
  expect(loads.count).toBe(2);
});

test("the page does not reload itself", async ({ page }) => {
  const loads = countLoads(page);
  await page.goto("/");
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await page.waitForTimeout(2000);
  expect(loads.count).toBe(1);
});

test("a link to a page outside the application loads it", async ({ page }) => {
  const loads = countLoads(page);
  await page.goto("/");
  await page.click("#to-server");
  await expect(page).toHaveURL(/\/server$/);
  await expect(page.locator("#title")).toHaveText("server");
  expect(loads.count).toBe(2);
  await page.goBack();
  await expect(page.locator("#title")).toHaveText("main");
  await checkIncrement(page);
});
