// Raw HTML (Unsafe.data) gives the same nodes whether the server or the
// client builds the page.
"use strict";
const { test, expect } = require("@playwright/test");
const { open } = require("./helpers");

test("raw HTML built by the client", async ({ page }) => {
  const errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  await open(page, "/raw-html");
  expect(await page.evaluate(() => window.e2eRawScripts)).toBe(1);
  await page.click("#build");
  await expect(page.locator("#client-react")).toHaveText("1");
  const server = page.locator("#server-raw");
  const client = page.locator("#client-raw");
  // The contents of the elements: the client gives the attributes of svg in
  // another order
  const contents = (l) =>
    l.locator(":scope > *").evaluateAll((es) => es.map((e) => e.innerHTML));
  expect(await contents(client)).toEqual(await contents(server));
  await expect(client.locator("b").first()).toHaveText("bold");
  // innerHTML does not show the namespace, nor whether a script contains
  // elements
  expect(await client.locator("circle").evaluate((e) => e.namespaceURI)).toBe(
    "http://www.w3.org/2000/svg",
  );
  await expect(client.locator("script *")).toHaveCount(0);
  expect(await page.evaluate(() => window.e2eRawScripts)).toBe(2);
  await expect(page.locator("#client-top b")).toHaveText("top");
  await expect(page.locator("#client-top i")).toHaveText("two");
  await expect(page.locator("#client-table td")).toHaveText("row");
  await expect(page.locator("#client-injected b")).toHaveText("injected");
  await expect(page.locator("#client-d")).toBeEmpty();
  await expect(page.locator("#client-react b")).toHaveText("1");
  expect(errors).toEqual([]);
});
