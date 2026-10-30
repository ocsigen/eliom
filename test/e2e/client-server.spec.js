// Client and server: server functions, a bus, React events and signals of the
// server, and the state of the tabs and of the session.
"use strict";
const { test, expect } = require("@playwright/test");
const { open } = require("./helpers");

// The bus and the counter of the server are shared by the tests
test.describe.configure({ mode: "serial" });

// Clicks the button [id] of the page, which shows the result of its action
async function run(page, id, result) {
  await page.click(`#${id}`);
  await expect(page.locator(`#${id}-result`)).toHaveText(result);
}

test("server function", async ({ page }) => {
  await open(page, "/client-server");
  await run(page, "double", "42");
});

test("exception in a server function", async ({ page }) => {
  await open(page, "/client-server");
  await run(page, "fail", "exception on the server");
});

test("server function returning a client value", async ({ page }) => {
  await open(page, "/client-server");
  await run(page, "client-value", "made by a client value");
});

// [f a b] runs with the page of the application open in two browsers
async function inTwoBrowsers(browser, f) {
  const contexts = [await browser.newContext(), await browser.newContext()];
  try {
    const pages = await Promise.all(contexts.map((c) => c.newPage()));
    for (const page of pages) await open(page, "/client-server");
    await f(...pages);
  } finally {
    for (const context of contexts) await context.close();
  }
}

test("bus between two browsers", async ({ browser }) => {
  await inTwoBrowsers(browser, async (a, b) => {
    await run(a, "bus-write", "sent");
    await expect(b.locator("#bus")).toHaveText("on the bus");
  });
});

test("signal of the server in two browsers", async ({ browser }) => {
  await inTwoBrowsers(browser, async (a, b) => {
    const counter = b.locator("#counter-down");
    await expect(counter).toHaveText(/^\d+$/);
    const before = Number(await counter.textContent());
    await run(a, "increment", "sent");
    await expect(counter).toHaveText(String(before + 1));
  });
});

test("event sent up to the server", async ({ page }) => {
  await open(page, "/client-server");
  await run(page, "up", "sent");
  await page.reload();
  await expect(page.locator("#last-up")).toHaveText("sent up");
});

// An action answers without content (204): Client.call_service, which returns
// the content of the answer, fails with Failed_request 204, which the callers
// that expect an action catch, as Bus.write does.
test("action of the server", async ({ page }) => {
  await open(page, "/client-server");
  await run(page, "action", "Eliom.Request.Failed_request(204)");
});

test("state of the tabs and of the session", async ({ context }) => {
  const [one, two] = [await context.newPage(), await context.newPage()];
  await open(one, "/client-server");
  await open(two, "/client-server");
  await run(one, "count", "tab 1, session 1");
  await run(one, "count", "tab 2, session 2");
  await run(two, "count", "tab 1, session 3");
});
