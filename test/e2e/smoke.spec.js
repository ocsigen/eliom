// The page of the application: the client program starts, reads an injected
// value, runs code at load, and handles events on nodes of the server.
"use strict";
const { test, expect } = require("@playwright/test");

test("the client program runs in the page", async ({ page }) => {
  const errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  page.on("console", (m) => {
    if (m.type() === "error") errors.push(m.text());
  });
  await page.goto("/");
  await expect(page.locator("#injection")).toHaveText("sent by the server");
  await expect(page.locator("#onload")).toHaveText("ran at load");
  await page.click("#increment");
  await page.click("#increment");
  await expect(page.locator("#counter")).toHaveText("2");
  expect(errors).toEqual([]);
});
