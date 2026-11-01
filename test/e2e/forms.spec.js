// Forms submitted by the client program (XHR) send what the browser sends
// when it submits them natively (~xhr:false): the fields, the button that
// submits the form, the point of an image button. A button that overrides the
// action of its form makes the browser submit it natively.
"use strict";
const { test, expect } = require("@playwright/test");
const { open, markDocument, sameDocument } = require("./helpers");

const modes = [
  { name: "XHR", path: "forms", keepsDocument: true },
  { name: "native", path: "native", keepsDocument: false },
];

// [submit] submits a form of the page; [received] is what the server receives
const cases = [
  ["button B", (p) => p.click("#b"), "t=hello b=[b] n=[]"],
  ["Enter in a field", (p) => p.press("#t", "Enter"), "t=hello b=[a] n=[]"],
  [
    "Enter after a click on a button that does not submit",
    async (p) => {
      await p.click("#decoy");
      await p.press("#t", "Enter");
    },
    "t=hello b=[a] n=[]",
  ],
  ["input of type submit", (p) => p.click("#c"), "t=hello b=[c] n=[]"],
  ["label of a button", (p) => p.click("#d-label"), "t=hello b=[d] n=[]"],
  [
    "requestSubmit after a click on a button that does not submit",
    async (p) => {
      await p.click("#decoy");
      await p.$eval("#main-form", (f) => f.requestSubmit());
    },
    "t=hello b=[] n=[]",
  ],
  [
    "Enter with an image button",
    (p) => p.press("#it", "Enter"),
    "t=image img=0,0",
  ],
];

for (const meth of ["get", "post"]) {
  for (const mode of modes) {
    test.describe(`${meth.toUpperCase()}, ${mode.name}`, () => {
      for (const [name, submit, received] of cases) {
        test(name, async ({ page }) => {
          await open(page, `/${mode.path}/${meth}`);
          await markDocument(page);
          await submit(page);
          await expect(page.locator("#received")).toHaveText(received);
          expect(await sameDocument(page)).toBe(mode.keepsDocument);
        });
      }

      test("button with formaction", async ({ page }) => {
        await open(page, `/${mode.path}/${meth}`);
        await markDocument(page);
        await page.click("#elsewhere");
        await expect(page).toHaveURL(/\/elsewhere\?/);
        await expect(page.locator("#received")).toHaveText(
          "t=hello b=[e] n=[]",
        );
        expect(await sameDocument(page)).toBe(false);
      });
    });
  }

  // The point of an image button depends on how the browser rounds the position
  // of the click: the client program sends the point that the browser sends
  test(`${meth.toUpperCase()}, click on an image button`, async ({ page }) => {
    const point = async (mode) => {
      await open(page, `/${mode.path}/${meth}`);
      await markDocument(page);
      await page.click("#img", { position: { x: 5, y: 7 } });
      const received = page.locator("#received");
      await expect(received).toHaveText(/^t=image img=[4-6],[6-8]$/);
      expect(await sameDocument(page)).toBe(mode.keepsDocument);
      return received.textContent();
    };
    const [xhr, native] = modes;
    expect(await point(xhr)).toBe(await point(native));
  });
}
