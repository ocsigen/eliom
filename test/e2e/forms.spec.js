// Forms submitted by the client program (XHR) send what the browser sends
// when it submits them natively (~xhr:false): the fields, the button that
// submits the form, the point of an image button. A button that overrides the
// action of its form makes the browser submit it natively.
"use strict";
const { test, expect } = require("@playwright/test");
const { open, countLoads } = require("./helpers");

const modes = [
  { name: "XHR", path: "forms", loads: 1 },
  { name: "native", path: "native", loads: 2 },
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
    "requestSubmit",
    async (p) => {
      await p.click("#decoy");
      await p.$eval("#main-form", (f) => f.requestSubmit());
    },
    "t=hello b=[] n=[]",
  ],
  [
    "click on an image button",
    (p) => p.click("#img", { position: { x: 5, y: 7 } }),
    "t=image img=5,7",
  ],
  ["Enter with an image button", (p) => p.press("#it", "Enter"), "t=image img=0,0"],
];

for (const meth of ["get", "post"]) {
  for (const mode of modes) {
    test.describe(`${meth.toUpperCase()}, ${mode.name}`, () => {
      for (const [name, submit, received] of cases) {
        test(name, async ({ page }) => {
          const loads = countLoads(page);
          await open(page, `/${mode.path}/${meth}`);
          await submit(page);
          await expect(page.locator("#received")).toHaveText(received);
          expect(loads.count).toBe(mode.loads);
        });
      }

      test("button with formaction", async ({ page }) => {
        const loads = countLoads(page);
        await open(page, `/${mode.path}/${meth}`);
        await page.click("#elsewhere");
        await expect(page).toHaveURL(/\/echo\?/);
        await expect(page.locator("#received")).toHaveText("t=hello b=[e] n=[]");
        expect(loads.count).toBe(2);
      });
    });
  }
}
