// A minimal browser for the client tests under Node.js: only what the
// initialisation of an Eliom client program uses, at the URL of the page of
// the test application. The load event never fires, so the code that runs at
// load is not tested here.
"use strict";
const url = new URL("http://eliom-test/page");
globalThis.location = {
  href: url.href,
  protocol: url.protocol,
  host: url.host,
  hostname: url.hostname,
  port: url.port,
  pathname: url.pathname,
  search: url.search,
  hash: url.hash,
};
globalThis.history = { state: null, pushState() {}, replaceState() {} };
globalThis.addEventListener = () => {};
globalThis.removeEventListener = () => {};
function createElement(tagName) {
  return {
    tagName: tagName.toUpperCase(),
    childNodes: [],
    appendChild(child) {
      this.childNodes.push(child);
      return child;
    },
  };
}
const head = createElement("head");
globalThis.document = {
  readyState: "loading",
  head,
  createElement,
  querySelector: (selector) => (selector === "head" ? head : null),
  addEventListener() {},
  removeEventListener() {},
};
