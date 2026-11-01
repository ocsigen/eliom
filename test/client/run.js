// node run.js FILE...: loads each file in turn, as the scripts of a page.
"use strict";
const path = require("path");
for (const file of process.argv.slice(2)) require(path.resolve(file));
