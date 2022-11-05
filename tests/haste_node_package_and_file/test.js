// @flow

/**
 * In this test, we have a Haste package (package/package.json), and a file
 * named package.js which does not provide a Haste module named "package".
 *
 * This is a regression test: we had a bug where we'd know that "package" in
 * `import ... from "package"` refers to Haste package <PROJECT_ROOT>/package,
 * but then incorrectly resolve that to /package.js instead of /package/index.js.
 *
 * This is the Haste equivalent of the Node module resolution LOAD_NODE_MODULE
 * step (https://nodejs.org/api/modules.html#all-together). If it doesn't begin
 * with ./ or ../ or /, then it must be a folder with a package.json.
 */

import pkg from "package";
(pkg: empty); // error
