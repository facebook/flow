/**
 * Client imports some but not all modules,
 * triggering/suppressing parse errors.
 * @flow
 */

// non-flow files should not show parse errors
var A = require("Provides");          // non-Flow haste file
var B = require("./NoProvides"); // non-Flow file

var C = require("./ParseError"); // Flow file
