/**
 * Client imports some but not all modules,
 * triggering/suppressing parse errors.
 * @flow
 */

// non-flow files should not give parse errors
var A = require("./Imported");          // non-Flow file

var B = require("./ParseError");        // Flow file
