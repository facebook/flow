/**
 * Client imports some but not all modules,
 * triggering/suppressing parse errors.
 * @flow
 */

// non-flow files should give parse errors if they're imported
var A = require("Foo");          // non-Flow file @providesModule Foo
var B = require("./NoProvides"); // non-Flow file
