// Top-level `import X = require(...)` in a libdef is illegal — it would
// create a RemoteBinding in the Global scope and crash pack_builtin with
// `Failure("unexpected remote builtin")`. Now silently ignored, matching
// the existing behavior for top-level `import` statements in libdefs.

declare module "lib_foo" {
  declare export var x: number;
}

import F = require("lib_foo"); // ERROR
