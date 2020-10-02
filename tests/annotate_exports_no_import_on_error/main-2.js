// @flow

import { foo, bar } from "./lib-2";

module.exports._1 = foo(); // error: C is not exported
module.exports._2 = 0 < 1 ? foo() : bar(); // error: C is not exported, even though D is okay
module.exports._3 = bar(); // okay
