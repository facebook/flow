// @flow

import { foo } from "./lib-1";

module.exports = foo(); // error: C is not exported
