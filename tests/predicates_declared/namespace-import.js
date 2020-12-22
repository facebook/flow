// @flow
import * as t from "./namespace-import-lib.js";

let a: ?number = 1;
if (!t.isNil(a)) {
  (a: number);
} else {
  (a: number); // error
}
