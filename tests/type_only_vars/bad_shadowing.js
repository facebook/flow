/**
 * @flow
 */

import typeof A from "./A.js";
import type {Foo, Bar as Baz} from "./A.js";

type duck = {
  quack: () => string;
}

// After the TS-style two-namespace split, type-only imports/aliases live in the
// type namespace and `var` lives in the value namespace, so these now coexist
// without error.
var A: string = "Hello";
var Foo: string = "Goodbye";
var Baz: string = "Go away please";

// Same here: the typedef is in the type namespace, `var` is in the value
// namespace, so no conflict.
var duck: string = "quack";
