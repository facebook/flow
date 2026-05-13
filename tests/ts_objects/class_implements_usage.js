// @flow
// .js consumer of a .ts-declared object-typed shape. The relaxation is
// keyed on the consumer file's extension, so this must still error with
// `[cannot-implement]` (or equivalent).

import type {Shape} from "./class_implements_lib";

class JsImpl implements Shape { // ERROR: cannot-implement in .js
  a: number = 1;
  b: string = "x";
}
