// @flow
// .js consumer of a .ts-declared object-typed shape. The imported shape
// retains TypeScript's structural implements semantics.

import type {Shape} from "./class_implements_lib";

class JsImpl implements Shape { // OK: Shape comes from .ts
  a: number = 1;
  b: string = "x";
}
