// @flow

// `this` type guards are supported on non-static methods of classes and
// interfaces, but not on record methods. See tests/type_guards/this_classes.js.

class B {}

{
  record R {
    a: number,

    isB(): this is B { // error `this` type guards are not supported on records
      return true;
    }

    static isBStatic(): this is B { // error also rejected on static record methods
      return true;
    }
  }

  declare const r: R;
  if (r.isB()) {
    // TODO should not refine. The guard is still recorded despite the error,
    // the same way it is for object literal methods (see tests/type_guards/this.js).
    r as B;
  }
}
