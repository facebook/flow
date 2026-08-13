// @flow

// `this` type guards on methods of generic classes.

import {
  GenericBase as GenericBaseImported,
  GenericDerived as GenericDerivedImported,
} from './this_class_exports';

function classTypeParams() {
  class C<X> {
    // The class type parameter flows into the guard type.
    isD(): this is D<X> {
      return this instanceof D; // okay
    }

    isDUnchecked(): this is D<X> {
      return true; // error C ~> D
    }

    // A concrete type argument that does not match what the body establishes.
    isDString(): this is D<string> {
      return this.isDNumber(); // error twice: number ~> string, and negation
    }

    isDNumber(): implies this is D<number> {
      return this instanceof D; // okay
    }

    x: X;
  }

  class D<X> extends C<X> {}

  declare const c: C<number>;
  if (c.isD()) {
    c as C<number>; // okay
    c as D<number>; // okay
    c as D<string>; // error number ~> string
  } else {
    c as D<number>; // error
  }
}

function methodTypeParams() {
  class C<X> {
    isD1<Y>(): this is D<Y> {
      return this instanceof D; // okay
    }

    isD2<Y>(y: Y): this is D<Y> {
      return this instanceof D; // okay
    }

    // The guard type is an unbounded method type parameter.
    isY<Y>(): this is Y {
      return true; // error C ~> Y
    }
  }

  class D<X> extends C<X> {}

  declare const c: C<number>;
  if (c.isD1<string>()) {
    c as D<string>; // okay
    c as D<number>; // error string ~> number
  }
  if (c.isD2(1)) {
    c as D<number>; // okay
  }
}

function variance() {
  // The guard type of a checked guard sits in invariant position, so an
  // override cannot change it even along a variance-compatible direction.
  class C<out X> {
    isD(): this is D<X> {
      return this instanceof D;
    }
  }
  class D<out X> extends C<X> {}
  class E<out X> extends D<X> {}

  class Bad<out X> extends C<X> {
    isD(): this is E<X> { // error type guard is in invariant position
      return this instanceof E;
    }
  }

  class Implies<out X> extends C<X> {
    isD(): implies this is E<X> { // error twice: one-sided vs default guard, and D ~> E
      return this instanceof E;
    }
  }
}

function bounded() {
  class C<X extends {...}> {
    isD(): this is D<X> {
      return this instanceof D; // okay
    }
  }
  class D<X extends {...}> extends C<X> {}

  declare const c: C<{a: number}>;
  if (c.isD()) {
    c as D<{a: number}>; // okay
  }
}

function crossFile() {
  declare const g: GenericBaseImported<number>;
  if (g.isGenericDerived()) {
    g as GenericDerivedImported<number>; // okay
    g as GenericDerivedImported<string>; // error number ~> string
  }
}
