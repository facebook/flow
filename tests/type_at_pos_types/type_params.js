// @flow

// A type parameter is framed by its kind alone. It stands for whatever it is
// instantiated with, so unlike every other binder there is no type to annotate
// it with.

type Alias<A, B = string> = [A, B];
//         ^
type AliasDefault<A, B = string> = [A, B];
//                   ^
type AliasUse<A> = A;
//                 ^

type Bounded<A extends number> = A;
//           ^
type InBound<A extends number, B extends A> = [A, B];
//                                       ^
type Variance<out A> = () => A;
//                ^

function fn<Z>(z: Z): Z {
//          ^
  return z;
}

class Cls<C> {
//        ^
  m(): C {
    throw new Error();
  }
}

interface Iface<I> {
//              ^
  p: I;
}

type Infer<X> = X extends {a: infer Y} ? Y : empty;
//                                  ^
type InferUse<X> = X extends {a: infer Y} ? Y : empty;
//                                          ^
type Nested<X> = X extends infer A ? (A extends infer B ? [A, B] : empty) : empty;
//                                                         ^

// An `infer` binding is in scope in the true branch only, so this `Y` is the
// alias below and prints as one.
type Y = string;
type InferFalse<X> = X extends {a: infer Y} ? empty : Y;
//                                                    ^
type Mapped = {[K in 'a' | 'b']: number};
//              ^
type MappedUse = {[K in 'a' | 'b']: K};
//                                  ^

component Comp<P>(p: P) {
//             ^
  return null;
}

function outer<Outer>(value: Outer): Outer {
  function inner<Inner>(innerValue: Inner): Outer {
//               ^
    const innerResult: Inner = innerValue;
//                     ^
    const outerResult: Outer = value;
//                     ^
    return outerResult;
  }

  function shadow<Outer>(shadowValue: Outer): Outer {
//                                    ^
    return shadowValue;
  }

  return shadow(inner((value as any)));
}

type GenericFunction = <T>(value: T) => T;
//                      ^

// Anonymous declarations have no name for an `in ...` clause, so their
// parameters stay contextless.
const anonFn = function<AnonFn>(value: AnonFn): AnonFn {
//                      ^
  return value;
};
const anonArrow = <AnonArrow>(value: AnonArrow): AnonArrow => value;
//                 ^
