/* @flow */

class C {
  static p: string;
}
C.p = 'hi';

// Class static fields are compatible with object types
C as {p: string}; // ok
C as {p: number}; // errors, string ~> number & vice versa (unify)

declare var o: {p: number};
o as Class<C>; // error, object type incompatible with class type

class Dup1 {
  static x: string;
  // $FlowExpectedError[duplicate-class-member]
  static x() {}
}
Dup1.x as empty; // function ~> empty

class Dup2 {
  static x() {}
  // $FlowExpectedError[duplicate-class-member]
  static x: string;
}
Dup2.x as empty; // string ~> empty
