// @flow

var x = 42;
x++;
(++x: empty); // err
(x: empty); // err
x += 42;
(x: empty); // err
x -= 42;
(x: empty); // err


{
  // Changing type
  let x: boolean = false;
  let y: boolean = false;
  let z: ?boolean = false;
  x &&= 3; // Error: number not assignable to boolean
  y ||= 3; // Error: number not assignable to boolean
  z ??= 3; // Error: number not assignable to boolean
}

{
  // Nullable refinements
  class A {}

  declare function expectNullable(null): A;

  let x: A | null = null;
  x ??= (x: null);
}

{
  declare var invariant: any;
  // Flow does not understand LHS's nullability/truthiness/falseyness and the statement always throws.
  class A {}
  let x: null = null;
  let y: A = new A();

  function alwaysThrows1(): number { x ??= invariant(false); } // Error
  function alwaysThrows2(): number { y &&= invariant(false); } // Error
  function alwaysThrows3(): number { x ||= invariant(false); } // Error
}
