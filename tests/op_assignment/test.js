// @flow

declare function invariant(boolean): empty;

///////////
// Valid //
///////////
{
  // Let binding
  let x = 1;
  x += 2; // Ok
  x -= 2; // Ok
  x *= 2; // Ok
  x /= 2; // Ok
  x **= 2; // Ok
  x %= 2; // Ok
  x &&= 42; // Ok
  x ||= 42; // Ok
  x ??= 1; // Ok

  let sx = "a";
  sx += "b"; // Ok
}

{
  // Regular object
  const o: {|p: number|} = {p: 1};
  o.p += 2; // Ok
  o.p -= 2; // Ok
  o.p *= 2; // Ok
  o.p /= 2; // Ok
  o.p **= 2; // Ok
  o.p %= 2; // Ok
  o.p &&= 2; // Ok
  o.p ||= 2; // Ok
  o.p ??= 2; // Ok
}

{
  // Truthy falsey refinements
  class A {}

  declare function expectNullable(null): A;

  let x: A | null = null;
  x &&= (x: A);
  let y: A | null = null;
  y ||= expectNullable(y);
  (y: A)
}

{
  // ??= refinement after statement
  let v: ?number;
  v ??= 3;
  (v: number)

  const o: {|p: ?number|} = {p: null};
  o.p ??= 3;
  (o.p: number)
}

////////////
// Errors //
////////////
{
  // Const binding
  const x = 1;
  x += 2; // Error: cannot reassign constant
  x -= 2; // Error: cannot reassign constant
  x *= 2; // Error: cannot reassign constant
  x /= 2; // Error: cannot reassign constant
  x **= 2; // Error: cannot reassign constant
  x %= 2; // Error: cannot reassign constant
  x &&= 2; // Error: cannot reassign constant
  x ||= 2; // Error: cannot reassign constant
  x ??= 2; // Error: cannot reassign constant
}

{
  // Const binding, string value
  const sx = "a";
  sx += "b"; // Error: cannot reassign constant
}

{
  // Read-only property
  const o: {|+p: number|} = {p: 1};
  o.p += 2; // Error: property is non-writable
  o.p -= 2; // Error: property is non-writable
  o.p *= 2; // Error: property is non-writable
  o.p /= 2; // Error: property is non-writable
  o.p **= 2; // Error: property is non-writable
  o.p %= 2; // Error: property is non-writable
  o.p &&= 2; // Error: property is non-writable
  o.p ||= 2; // Error: property is non-writable
  o.p ??= 2; // Error: property is non-writable
}

{
  // Read-only property, string value
  const o: {|+p: string|} = {p: "a"};
  o.p += "b"; // Error: property is non-writable
}

{
  // Write-only property
  const o: {|-p: number|} = {p: 1};
  o.p += 2; // Error: property is non-readable
  // TODO(T56716039): If you read a write-only property after it is written, there is no error
  // o.p -= 2; // Error: property is non-readable
}

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
  x ??= (x: null); // TODO: should not error `A` is not compatible with null
}

{
  // Flow does not understand LHS's nullability/truthiness/falseyness and the statement always throws.
  class A {}
  let x: null = null;
  let y: A = new A();

  function alwaysThrows1(): number { x ??= invariant(false); } // Error
  function alwaysThrows2(): number { y &&= invariant(false); } // Error
  function alwaysThrows3(): number { x ||= invariant(false); } // Error
}
