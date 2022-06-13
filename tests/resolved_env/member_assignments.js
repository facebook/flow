// @flow

function member_assignment_simple() {
  type Foo = {bar: string};
  declare var foo: Foo;

  (foo.bar: string); // ok
  (foo.bar: number); // error: string is incompatible with number
  foo.bar = ""; // ok
  (foo.bar: string); // ok
  (foo.bar: number); // error: string is incompatible with number
}

function member_assignment_contravariant() {
  type Foo = {-bar: string};
  declare var foo: Foo;

  foo.bar; // error: not readable
  foo.bar = ""; // ok
  (foo.bar: string); // ok
  (foo.bar: number); // error: string is incompatible with number
}

function member_op_assignment_ok(o: {|p: number|}) {
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

function member_op_assignment_refinement_ok(o: {|p: ?number|}) {
  o.p ??= 3;
  (o.p: number) // ok
}

function member_op_assignment_non_writeable(o: {|+p: number|}) {
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
