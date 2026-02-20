// @flow

// Test 1: Double nullable - ?? in type position should be two ? tokens
// ??T means ?(?T), i.e. nullable of nullable
function doubleNullable(x: ??string): ?string {
  return x; // ok: ??string is equivalent to ?string
}

// Test 2: Double nullable in variable type annotation
var y: ??number = null;
var z: ??number = undefined;
var w: ??number = 42;

// Test 3: Double nullable in cast
(('hello': ??string): ?string); // ok

// Test 4: Double nullable in object type
type Obj = {
  x: ??string,
};
