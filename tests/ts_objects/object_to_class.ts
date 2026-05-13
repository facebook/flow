// Object literal (and other object types) flowing INTO a class type.
// In .ts files, classes are checked structurally (like interfaces always
// were), AND the synthetic `constructor` proto-prop is filtered out so
// `{ x: 1 } as C` works -- matching TS behavior.

declare class Point {
  x: number;
  y: number;
}

// Object literal with the right shape: OK. The class's `constructor`
// proto-prop must be ignored, otherwise this would error for missing it.
({x: 1, y: 2}) as Point; // OK

// Missing a required field is still an error.
({x: 1}) as Point; // ERROR: missing `y`

// Wrong type on a field is still an error.
({x: 1, y: "two"}) as Point; // ERROR: string vs number

// Class with methods: methods on the class are checked structurally too.
declare class Greeter {
  name: string;
  greet(): string;
}

// Object literal supplying the method: OK.
({
  name: "world",
  greet(): string {
    return "hi";
  },
}) as Greeter; // OK

// Object literal missing the method: still ERROR.
({name: "x"}) as Greeter; // ERROR: missing `greet`

// Regression: the InterfaceKind branch of `structural_subtype` was already
// reachable before this change. Verify it still accepts an object literal.
interface IPoint {
  x: number;
  y: number;
}
({x: 1, y: 2}) as IPoint; // OK
({x: 1}) as IPoint; // ERROR: missing `y`
