// @flow

type O =
  | { foo: "a" }
  | { foo: "b" }
  | { foo: "c" }

declare const o: O;

o.foo;
// ^

if (o.foo === "a") {
  o.foo;
//   ^
} else if (o.foo === "b") {
  o.foo;
//   ^
} else if (o.foo === "c") {
  o.foo;
//   ^
}
