// @flow

type T = {| foo: string, bar: number |};

const x: T = {  };
//             ^
const y: T = {    : "foo" };
//              ^

declare function f({|foo?: string, bar: number, baz?: boolean|}): void;
f({
         // Should suggest `foo` and `baz`
// ^
  bar: 1,
         // Should suggest `foo` and `baz`
// ^
});
