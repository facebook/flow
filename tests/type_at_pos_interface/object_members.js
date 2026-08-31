// @flow

// Members of a named interface's own body are qualified with the interface
// name; members of an object type are not, since it has no name.

interface Named {
  p: number;
//^
  m(): void;
//^
  readonly rp: number;
//         ^
  op?: number;
//^
  nested: {inner: number};
//         ^
}

type Anon = {a: number, b: string};
//           ^

type Intersected = {c: 1} & {d: 2};
//                  ^

// An object type in the extends clause is not part of the interface body.
interface Extending extends $ReadOnly<{outer: number}> {}
//                                     ^
