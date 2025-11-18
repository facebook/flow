type T = $ReadOnly<{a: string}>; // OK

export type exportedType = Readonly<{
  a?: ?number,
}>;

let a: exportedType = {b: 1}; // ERROR
