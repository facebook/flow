type T = $ReadOnly<{a: string}>; // ERROR

export type exportedType = Readonly<{
  a?: ?number,
}>;

let a: exportedType = {b: 1}; // ERROR
