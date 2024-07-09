{
  declare const x: {|a: true, b: number|} | {|a: false, b: string, c: true|};
  if (x.c) {
    x as empty; // ERROR
    x as {|a: false, b: string, c: true|}; // OK
  } else {
    x as empty; // ERROR
    x as {|a: true, b: number|}; // OK
  }
}

{
  // Non-readable
  declare const x: {|a: true, b: number|} | {|a: false, b: string, -c: true|};
  if (x.c) { // ERROR: prop not readable
    x as empty; // ERROR
  } else {
    x as empty; // ERROR
  }
}
