// @flow

export function f(): typeof f.g { return 1; }
f.g = 1;

declare export class C {
  static P: number;
  static Q: typeof (C.P);
}

declare export var x: {
  p: number;
  q: typeof (x.p);
}
