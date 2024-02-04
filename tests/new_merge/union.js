// @flow

declare class C1 {
  f: number;
}

declare class C2 {
  f: string;
}

declare var c: C1 | C2;

declare export var x: c; // error: value-as-type
