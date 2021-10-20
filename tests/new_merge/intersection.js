// @flow

declare class C1 {
  f: number;
}

declare class C2 {
  f: string;
}

declare var c: C1 & C2;

// $FlowFixMe[incompatible-type]
declare export var x: c;
