// @flow

export var r: any | typeof r = 0;

// $FlowExpectedError[value-as-type]
declare export var x: { r: r };

export type T =
  // $FlowExpectedError[incompatible-use]
  | T.A
  // $FlowExpectedError[incompatible-use]
  | T.B

export type S = string | R;

export type R = { p: S };

export class C {
  static s(): C {
    return new C();
  }
}

export type DisjoinUnionOpt =
  | { kind: 'kind1', items: $ReadOnlyArray<DisjoinUnionOpt | null> }
  | { kind: 'kind2' };

declare var o: o;
export const arr = [o.f, o.g];
