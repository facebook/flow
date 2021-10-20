// @flow

declare class C { f: number }
declare class D extends C {}

declare var d: D;

declare export var x: typeof d.f;

declare class B { static f: number; }
declare class A extends B { static f: number; }
export type S = {
  // $FlowExpectedError[value-as-type]
  f: A.f,
  // $FlowExpectedError[prop-missing]
  g: A.g
};

declare class P<X> { static f: number; }
declare class Q extends P<any> {}
export type T = {
  // $FlowExpectedError[value-as-type]
  f: Q.f,
};

declare var Any: any;
declare class W extends Any {}
// $FlowExpectedError[value-as-type]
export type R = W.f;
