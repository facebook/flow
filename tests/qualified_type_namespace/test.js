import * as NS from './m';
import typeof * as NSTypeof from './m';
import * as Missing from './does-not-exist'; // error
import Untyped from './untyped';

type A1 = NS.T;
1 as A1;
's' as A1; // error
type A2 = NS.O;
type A3 = NS.C;
new NS.C() as A3;
type A4 = NS.I;
type A5 = NS.v; // error: value used as a type
type A6 = NS.nope; // error
type A7 = Missing.T; // error: any-typed value used as a type
type A8 = Untyped.T; // error: any-typed value used as a type
type A9 = NSTypeof.v; // error

declare namespace DNS {
  type T = string;
  declare const c: number;
}
type B1 = DNS.T;
's' as B1;
1 as B1; // error
type B2 = DNS.c; // error: value used as a type
type B3 = DNS.nope; // error

declare class K {
  static S: number;
}
type C1 = K.S; // error: value used as a type
declare const obj: {P: number};
type C2 = obj.P; // error: value used as a type
declare var anyV: any;
type C3 = anyV.Q; // error: any-typed value used as a type
