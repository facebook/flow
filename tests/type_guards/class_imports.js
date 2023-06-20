import {C, D} from './class_exports';

declare var x: mixed;

const c = new C();
if (c.m(x)) {
  (x: number);
  (x: string); // error number ~> string
}

const d = new D();
if (d.m(x)) {
  (x: number);
  (x: string); // error number ~> string
}
