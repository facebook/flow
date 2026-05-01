import {C, D} from './class_exports';

declare var x: unknown;

const c = new C();
if (c.m(x)) {
  x as number;
  x as string; // error number ~> string
}

if (c.os(x)) {
  x as number;
  x as string; // error number ~> string
}

const d = new D();
if (d.m(x)) {
  x as number;
  x as string; // error number ~> string
}

if (d.os(x)) {
  x as number;
  x as string; // error number ~> string
}
