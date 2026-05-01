import type { MyType } from './mod'; // ok

declare var o1: MyType;
o1 as string; // ok
o1 as empty; // error
