import type { MyType } from './mod'; // ok

declare var o1: MyType;
(o1: string); // ok
(o1: empty); // error
