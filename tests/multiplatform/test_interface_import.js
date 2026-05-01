import type { MyType } from './shadowing'; // ok

import './shadowing.ios'; // ok, platform specific files can still be **explictly** imported
import './shadowing.android'; // ok, platform specific files can still be **explictly** imported

declare var o1: MyType;
o1 as string; // ok
o1 as empty; // error

import './allow_no_interface'; // error
