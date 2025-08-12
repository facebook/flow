import type { MyType } from './shadowing'; // ok

import './shadowing.ios'; // ok, platform specific files can still be **explicitly** imported
import './shadowing.android'; // ok, platform specific files can still be **explicitly** imported

declare var o1: MyType;
(o1: string); // ok
(o1: empty); // error

import './allow_no_interface'; // error
