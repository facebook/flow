import type { MyType } from 'shadowing'; // ok

import 'shadowing.ios'; // error, platform specific files cannot be explicitly imported in Haste, since metro doesn't support it
import 'shadowing.android'; // error, platform specific files cannot be explicitly imported in Haste, since metro doesn't support it

declare var o1: MyType;
(o1: string); // ok
(o1: empty); // error
