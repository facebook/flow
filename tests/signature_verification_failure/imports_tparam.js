//@flow

import {f} from './tparam';
// make sure we're importing the right types

var x = new (f())();
x.p as string;
x as number; // nope
