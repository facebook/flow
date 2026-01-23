// Test usage of generic functions with type parameters in ambient context

import {identity, map, combine} from './generic_functions.js.flow';

identity<string>("hello") as string; // OK
identity<number>(42) as number; // OK

map<number, string>([1, 2, 3], (x) => String(x)) as Array<string> ; // OK
map<string, number>(["1", "2"], (s) => Number(s)) as Array<number>; // OK

combine<string, number, boolean>("a", 1, (a, b) => true) as boolean; // OK

identity<string>("hello") as number; // ERROR - string not assignable to number
map<number, string>([1, 2], (x) => String(x)) as Array<number>; // ERROR - Array<string> not compatible with Array<number>
