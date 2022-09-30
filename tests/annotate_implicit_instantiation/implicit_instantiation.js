//@flow
import {x} from './opaque';
declare function f<T>(): {x: T}
const a = f();
a.x = x;


const map = new Map();
map.set('x', 3);

declare function id<T>(x:T): T;
const b = id(3); // ok!
