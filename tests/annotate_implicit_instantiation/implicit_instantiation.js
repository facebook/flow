//@flow
import {x} from './opaque';
declare function f<T>(): {x: T}
const a = f();
a.x = x;


const map = new Map();
map.set('x', 3);

declare function id<T>(x:T): T;
const b = id(3); // ok!


declare function semi<T, U>(x: T): [T, U];
const c = semi(3); // semi<_, number>
c[1] = 3;
