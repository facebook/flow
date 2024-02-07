function f<T extends string>(t: T): void {} // ok
f(1); // error: number ~> string
require('./exported').f(1); // error: number ~> string
