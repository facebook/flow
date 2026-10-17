declare function promiseFirst(): Promise<void> & {foo: number};
declare function objectFirst(): {foo: number} & Promise<void>;
declare function promises(): Promise<void> & Promise<number>;
declare function union(): Promise<void> | number;
declare function anyF(): any;

class MyPromise extends Promise<void> {}
class MyPromise2 extends MyPromise {}
declare function sub(): MyPromise2;

async function edge() {
  promiseFirst(); // error
  objectFirst(); // no error: only the first intersection member is checked
  promises(); // error
  union(); // error
  anyF(); // ok
  sub(); // error
}

function generic<T extends Promise<void>>(x: () => T) {
  x(); // error
}

import type {O} from './opaque';
declare function op(): O;
function opaque() {
  op(); // error
}

// The same interface has a different super for each type argument
type Sel<T> = T extends 1 ? Promise<void> : Object;
type W<T> = interface extends Sel<T> {};
declare function both(): W<2> | W<1>;
async function superDependsOnTypeArgs() {
  both(); // error
}
