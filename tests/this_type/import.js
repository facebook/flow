// Check that imports are handled properly with this types

import { A1 } from './export';
import type { A2 } from './export';
import { A3 } from './export';

class B1 extends A1 {
  foo(): B1 { return new B1(); } // error
}

new B1().bar() as B1; // OK

class B3<X> extends A3<X> {
  foo(): B3<X> { return new B3(); } // error
}

new B3<mixed>().bar() as B3<any>; // OK
new B3<string>().qux(0) as string; // error

new B3<mixed>().bar() as A2<any>; // OK
new B3<string>().bar() as B3<string> as A2<number>; // error
(new B3() as A2<number>).qux(0) as string; // error

import Export from './export';

declare var a4: Export.A4;
let _ = a4.foo();
