//@flow
import {Bar} from './cycle2'
export const foo: number = 3;

export component Foo() { return null; }

(Foo: typeof Bar); // OK
(Bar: typeof Foo); // OK

component Baz() { return null; }
(Bar: typeof Baz); // ERROR
(Foo: typeof Baz); // ERROR
(Baz: typeof Bar); // ERROR
