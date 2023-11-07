//@flow
import {Bar} from './cycle2';
export const foo: number = 3;

export component Foo() {
  return null;
}

Foo as typeof Bar; // OK
Bar as typeof Foo; // OK

component Baz() {
  return null;
}
Bar as typeof Baz; // ERROR
Foo as typeof Baz; // ERROR
Baz as typeof Bar; // ERROR
