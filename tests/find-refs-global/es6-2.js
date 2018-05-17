/* @flow */

import Bar, {foo, Foo} from './es6-1';
import * as all from './es6-1';

foo();

const x = new Foo();
x.foo();

(x: any).foo();

export function returnsFoo(): Foo {
  return new Foo();
}

x.bar();

new Bar();
new all.default();
