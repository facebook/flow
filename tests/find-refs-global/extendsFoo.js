// @flow

import {Foo} from './es6-1';

class Bar extends Foo {}

const x = new Bar();
x.foo();
