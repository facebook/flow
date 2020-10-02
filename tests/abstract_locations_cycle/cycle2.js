// @flow

import {Foo} from './cycle1';

export const foo: Foo = new Foo();

export type Bar = Foo;
