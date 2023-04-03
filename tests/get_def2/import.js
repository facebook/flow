// @flow

import type {Foo} from './types';
//            ^

import type {} from './types';
//                     ^

function takesFoo(foo: Foo) { }
//                      ^
