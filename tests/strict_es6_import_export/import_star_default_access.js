// @flow

import * as Foo from './foo';

Foo.default;
Foo['default'];

const {default: renamed1} = Foo;
const {'default': renamed2} = Foo;

let x = 0;
({default: x}) = Foo;
({'default': x}) = Foo;

import {default as renamed3} from './foo';
