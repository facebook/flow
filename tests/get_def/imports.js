// @flow

import thing from "./helpers/exports_default.js";
thing;

import {foo, bar as baz, letBinding, varBinding, fun, Cls} from "./helpers/exports_named.js";
foo;
baz;

import * as things from "./helpers/exports_named.js";
things;

// $FlowFixMe
import type {DoesNotExist} from "./doesNotExist";
const x: DoesNotExist<number> = "foo";
x.foo();

type Foo = any;
// $FlowFixMe
const y: Foo<number> = "foo";
// $FlowFixMe
y.foo();

import * as test_lib from 'test_lib';
test_lib;

import typeof typeof_thing from "./helpers/exports_default.js";
("foo": typeof_thing);

import typeof * as things_ns from "./helpers/exports_named.js";
// $FlowFixMe
({}: things_ns);
