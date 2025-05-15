// @flow

import thing from "./helpers/exports_default.js";
thing;
import thing2 from "./helpers/module_exports.js";
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

import {
  type X as _barrel_X1,
  x as _barrel_x1,
  y as _barrel_y1,
  z as _barrel_z1,
  foo as _barrel_foo1,
  bar as _barrel_bar1,
} from "./helpers/barrel_export_level1.js";
import {
  type X as _barrel_X2,
  x as _barrel_x2,
  y as _barrel_y2,
  z as _barrel_z2,
  foo as _barrel_foo2,
  bar as _barrel_bar2,
} from "./helpers/barrel_export_level2.js";
