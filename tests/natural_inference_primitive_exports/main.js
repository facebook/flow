// @flow

import default_ from './es6_default';

import cjs_prim from './cjs_primitive';
import cjs_obj from './cjs_obj';

declare var _: any;

function test_default(x: typeof default_) {
  x as "abc"; // error string ~> "abc"
}

// CommonJS primitive literals are inferred as their general type
function test_cjs1() {
  cjs_prim as "abc"; // error string ~> "abc"
}

function test_cjs2() {
  cjs_obj.foo as "abc"; // error string ~> "abc"
  cjs_obj.bar as 1; // error number ~> 1
  cjs_obj.baz as true; // error boolean ~> true
}
