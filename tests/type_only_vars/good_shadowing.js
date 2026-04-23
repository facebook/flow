/**
 * @flow
 */

import typeof A from './A.js';
import type {Foo, Bar as Baz} from './A.js';

// Cross-namespace coexistence: the value-side `var` bindings are independent
// of the same-named type-only imports.
var A = require('./A.js');
var Foo = A.Foo;
var Baz = A.Bar;

// Value-position reads now resolve to the var bindings above, so no errors.
var m = A;
var n = Foo;
var o = Baz;

// Type-position uses still resolve to the type-only imports, and value-position
// uses (`new Foo()`) resolve to the value bindings, so all of these are fine.
var a: Foo = new Foo();
var b: Foo = new A.Foo();
new A.Bar() as Baz;
