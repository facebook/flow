/**
 * @flow
 */

import type * as A from "./A.js";
import type {Foo, Bar as Baz} from "./A.js";

var A = require('./A.js');
var Foo = A.Foo;
var Baz = A.Bar;

// Since we shadowed the type vars with value vars, this should be fine
var m = A;
var n = Foo;
var o = Baz;

// And using them in types should work too
var a: Foo = new Foo();
var b: Foo = new A.Foo();
(new A.Bar(): Baz);
