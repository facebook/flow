/**
 * @flow
 */

// Make sure the empty import works
import type {} from "./B";
import type { Foo, Bar as Baz } from "./B";

var actualB = require('./B');

(new actualB.Foo(): Foo);
(new actualB.Bar(): Baz);

// Bar should be unknown
(new actualB.Bar(): Bar);
