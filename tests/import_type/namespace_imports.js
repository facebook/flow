/**
 * @flow
 */

import type * as A from "./A";
var actualA = require('./A');

// Import a class and make sure it has the right type
(actualA: typeof A);
(new actualA(): A);

import type * as B from "./B";
var actualB = require('./B');

(new actualB.Foo(): B.Foo);
(new actualB.Bar(): B.Bar);

// And to make sure it's working lets use some of this stuff wrong
(123: A);
(456: B.Bar);
