/**
 * @flow
 */

var actualA = require('./A');

// TODO actually notice that Foo Baz and A are out of scope
// foo, baz, and A shouldn't be in scope yet
(new actualA.Foo(): Foo);
(new actualA.Bar(): Baz);
(actualA: typeof A);

import type { Foo, Bar as Baz } from "./B";
import type * as A from './A';
