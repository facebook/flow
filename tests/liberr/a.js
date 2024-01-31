/**
 * @flow
 */
// one error here, to verify lib errors sort to top.
var x: string = 0;

require('c') as empty; // error: boolean ~> empty

require('toplevel-declared-module'); // ok
require('nested-declare-module1'); // error
require('nested-declare-module2'); // error
require('nested-declare-module3'); // error

declare module 'non-libdef-declare-module' {} // error
require('non-libdef-declare-module'); // error

require('contains-unsupported-statements') as empty; // error: number ~> empty
