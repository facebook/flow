// @flow
// Cross-file: a .js consumer importing an `object`-typed function from a .ts
// file. The `object` type is not nameable in .js (see js_unrecognized.js), but
// values typed by it flow across the module boundary, and its primitive
// rejection travels with the type.
import {takesObject} from './object';

takesObject({a: 1}); // OK -- object literal
takesObject([1, 2]); // OK -- array
takesObject(1); // ERROR -- number is not an object
takesObject("x"); // ERROR -- string is not an object
