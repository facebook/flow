// @flow

import {
  frozenObject,
  frozenObjectWithSpread,
  frozenNumber,
  frozenSpreadAny,
  frozenInexact,
  frozenNegativeNumber,
} from './object_freeze';

// Tests

frozenObject.bar = '23456'; // error bar is not writable
frozenObject as {bar: '12345'}; // error bar is readonly
frozenObject as {+bar: '1234'}; // error '12345' ~> '1234'
frozenObject as {+bar: '12345', baz: '12345'}; // error baz is missing in frozenObject
// $FlowExpectedError[unsafe-object-assign]
Object.assign(frozenObject, {bar: '12345'}); // error bar is not writable

frozenObjectWithSpread.bar = '23456'; // error bar is not writable
frozenObjectWithSpread.baz = 3456; // error baz is not writable
frozenObjectWithSpread.corge; // error corge is missing
frozenObjectWithSpread.constructor = baz; // error baz not a function
frozenObjectWithSpread.toString = function () {}; // error toString missing

frozenObjectWithSpread as {bar: '12345', baz: number}; // error bar and baz are readonly
frozenObjectWithSpread as {+bar: '1234', +baz: number}; // error '12345' ~> '1234'
frozenObjectWithSpread as {+bar: '12345', }; // error baz is missing in frozenObjectWithSpread
// $FlowExpectedError[unsafe-object-assign]
Object.assign(frozenObjectWithSpread, {bar: '12345'}); // error bar is not writable

frozenNumber as number; // okay

frozenSpreadAny.foo = 'bar'; // okay, there is no frozen form of AnyT so this is "allowed"

frozenInexact as {}; // Error: inexact -> exact

frozenNegativeNumber.foo as 1; // error -1 ~> 1
frozenNegativeNumber.foo as -1; // okay
1 as typeof frozenNegativeNumber.foo; // error 1 ~> -1
-1 as typeof frozenNegativeNumber.foo; // okay
