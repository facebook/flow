// @flow

// local

const baz = {baz: 12345};
const inexact: {...} = {p: 0};
const any_ = 0 as any;

// exports

export const frozenObject = Object.freeze({bar: '12345'});
export const frozenObjectWithSpread = Object.freeze({bar: '12345', ...baz});
export const frozenNumber = Object.freeze(123); // TODO type_sig support
export const frozenSpreadAny = Object.freeze({...any_});
export const frozenInexact = Object.freeze({...inexact});
export const frozenNegativeNumber = Object.freeze({foo: -1});

// Tests

frozenObject.bar = '23456'; // error bar is not writable
frozenObject as {bar: '12345'}; // TODO error bar is readonly
frozenObject as {+bar: '1234'}; // error '12345' ~> '1234'
frozenObject as {+bar: '12345', baz: '12345'}; // error baz is missing in frozenObject
Object.assign(frozenObject, {bar: '12345'}); // error bar is not writable

frozenObjectWithSpread.bar = '23456'; // error bar is not writable
frozenObjectWithSpread.baz = 3456; // error baz is not writable
frozenObjectWithSpread.corge; // error corge is missing
frozenObjectWithSpread.constructor = baz; // error baz not a function
frozenObjectWithSpread.toString = function () {}; // error toString missing

frozenObjectWithSpread as {bar: '12345', baz: 12345}; // TODO error bar and baz are readonly
frozenObjectWithSpread as {+bar: '1234', +baz: 12345}; // error '12345' ~> '1234'
frozenObjectWithSpread as {+bar: '12345', }; // error baz is missing in frozenObjectWithSpread
Object.assign(frozenObjectWithSpread, {bar: '12345'}); // error bar is not writable

frozenNumber as number; // okay

frozenSpreadAny.foo = 'bar'; // okay, there is no frozen form of AnyT so this is "allowed"

frozenInexact as {}; // Error: inexact -> exact

frozenNegativeNumber.foo as 1; // error -1 ~> 1
frozenNegativeNumber.foo as -1; // okay
1 as typeof frozenNegativeNumber.foo; // error 1 ~> -1
-1 as typeof frozenNegativeNumber.foo; // okay
