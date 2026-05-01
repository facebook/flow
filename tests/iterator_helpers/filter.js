/* @flow */
declare const iterator: Iterator<number>;

declare function check(x: number): boolean;

// Basic usage
iterator.filter(check) as Iterator<number>; // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.filter
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
iteratorWithReturn.filter(check) as $Iterator<number, void, void>; // OK

// Filters nullish values with Boolean
declare const iteratorWithNullableValues: Iterator<?number>;
iteratorWithNullableValues.filter(Boolean) as Iterator<number>; // OK

// Propagates type guards
declare function guarded(x: ?number): implies x is number;
iteratorWithNullableValues.filter(guarded) as Iterator<number>; // OK
