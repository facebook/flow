/* @flow */
declare const iterator: IteratorObject<number>;

declare function check(x: number): boolean;

// Basic usage
iterator.filter(check) as IteratorObject<number>; // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.filter
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
iteratorWithReturn.filter(check) as $Iterator<number, void, void>; // OK

// Filters nullish values with Boolean
declare const iteratorWithNullableValues: IteratorObject<?number>;
iteratorWithNullableValues.filter(Boolean) as IteratorObject<number>; // OK

// Propagates type guards
declare function guarded(x: ?number): implies x is number;
iteratorWithNullableValues.filter(guarded) as IteratorObject<number>; // OK
