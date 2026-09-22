/* @flow */
declare const iterator: IteratorObject<number>;

// Basic usage
iterator.drop(2) as IteratorObject<number>; // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.drop
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
iteratorWithReturn.drop(2) as $Iterator<number, void, void>; // OK
