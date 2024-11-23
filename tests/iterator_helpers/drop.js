/* @flow */
declare const iterator: Iterator<number>;

// Basic usage
(iterator.drop(2): Iterator<number>); // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.drop
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
(iteratorWithReturn.drop(2): $Iterator<number, void, void>); // OK
