/* @flow */
declare const iterator: IteratorObject<number>;

// Basic usage
iterator.map(String) as IteratorObject<string>; // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.map
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
iteratorWithReturn.map(String) as $Iterator<string, void, void>; // OK
