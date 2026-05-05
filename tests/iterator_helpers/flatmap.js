/* @flow */
declare const iterator: Iterator<number>;
declare function mapper(x: number): ReadonlyArray<string>

// Basic usage
iterator.flatMap(mapper) as Iterator<string>; // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.flatmap
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
iteratorWithReturn.flatMap(mapper) as $Iterator<string, void, void>; // OK
