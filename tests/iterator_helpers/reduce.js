/* @flow */
declare const iterator: Iterator<number>;
declare const numberReducer: (number,  number) => number;
declare const stringReducer: (string,  number) => string;
declare const flexibleReducer: (string | number,  number) => string;

// Basic usage
(iterator.reduce(numberReducer): number); // OK
(iterator.reduce(flexibleReducer): string | number); // OK
(iterator.reduce(stringReducer, ''): string); // OK

// Return type is discarded: https://tc39.es/proposal-iterator-helpers/#sec-iteratorprototype.reduce
// "ii. If value is done, return undefined."
declare const iteratorWithReturn: $Iterator<number, number, void>;
(iteratorWithReturn.map(numberReducer): $Iterator<number, void, void>); // OK
