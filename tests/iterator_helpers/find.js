/* @flow */
declare const iterator: Iterator<number>;
declare const predicate: (number) => boolean;

// Basic usage
(iterator.find(predicate): ?number); // OK

// Propagates type guards
declare const mixedIterator: Iterator<number | string>;
declare function guarded(x: number | string): implies x is number;
(mixedIterator.find(guarded): ?number); // OK
