/* @flow */
declare const iterator: IteratorObject<number>;
declare const predicate: (number) => boolean;

// Basic usage
iterator.every(predicate) as boolean; // OK
