/* @flow */
declare const iterator: Iterator<number>;
declare const predicate: (number) => boolean;

// Basic usage
iterator.some(predicate) as boolean; // OK
