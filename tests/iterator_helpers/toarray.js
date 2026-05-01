/* @flow */
declare const iterator: Iterator<number>;

// Basic usage
iterator.toArray() as Array<number>; // OK
