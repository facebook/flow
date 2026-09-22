/* @flow */
declare const iterator: IteratorObject<number>;

// Basic usage
iterator.toArray() as Array<number>; // OK
