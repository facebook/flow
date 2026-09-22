/* @flow */
declare const iterator: IteratorObject<number>;

// Basic usage
iterator.forEach((x: number): unknown => {}) as void; // OK
