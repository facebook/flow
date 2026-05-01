/* @flow */
declare const iterator: Iterator<number>;

// Basic usage
iterator.forEach((x: number): mixed => {}) as void; // OK
