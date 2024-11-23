/* @flow */
declare const iterator: Iterator<number>;

// Basic usage
(iterator.forEach((x: number): mixed => {}): void); // OK
