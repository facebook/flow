/* @flow */
declare const iterator: Iterator<number>;
declare const predicate: (number) => boolean;

// Basic usage
(iterator.some(predicate): boolean); // OK
