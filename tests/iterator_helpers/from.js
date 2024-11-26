/* @flow */
// From a built-in iterable
(Iterator.from([1]): Iterator<number>);

// From a custom iterator implementation
declare const iterator: $IteratorProtocol<number>;
(Iterator.from(iterator): Iterator<number>);
