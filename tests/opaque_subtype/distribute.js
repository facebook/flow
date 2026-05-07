// @flow

type A<T> = { +tag: 1, value: T } | { +tag: 2, value: T };

declare opaque type O: A<string>;

declare const o: O;
o as A<string>; // okay
o as A<number>; // error
