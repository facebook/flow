// @flow

type A<T> = { readonly tag: 1, value: T } | { readonly tag: 2, value: T };

declare opaque type O: A<string>;

declare const o: O;
o as A<string>; // okay
o as A<number>; // error
