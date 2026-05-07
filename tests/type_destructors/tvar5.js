// @flow

declare function unReadOnly<T>(thing: Readonly<T>): T;
declare const foo: Readonly<{foo: string, ...}>;
unReadOnly(foo) as empty; // error
