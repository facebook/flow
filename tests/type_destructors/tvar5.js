// @flow

declare function unReadOnly<T>(thing: Readonly<T>): T;
declare var foo: Readonly<{foo: string}>;
unReadOnly(foo) as empty; // error
