// @flow

declare function unReadOnly<T>(thing: $ReadOnly<T>): T;
declare var foo: $ReadOnly<{foo: string}>;
(unReadOnly(foo): empty); // error
