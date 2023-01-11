// @flow

declare function f<T>(): T
// $FlowFixMe[underconstrained-implicit-instantiation]
const a = f(); // No annotations should be added
(a: string);

declare function g<T>(Set<T>): T;
const b = g(new Set()); // Annotate with string
(b: string);
