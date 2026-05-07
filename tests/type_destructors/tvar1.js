// @flow

declare function fn<T>(x: T): T['foo'];

declare const c: {foo: {bar: any, ...}, ...};
const x = fn(c);

x as {bar: unknown, ...};
x as {bar: empty, ...};
