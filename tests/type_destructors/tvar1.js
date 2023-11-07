// @flow

declare function fn<T>(x: T): $PropertyType<T, 'foo'>;

declare var c: {foo: {bar: any}};
const x = fn(c);

x as {bar: mixed};
x as {bar: empty};
