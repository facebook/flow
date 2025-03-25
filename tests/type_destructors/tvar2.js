// @flow

class C<T> {
  x: T;
}

declare function fn<T>(x: C<T>): C<T['foo']>;

declare var c: C<{foo: {bar: any}}>;
const x = fn(c);

x as C<{bar: mixed}>;
x as C<{bar: empty}>;
