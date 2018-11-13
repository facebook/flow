// @flow

type C = {| f: ?string |};
type D = {| f: $PropertyType<C, "f"> |};
type E = $ReadOnly<{ x: number }>;

export const obj = Object.freeze({ A: 'a', B: 'b' });
type F = {| vs: $Values<typeof obj> |};

type G<R> = $Call<<N>() => N, R>;

type H = {| a: string, b: number |};


type J<K> = $ElementType<H, K>;
type K = J<'a'>;
type L = J<mixed>;

type Ma<K: 'a', L: K> = $ElementType<H, L>;
type Mb<K: 'b', L: K> = $ElementType<H, L>;
type Mc<K: 'c', L: K> = $ElementType<H, L>; // empty

type N = $ElementType<H, 'c'>; // Flow would throw error so returns 'any'

declare class P<O: Object, A: 'a'> {
  m<B: 'b'>(
    o: $ElementType<H, O>,
    a: $ElementType<H, A>,
    b: $ElementType<H, B>,
  ): void;
}

type Q = $NonMaybeType<?number>;
type QP<X> = $NonMaybeType<X>;

type R = $Values<H>;
type RP<X> = $Values<X>;

declare opaque type OX;
declare opaque type OY;
type S = $Rest<{|y: OY|}, {|[string]: OY|}>;
type SP<X> = $Rest<{|y: X|}, {|[string]: X|}>;

type U = $ObjMap<{a: number, b?: number}, <T>(T) => Array<T>>;
type UP<X, Y> = $ObjMap<{a: X, b?: Y}, <T>(T) => Array<T>>;

type V = $TupleMap<[mixed, mixed], <K>(k: K) => 'FOO'>;
type VP<X> = $TupleMap<[X, mixed], <K>(k: K) => 'FOO'>;


// React

import * as React from 'react';

class MyComponent extends React.Component<{foo: number}> {
  static defaultProps = {foo: 42};
  render() {
    return this.props.foo;
  }
}

type ReactA = React.ElementProps<typeof MyComponent>;
type ReactAP<X> = React.ElementProps<X>;

// The following tests caching of EvalT result. If re-evaluated the $NonMaybeType
// under the second EvalT would appear as empty
declare var a: { m<T>(x: $NonMaybeType<T>): T };
declare var b: { x: typeof a; y: typeof a; }
