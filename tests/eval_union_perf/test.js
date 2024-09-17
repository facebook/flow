//@flow

import type {UnionElementOne} from './type.js'

export type UnionElementTwo = Info<{
  foo: $DeepReadOnly<{
    foo: boolean,


  }>,
  bar:
    | {|
        type: 'A',
        data: string,
      |}
    | {|type: 'B'|}
    | {|type: 'C'|},
  baz: $DeepReadOnly<{

  }>,
}>;

type Elements = {
  one: UnionElementOne,
  two: UnionElementTwo,
};

export type NoBar =
  | BarOne
  | BarTwo
  | BarThree;

export type $DeepReadOnly<T> =
  T extends $ReadOnlyArray<infer V> ? $ReadOnlyArray<$DeepReadOnly<V>> :
  T extends {...} ? {+[K in keyof T]: $DeepReadOnly<T[K]>} : T;

export type Info<
  T: {+bar: {+type: string}},
> =
  [+t: T] extends [+t: {bar: infer A}]
  ? {
      bar:
        | BarOne
        | BarTwo
        | BarThree
        | $DeepReadOnly<$Exact<A>>,
    }
  : empty;

type Bar<T> = $ElementType<T, 'bar'>;

type BarOne = $DeepReadOnly<{|
  type: 'one',
|}>;

type BarTwo = $DeepReadOnly<{|
  type: 'two',
|}>;
type BarThree = $DeepReadOnly<{|
  type: 'three',
|}>;
type BarFour = $DeepReadOnly<{|
  type: 'four',
|}>;

type BarFive = $DeepReadOnly<{|
  type: 'five',
|}>;
type BarSix = $DeepReadOnly<{|
  type: 'six',
|}>;
type BarSeven = $DeepReadOnly<{|
  type: 'seven',
|}>;

type BarUnion =
  | BarOne
  | BarTwo
  | BarThree
  | BarFour
  | BarFive
  | BarSix
  | BarSeven
  | $Values<{[K in keyof Elements]: Bar<Elements[K]>}>;

type Config = {|
  f: (dispatch: (BarUnion) => void) => void,
  g: (dispatch: (BarUnion) => void) => void,
  h: (dispatch: (BarUnion) => void) => void,
|};

function test<X>(config: Config): Config {
  const {f, g, h} = config;

  return {
    f: dispatch => f(dispatch),
    g: dispatch => g(dispatch),
    h: dispatch => h(dispatch),
  };
}
