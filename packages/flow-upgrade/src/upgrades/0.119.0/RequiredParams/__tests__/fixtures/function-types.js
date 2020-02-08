// @flow

type Fn = ?number => string;

declare function fn2(x: ?number): null;

type T1 = ?number;
type T2 = number;

declare function fn3(x: T1): null;

declare function fn4(x: T2): null;

declare function fn5<T>(x: T): null;

declare function fn6<T: number>(x: T): null;

declare function fn7<T: T1>(x: T): null;

declare function fn8<T: T2>(x: T): null;

declare var fn9: (x: number) => void;

declare var fn10: (x: ?number) => void;

declare var fn11: <T>(x: T) => void;

declare var fn12: <T: T1>(x: T) => void;

declare var fn13: <T: T2>(x: T) => void;

declare function fn14(x: number, ...rest: string[]): void;

declare function fn15(x: ?number, ...rest: string[]): void;

type O1 = {
  (number): number,
};

type O2 = {
  (?number): number,
};

type O3<T: number> = {
  <T1: T>(T1): number,
};

type O4<T: ?number> = {
  <T1: T>(T1): number,
};

type O5<T> = {
  <T1: number>(T1): number,
};

type O6<T> = {
  <T1: number>(T): number,
};
