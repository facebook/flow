// @flow

function fn1<T>(x: T) { }

function fn2<T: number>(x: T) { }

function fn3<T: ?number>(x: T) { }

function fn4<T: number>(x: ?T) { }

type T1 = number | ?string;
type T2 = number | string;
type T3 = T1 | string;

function fn5(x: T1) {}

function fn6(x: T2) {}

function fn7(x: T3) {}

function fn8<T: T1>(x: T) {}

function fn() {
  // Local overwrite
  type T1 = number;
  function fn9<T: T1>(x: T) {}
  function fn10<T: T3>(x: T) {}
}

function fn11(x: $ReadOnlyArray<?string>) {}

function fn12(x: $NonMaybeType<?string>) {}