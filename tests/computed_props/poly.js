//@flow

function f1<T extends string>(x: T) { return {[x]: x} } // intended as the paired OK case (bounded T extends string should allow computed-prop keys); currently ERROR — Flow requires a return annotation on poly functions, and even with one (`: {[T]: T}`) it widens the computed key from T to string and emits [incompatible-type]
function f2<T>(x: T) { return {[x]: x} } // error. mixed can't be used as a computed prop
