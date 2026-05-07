//@flow

type T<S, T = S> = {x: S, y: T};
declare const a: T<number>;
a.
//^
