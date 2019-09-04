//@flow

type T<S, T = S> = {x: S, y: T};
declare var a: T<number>;
a.
