//@flow

type T<S=number> = {x: S};
declare const a: T<>;
a.
//^
