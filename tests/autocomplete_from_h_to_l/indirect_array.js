//@flow

declare const y: ?Array<number>;

y.
//^

opaque type t: Array<number> = Array<number>;
declare const z: t;

z.
//^
