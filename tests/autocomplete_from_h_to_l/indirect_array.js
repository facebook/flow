//@flow

declare var y: ?Array<number>;

y.
//^

opaque type t: Array<number> = Array<number>;
declare var z: t;

z.
//^
