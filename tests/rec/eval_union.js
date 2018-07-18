// @flow

declare var b: boolean;
declare function make<U>(cb: (U) => U): $NonMaybeType<U>;
make(x => (b ? x : 42));
