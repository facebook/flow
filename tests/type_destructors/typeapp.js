// @flow

type X<A, B> = A | B;

(42: $NonMaybeType<number | string>); // Ok
(42: $NonMaybeType<X<number, string>>); // Ok
