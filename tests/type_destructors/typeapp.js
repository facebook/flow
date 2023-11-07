// @flow

type X<A, B> = A | B;

42 as $NonMaybeType<number | string>; // Ok
42 as $NonMaybeType<X<number, string>>; // Ok
