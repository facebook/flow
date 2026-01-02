// @flow

type X<A, B> = A | B;

42 as NonNullable<number | string>; // Ok
42 as NonNullable<X<number, string>>; // Ok
