declare component Poly<T: mixed>();

declare const polyNumber: renders Poly<number>;
polyNumber as renders Poly<number>; // OK
polyNumber as renders Poly<string>; // SHOULD ERROR

declare const polyNoArgs: renders Poly;
polyNoArgs as renders Poly<mixed>; // OK
polyNoArgs as renders Poly<number>; // SHOULD ERROR
