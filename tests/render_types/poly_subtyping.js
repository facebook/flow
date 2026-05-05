declare component Poly<T extends unknown>();

declare const polyNumber: renders Poly<number>;
polyNumber as renders Poly<number>; // OK
polyNumber as renders Poly<string>; // SHOULD ERROR

declare const polyNoArgs: renders Poly;
polyNoArgs as renders Poly<unknown>; // OK
polyNoArgs as renders Poly<number>; // SHOULD ERROR
