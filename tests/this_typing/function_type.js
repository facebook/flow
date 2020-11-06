// @flow

/* simple */
declare var x : (this : number) => void;

(x.bind(3)());
(x.bind("")()); // error: number incompatible with string

/* generics */
type F<T> = (this : T) => void;

declare var y : F<number>;
y.bind(3);
y.bind(""); // error: number incompatible with string

(x : F<number>);
(y : F<string>); // error: number incompatible with string

declare var z : <T>(this : T) => T;
(z.bind(3)() : number);
(z.bind("")() : string);
(z.bind(3)() : string); // error: number incompatible with string
(z.bind("")() : number); // error: number incompatible with string

/* Subtyping */
/* this is contravariant */

type TopFn = (this : empty) => mixed;
type BotFn = (this : mixed) => empty;

(x : TopFn);
(x : BotFn); // error
