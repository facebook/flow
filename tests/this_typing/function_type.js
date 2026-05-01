/* simple */
declare var x : (this : number) => void;

(x.bind(3)());
(x.bind("")()); // error: number incompatible with string

/* generics */
type F<T> = (this : T) => void;

declare var y : F<number>;
y.bind(3);
y.bind(""); // error: number incompatible with string

x as F<number>;
y as F<string>; // error: number incompatible with string

declare var z : <T>(this : T) => T;
z.bind(3)() as number;
z.bind("")() as string;
z.bind(3)() as string; // error: number incompatible with string
z.bind("")() as number; // error: number incompatible with string

/* Subtyping */
/* this is contravariant */

type TopFn = (this : empty) => mixed;
type BotFn = (this : mixed) => empty;

x as TopFn;
x as BotFn; // error
