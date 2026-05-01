// `typeof X` resolves to the value binding regardless of whether a same-named
// type binding exists.
const T = 42;
interface T { x: number; }

const a: typeof T = 42;     // typeof T is `number` (value side)
const b: T = { x: 1 } as T; // T is the interface (type side)
