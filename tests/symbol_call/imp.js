import {registered, shared, table} from './exp';

table[shared] as number; // OK
table[registered] as string; // OK

// A symbol minted here is a different symbol, however it describes itself.
const local = Symbol('shared');
table[local]; // ERROR: not a key of `table`

// `Symbol.for` returns the same symbol for the same key at runtime, but each
// call site is read as a symbol of its own, so this one is not the key
// `exp.js` registered. TypeScript reads `Symbol.for` the same way.
const reRegistered = Symbol.for('registered');
table[reRegistered]; // ERROR: not a key of `table`

// An inferred symbol remains fresh across a module boundary, so natural
// inference can widen it when it is copied into a mutable position.
let mutableShared = shared;
mutableShared = Symbol(); // OK

const holder = {shared};
holder.shared = Symbol(); // OK
