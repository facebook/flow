// `typeof s` for a `unique symbol` binding resolves to that specific unique
// symbol, never the generic `symbol` type. If it widened to `symbol`, neither
// cast below would error: a distinct `unique symbol` would satisfy it, and a
// generic `symbol` would too.
declare const s: unique symbol;
declare const t: unique symbol;
declare const gen: symbol;

s as typeof s; // OK: the same unique symbol
t as typeof s; // ERROR: a distinct unique symbol, not merged into `symbol`
gen as typeof s; // ERROR: generic `symbol` is not the unique symbol `s`
