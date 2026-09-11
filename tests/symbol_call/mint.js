// A call to the global `Symbol`, or to its registry lookup `Symbol.for`, is the
// one expression that returns a symbol nothing else can return. Where such a
// call initializes a `const`, the binding holds that one symbol for good, so
// the symbol is given an identity and can be used as a property key. Anywhere
// else the call keeps the ordinary `symbol`, since one call site would there
// stand for values that are distinct at runtime. TypeScript mints one for the
// same two callees in the same position.

const minted = Symbol();
const described = Symbol('a description');
const registered = Symbol.for('a key');

const obj = {[minted]: 1, [described]: 2, [registered]: 3};
obj[minted] as number; // OK
obj[described] as number; // OK
obj[registered] as number; // OK

// Each declarator of one statement is its own binding, so each call mints a
// symbol of its own.
const first = Symbol(), second = Symbol();
const pair = {[first]: 1, [second]: 2};
pair[first] as number; // OK
pair[second] as number; // OK
const onlyFirst = {[first]: 1};
onlyFirst[second]; // ERROR: a different symbol

// A second call is a second symbol, so it is not a key of the object above.
// The description is not the identity: two calls that describe themselves the
// same way are still two symbols.
const other = Symbol('a description');
obj[other]; // ERROR: not a key of `obj`

// A binding that can be reassigned could be pointed at another symbol, so the
// call keeps the ordinary `symbol`, which names no property.
let rebindable = Symbol();
rebindable = Symbol();
const fromLet = {[rebindable]: 1}; // ERROR: `symbol` is not a key

var alsoRebindable = Symbol();
const fromVar = {[alsoRebindable]: 1}; // ERROR: `symbol` is not a key

// An annotation says what the binding holds, and `symbol` is what this one
// says.
const annotated: symbol = Symbol();
const fromAnnotated = {[annotated]: 1}; // ERROR: `symbol` is not a key

// A call mints where the binding holds what that call returned. Inside an
// object or an array the binding holds the container instead, of which the
// symbol is one part among many, so the call mints nothing.
const inObject = {k: Symbol()};
const fromObject = {[inObject.k]: 1}; // ERROR: `symbol` is not a key

const inArray = [Symbol()];
const fromArray = {[inArray[0]]: 1}; // ERROR: `symbol` is not a key

const fromNothing = {[Symbol()]: 1}; // ERROR: `symbol` is not a key

// Reached through a comma the call is nested too, however little the comma
// changes what runs.
const throughComma = (0, Symbol());
const fromComma = {[throughComma]: 1}; // ERROR: `symbol` is not a key

// A function body runs on every call, so what it returns is a fresh symbol
// each time and `symbol` is all its result type can say.
function mk() {
  return Symbol();
}
const fromCall = mk();
const fromFunction = {[fromCall]: 1}; // ERROR: `symbol` is not a key

// A conditional mints once per branch, the same way Flow already reads a
// conditional of literals as one type per branch, so the binding holds one
// symbol or the other. TypeScript widens the whole conditional to `symbol`
// instead. Keying an object by a union of symbols then reads it as having every
// one of them, which is how a union key has always been read, whatever it was
// built from, and is more than the object holds at runtime.
declare const test: boolean;
const branched = test ? Symbol() : Symbol();
const fromBranch = {[branched]: 1};
fromBranch[branched] as number; // OK

// A `match` expression is the other place a binding takes one value per branch,
// and it mints the same way.
const matched = match (test) {
  true => Symbol(),
  _ => Symbol(),
};
const fromMatch = {[matched]: 1};
fromMatch[matched] as number; // OK

// `??`, `||` and `&&` are the other way a binding takes one operand or the
// other, and a call in one of their operands mints for the same reason.
declare const maybeKey: typeof minted | null;
const coalesced = maybeKey ?? Symbol();
const fromCoalesce = {[coalesced]: 1};
fromCoalesce[coalesced] as number; // OK
fromCoalesce[minted] as number; // OK: the left operand is one of the two keys

const ored = maybeKey || Symbol();
const fromOr = {[ored]: 1};
fromOr[ored] as number; // OK

// `&&` mints the same way, though what it holds when the left operand decides
// is that operand, which names no property. Only the left operand is reported
// on below: had the call not minted, `symbol` would be reported too.
const anded = test && Symbol();
const fromAnd = {[anded]: 1}; // ERROR: `boolean` is not a key

// A call that runs more than once is one symbol for all of its results, since
// the identity is the call's location and the location does not change with the
// run. Two objects built by two calls of `mkKeyed` hold two symbols at runtime
// and are read as holding one, which is the price of deciding this from the
// source alone. TypeScript settles for the same gap.
function mkKeyed() {
  const k = Symbol();
  return {o: {[k]: 1}, read: (x: {[k]: number}) => x[k]};
}
mkKeyed().read(mkKeyed().o); // OK, and the two symbols are not the same one

// A loop body is the same call site on every turn, so the binding it declares
// is the one symbol on every turn.
for (let i = 0; i < 3; i++) {
  const k = Symbol();
  const o = {[k]: i};
  o[k] as number; // OK
}

// An inferred unique symbol is fresh in the same way as an inferred primitive
// literal: later contexts decide whether to retain its precision. Mutable
// bindings, mutable object properties, and unconstrained generic inference all
// widen it back to `symbol`.
let widenedAlias = minted;
widenedAlias = Symbol(); // OK

const mutableHolder = {key: minted};
mutableHolder.key = Symbol(); // OK

declare function useState<T>(initial: T): [T, (next: T) => void];
const [state, setState] = useState(minted);
state as symbol; // OK
setState(Symbol()); // OK
