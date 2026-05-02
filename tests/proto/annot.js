type O = {p: number, __proto__: {q: string, ...}, ...};
declare var o: O;
o.p as empty; // error: number ~> empty
o.q as empty; // error: string ~> empty

({p: 0, q: ""} as O); // ok, lower `q` is own, but compatible with upper proto's `q`
({p: 0, __proto__: {q: ""}} as O); // ok
({p: 0, q: 0} as O); // error: number ~> string
({p: 0, __proto__: {q: 0}} as O); // error: number ~> string
({p: 0} as O); // error, property `q` not found

// __proto__? treated like a normal property
type O_optional = { __proto__?: { q: 0, ... }, ... };
declare var o_optional: O_optional;
o_optional.q as empty; // error: property `q` not found
o_optional.__proto__ as empty; // error: void ~> empty, object type ~> empty

// +__proto__ and -__proto__ treated like a normal property
type O_variance = { +__proto__: { q: 0, ... }, ... };
declare var o_variance: O_variance;
o_variance.q as empty; // error: property `q` not found
o_variance.__proto__ as empty; // error: object type ~> empty

// __proto__ for callable objects is an error
type O_callable = { (): void, __proto__: {...}, ... }; // error: unexpected proto after call
declare var o_callable: O_callable;
o_callable.q as empty; // error: property `q` not found
o_callable.__proto__ as empty; // error: function proto ~> empty

// __proto__() treated like a normal (function-valued) property
type O_method = { __proto__(): void, ... };
declare var o_method: O_method;
o_method.__proto__ as empty; // error: function ~> empty

type O_loop = { p: 0, __proto__: O_loop, ... };
declare var o_loop: O_loop;
o_loop.p as empty; // error: number ~> empty
o_loop.q as empty; // TODO: error (pruned at constraint cache)

type O_invalid = { __proto__: number, ... }; // error: number is not a valid proto

type O_multi = {
  __proto__: {...},
  __proto__: {...}, // error: multiple protos
  ...
}
