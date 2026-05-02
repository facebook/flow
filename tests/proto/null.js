type O_null = { __proto__: null, ... }
({} as O_null); // OK, similar to width subtyping we "forget" about Object.prototype

var o_null = { p: 0, __proto__: null }
o_null.toString(); // property `toString` not found

o_null as O_null; // OK

// NB: can't use __proto__ getter on null-proto object!
Object.getPrototypeOf(o_null) as null; // OK, NullProtoT is the same as NullT

declare var o_nonstrict: { __proto__: null, ... };
if (o_nonstrict.p) { // Error - property p is unknown
  o_nonstrict.p as empty; // error: mixed ~> empty
}
