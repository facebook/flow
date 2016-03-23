/* Object.values returns an array of the object's own properties in iteration
 * order.
 *
 * Ideally, we would return a tuple with specific types at specific indices.
 * However, it is not currently possible to soundly provide a more specific
 * return type than Array<mixed>, for two reasons:
 *
 * 1. Iteration order. Flow's object types do not enforce consistency in
 * iteration order, so an object type {a:A,b:B} can be satisfied by the object
 * {a,b} and the object {b,a}. This prevents us from returning [A,B].
 *
 * 2. Subtyping. An object {a,b,c} can flow into an object type {a:A,b:B}.  This
 * prevents us from returning [A,B] given such an object type, because the
 * object at runtime may have more properties.
 *
 * It is possible to return a more specific type, but only in the case of sealed
 * object literals, for which we statically know the complete set of values and
 * can statically determine the iteration order (numeric first in sort order,
 * then the rest in declaration order). Building the machinery to allow this is
 * left for future work.
 *
 * @flow
 */

class A {}
class B {}
class C {}
class D {}

// Literals

var sealed_literal = { b: C, "2": B, a: D, "1": A };
Object.values(sealed_literal).forEach(v => {
  (v: void); // errors: A|B|C|D ~> void
});

var unsealed = {};
Object.values(unsealed).forEach(v => {
  (v: void); // error: mixed ~> void
});


// Dictionaries

var dict: {[k:number]: string} = {}
Object.values(dict).forEach(v => {
  // TODO: should be OK, currently mixed ~> string error
  (v: string);
  // TODO: should be string ~> void error, currently mixed ~> void error
  (v: void);
});
// The TODO behavior above only holds if the following is an error, because it
// assumes that no properties can be added to `dict` unless they have `string`
// values.
dict.foo = 0; // error: `foo` is a string, incompatible with `k`


var mixed_dict: {[k:number]: string, foo: number} = {foo: 0};
Object.values(mixed_dict).forEach(v => {
  // TODO: should be OK, currently mixed ~> string|number error
  (v: string|number);
  // TODO: should be string|number ~> void errors, currently mixed ~> void error
  (v: void);
});
// The TODO behavior above only holds if the following is an error, because it
// assumes that no properties can be added to `dict` unless they have `string`
// values, or have the key `foo` and a `number` value.
mixed_dict.bar = true; // error: `bar` is a string, incompatible with `k`


// Object types

function object_type(o: { foo: string, bar: number }) {
  Object.values(o).forEach(v => {
    // It's unsafe to assume `v` is `string|number`. Subtyping rules allow
    // callers to pass an object with at least foo/bar:string/number, but may
    // pass more.
    (v: void); // error: mixed ~> void
  });
}
// The behavior above is necessary because the following is allowed.
object_type({ foo: "", bar: 0, baz: true });

function any_object(o: Object) {
  Object.values(o).forEach(v => {
    // TODO: Should be OK (only possible if v is any)
    // currently mixed ~> string, mixed ~> number
    (v: string);
    (v: number);
  });
}
