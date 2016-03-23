/* Object.keys returns an array of the object's own keys in iteration order.
 *
 * This method is currently unsound, but the unsound implementation has been
 * left in place until a solution can be found which is better than just
 * simplifying the type.
 *
 * Ideally, we would return a tuple with specific string literal types at
 * specific indices, but it is currently unsound to provide a more specific
 * return type than Array<string>, for two reasons:
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

// Literals

var sealed_literal = { b: null, "2": null, a: null, "1": null };
Object.keys(sealed_literal).forEach(k => {
  (k: void); // errors: 1|2|a|b ~> void
});

var unsealed = {};
Object.keys(unsealed).forEach(k => {
  (k: void) // error: string ~> void
});


// Dictionaries

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  (k: void) // error: string ~> void
});


// Object types

function object_type(o: { foo: string, bar: number }) {
  Object.keys(o).forEach(k => {
    // It's unsafe to assume `k` is `"foo"|"bar"`. Subtyping rules allow callers
    // to pass an object with at least foo/bar, but may pass more.
    // TODO should be string ~> void, currently foo|bar ~> void
    (k: void); // error: string ~> void
  });
}

function any_object(o: Object) {
  Object.keys(o).forEach(k => {
    (k: void); // error: string ~> void
  });
}
