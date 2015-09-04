/* regression tests */

function rest_array<T>(...xs: Array<T>): T {
  return xs[0];
}

// Warn, singleton tuple types don't represent rest params
function rest_tuple<T>(...xs: [T]): T {
  return xs[0];
}

function rest_any(...xs: any): any {
  return xs[0];
}

/* TODO -- mk_rest unifies T with Array<tvar>, but T is BoundT
function rest_t<U, T: Array<U>>(...xs: T): U {
  return xs[0];
}
*/
