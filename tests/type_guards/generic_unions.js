// A generic that satisfies the guard is refined away in the negative branch, even when its bound
// alone does not satisfy the guard.

function union_of_generics<
  First extends {foo: string},
  Second extends {bar: string},
  SubFirst extends First,
>(x: First | SubFirst | Second, isFirst: (value: First | Second) => value is First) {
  if (isFirst(x)) {
    x as First; // okay
  } else {
    x as Second; // okay
    x as First; // error Second ~> First
  }
}

function guard_is_type_param<T>(x: T | number, isT: (x: unknown) => x is T) {
  if (isT(x)) {
    x as T; // okay
  } else {
    x as number; // okay
    x as T; // error number ~> T
  }
}

declare function isString(x: unknown): x is string;

function any_bound<T extends any>(x: T) {
  if (isString(x)) {
    x.length as empty; // error number ~> empty
  }
}

function any_in_enum_bound<T extends 'a' | 'b' | any>(x: T) {
  if (isString(x)) {
    x.length as empty; // error number ~> empty
  }
}
