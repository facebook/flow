function basic(
  fn: (x: mixed) => x is number,
  y: mixed,
) {
  if (fn(y)) {
    (y: number); // okay
    (y: string); // error number ~> string
  }
}

function nonMaybe(
  fn: <T>(x: ?T) => x is T,
  x1: string,
  x2: string | null,
  x3: string | void,
  x4: ?string,
) {
  if (fn(x1)) {
    (x1: string);
    (x1: number); // error string ~> number
  }
  if (fn(x2)) {
    (x2: string);
    (x2: number); // error string ~> number
  }
  if (fn(x3)) {
    (x3: string);
    (x3: number); // error string ~> number
  }
  if (fn(x4)) {
    (x4: string);
    (x4: number); // error string ~> number
  }
}

function array_filter() {

  declare class ReadOnlyArray_<+T> {
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter<This, S: T>(predicate: (this: This, value: T, index: number, array: $ReadOnlyArray<T>) => value is S, thisArg?: This): Array<S>;
    filter<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg : This): Array<T>;
  }

  declare var arr: ReadOnlyArray_<mixed>;

  const arr1: Array<number> = arr.filter((x: mixed): x is number => { return typeof x === "number"; });
  const arr2: Array<string> = arr.filter((x: mixed): x is number => { return typeof x === "number"; }); // error
  const arr3: Array<string> = arr.filter((x: mixed) => { return typeof x === "number"; }); // error no refinement
}


function sentinel_simple() {
  type A = { tag: 'A', foo: number };
  type B = { tag: 'B', foo: string };

  declare var x: A | B;

  declare function sentinel_A(x: mixed): x is {tag: 'A', ...};
  if (sentinel_A(x)) {
      (x: A); // okay A ~> A
      (x: B); // error A ~> B
  }

  declare function invalid_sentinel_val(x: mixed): x is {tag: 'C', ...};
  if (invalid_sentinel_val(x)) {
      (x: A); // okay empty ~> A
      (x: B); // okay empty ~> B
  }

  declare function invalid_sentinel_key(x: mixed): x is {tag_: 'A', ...};
  if (invalid_sentinel_key(x)) {
      (x: A); // error B & {tag_: 'A',...} ~> A
      (x: B); // error A & {tag_: 'A',...} ~> B
  }
}

function sentinel_readonly() {
  type A = $ReadOnly<{ tag: 'A', foo: number }>;
  type B = $ReadOnly<{ tag: 'B', foo: string }>;

  declare var x: A | B;

  declare function sentinel_A(x: mixed): x is $ReadOnly<{tag: 'A', ...}>;
  if (sentinel_A(x)) {
      (x: A); // okay A ~> A
      (x: B); // error A ~> B
  }

  declare function invalid_sentinel_val(x: mixed): x is $ReadOnly<{tag: 'C', ...}>;
  if (invalid_sentinel_val(x)) {
      (x: A); // okay empty ~> A
      (x: B); // okay empty ~> B
  }

  declare function invalid_sentinel_key(x: mixed): x is $ReadOnly<{tag_: 'A', ...}>;
  if (invalid_sentinel_key(x)) {
      (x: A); // error B & {+tag_: 'A',...} ~> A
      (x: B); // error A & {+tag_: 'A',...} ~> B
  }
}

function sentinel_multi_tag() {
  type A = $ReadOnly<{ tag1: 'A', tag2: 1, foo: number }>;
  type B = $ReadOnly<{ tag1: 'B', tag2: 1, foo: string }>;

  declare var x: A | B;

  declare function unknown_sentinel(x: mixed): x is $ReadOnly<{tag: 'C', ...}>;
  if (unknown_sentinel(x)) {
      (x: A); // error B & {+tag: 'C', ...} ~> A
      (x: B); // error A & {+tag: 'C', ...} ~> B
  }

  declare function sentinel_A_1(x: mixed): x is $ReadOnly<{ tag1: 'A', tag2: 1, ...}>;
  if (sentinel_A_1(x)) {
      (x: A); // okay A ~> A
      (x: B); // error A ~> B
  }

  declare function sentinel_1(x: mixed): x is $ReadOnly<{ tag2: 1, ...}>;
  if (sentinel_1(x)) {
      (x: A); // error B ~> A
      (x: B); // error A ~> B
  }
}


function negation() {
  declare function isNumber(x: mixed): x is number;
  declare function isString(x: mixed): x is string;

  declare var x: number | string;

  if (isNumber(x)) {
    (x: number); // okay
    (x: string); // error number ~> string
  } else {
    (x: string); // okay
    (x: number); // error string ~> number
  }

  if (!isNumber(x)) {
    (x: string); // okay
    (x: number); // error string ~> number
  } else {
    (x: string); // error string ~> number
    (x: number); // okay
  }

  if (isNumber(x) || isString(x)) {
    (x: number); // error string ~> number
    (x: string); // error number ~> string
  } else {
    (x: string); // okay empty ~> string
    (x: number); // okay empty ~> number
  }
}

function any_input() {
  declare function isNotMaybe<T>(value: ?T): value is T;

  declare var maybeAny: ?any;

  if (isNotMaybe(maybeAny)) {
    (maybeAny: empty); // okay, we're filtering null and undefined
  }
}

function any_filter() {
  declare function isAny(value: mixed): value is any;

  declare var maybeNumber: number;

  if (isAny(maybeNumber)) {
    (maybeNumber: empty); // error number ~> empty
  }
}

function refine_any_to_type() {
  declare function isNumber(value: mixed): value is number;

  declare var anyVal: any;

  if (isNumber(anyVal)) {
    (anyVal: empty); // error number ~> empty
  }
}

function opaque_types() {
  declare function isTruthy<A>(value: ?A): value is A;

  declare class $FbtResultBase {}
  declare opaque type FbtString: string;
  declare type FbtElement = $FbtResultBase;
  declare type FbtWithoutString = FbtString | FbtElement;
  declare type Fbt = string | FbtWithoutString;

  declare var x: void | Fbt;

  if (isTruthy(x)) {
    (x: Fbt); // okay
    (x: empty); // error Fbt ~> empty
  }
}

import type {OpaqueOrString} from './opaque_exports';

function opaque_types_imported() {
  declare function isTruthy<A>(value: ?A): value is A;

  declare var x: void | OpaqueOrString;

  if (isTruthy(x)) {
    (x: OpaqueOrString); // TODO okay, currently imported opaque type is not filtered because it's underlying type is opaque to type-guard filtering
  }
}

function union_with_any() {
  declare function isTruthy<A>(value: ?A): value is A;

  declare var x: void | string | any;

  if (isTruthy(x)) {
    (x: string); // okay, type guard filters out void
    (x: empty); // error string ~> empty
  }
}
