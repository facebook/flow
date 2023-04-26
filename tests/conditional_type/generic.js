// We make conditional types where we cannot decide which branch to take uninhabitable.
function generic_in_check_type<T>(x: T) : [T] extends [string] ? boolean : number {
  if (typeof x === 'string') {
    return true; // error
  } else {
    return 1; // error
  }
}

(generic_in_check_type(''): boolean); // ok
(generic_in_check_type(1): number); // ok
(generic_in_check_type(''): empty); // error
(generic_in_check_type(1): empty); // error

function generic_in_extends_type<T>(x: T): [string] extends [T] ? boolean : number {
  if (typeof x === 'string') {
    return true; // error
  } else {
    return 1; // error
  }
}

(generic_in_extends_type(''): boolean); // ok
(generic_in_extends_type(1): number); // ok
(generic_in_extends_type(''): empty); // error
(generic_in_extends_type(1): empty); // error

function assign_from_generic_conditional_type<T>(x: string extends T ? boolean : number): void {
  const y: string = x; // error
}
assign_from_generic_conditional_type<number>(0); // ok

function generic_conditional_type_subtyping<T, S>(
  x: string extends T ? boolean : number,
  y: string extends S ? boolean : number,
  z: string extends T ? boolean : number,
) {
  x = y; // expected error
  x = z; // unfortunate error, but this is sound.
}

function definitely_assignable_choose_true_branch<T>(x: T): Array<T> extends $ReadOnlyArray<infer X> ? X : number {
  return x; // ok
}

(definitely_assignable_choose_true_branch(''): string); // ok
(definitely_assignable_choose_true_branch(''): number); // error: string ~> number

function definitely_not_assignable_choose_false_branch<T>(x: T): Set<T> extends Array<infer X> ? X : string {
  return ''; // ok
}

(definitely_not_assignable_choose_false_branch(''): string); // ok
(definitely_not_assignable_choose_false_branch(''): number); // error: string ~> number
