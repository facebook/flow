// TODO: generics in conditional types are broken
function generic_in_check_type<T>(x: T) : [T] extends [string] ? boolean : number {
  if (typeof x === 'string') {
    return true; // error
  } else {
    return 1; // TODO: should error
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
    return 1; // TODO: should error
  }
}

(generic_in_extends_type(''): boolean); // ok
(generic_in_extends_type(1): number); // ok
(generic_in_extends_type(''): empty); // error
(generic_in_extends_type(1): empty); // error
