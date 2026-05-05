// There is a check in Flow that enforces that 'group' is a subtype of the type
// of `a`. Here this would fail because 'group' <: A. Given that this is a common
// pattern, however, we relax the check and perform it against the bound of A,
// instead.

function a<A extends 'a'>(a: A): void {
    if (a === 'a'){ }
  }

  function b<A extends 'a', B extends A>(b: B): void {
    if (b === 'a'){ }
  }

  function a1<A extends 'a'>(a: A): void {
    if (a === 'b'){ } // error 'b' not compatible with 'a'
  }

  function b1<A extends 'a', B extends A>(b: B): void {
    if (b === 'b'){ } // error 'b' not compatible with 'a'
  }

  export type S = {|
    p: number,
  |};

  export type T = {|
    q: string,
    ...S,
  |};

  function c<X extends T>(x: $Keys<X>) {
    switch (x) {
      case 'p':
        break;
      case 'q':
        break;
      case 'r': // error
        break;
    }
  }
