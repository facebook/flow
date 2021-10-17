// @flow
//
// Tests for subtyping PolyT ~> PolyT.

// Test issue #8766: we'd accept when the subtyping looked OK with the
// upper tparams set to their bounds, even if it wasn't OK for other
// possible arguments.
{
  (o: <T>() => mixed): (<S>() => S) => o; // error

  function coerce<A, B>(x: A): B {
    const g = (o: <T>() => A): (<S>() => S) => o; // error
    return g(<T>() => x)();
  }

  const a: number = coerce('a');
  a.toFixed();
}

// Test issue #8766, round 2.
{
  (f: <T>(T) => mixed): (<S>(mixed) => S) => f; // error

  function coerce<A, B>(x: A): B {
    const g = (f: <T>(T) => mixed): (<S>(mixed) => S) => f; // error
    return g(<T>(y: T) => x)();
  }

  (coerce('a'): empty);
  coerce('a').toFixed();
}

// Test another symptom of #8766: we'd reject a subtyping when setting
// the lower tparams to their bounds didn't make it work, even if some
// other setting would work fine.
{
  (f: <T>(T) => T): (<S>(number) => number) => f; // ok

  const f: <T>(T) => T = <T>(x: T): T => x;
  f(3);
  (f: <S>(number) => number); // ok
  (f: <S>(number) => number)(3); // ok
}


// Test subtyping where lower has more parameters than upper…
(f: <R, S>(R, S) => S): (<T>(T, T) => T) => f; // ok
(f: <R, S>(R, S) => S): (<T>(T, T) => boolean) => f; // error

// … or fewer.
(f: <R>(R, mixed) => R): (<S, T>(S, T) => S) => f; // ok
(f: <R>(R, mixed) => R): (<S, T>(S, T) => T) => f; // error


// Test subtyping where only upper is polymorphic…
(f: mixed => boolean): (<T>(T) => boolean) => f; // ok
(f: (number, number) => number): (<T>(T, T) => T) => f; // error

// … or only lower.
(f: <T>(T, T) => T): ((number, number) => number) => f; // ok
(f: <T>(T, T) => T): ((number, string) => number) => f; // error
