// @flow
//
// Tests for subtyping PolyT ~> PolyT.

// Unsoundness issue #8766: we'd accept when the subtyping looked OK
// with the upper tparams set to their bounds, even if it wasn't OK
// for other possible arguments.
{
  (o: <T>() => mixed): (<S>() => S) => o; // should error

  function coerce<A, B>(x: A): B {
    const g = (o: <T>() => A): (<S>() => S) => o; // should error
    return g(<T>() => x)();
  }

  const a: number = coerce('a');
  a.toFixed();
}

// Test issue #8766, round 2.
{
  (f: <T>(T) => mixed): (<S>(mixed) => S) => f; // should error

  function coerce<A, B>(x: A): B {
    const g = (f: <T>(T) => mixed): (<S>(mixed) => S) => f; // should error
    return g(<T>(y: T) => x)();
  }

  (coerce('a'): empty);
  coerce('a').toFixed();
}

// Test issue #8766, round 3 -- this time also testing for a subtle
// pitfall in implementing the rule ∀-local / S-All-Loc from the
// polymorphic-subtyping literature:
//   https://github.com/facebook/flow/pull/8767#issuecomment-949402649
{
  const g = (f: <S, T: S>(T) => T): <S, T>(T) => S => f; // should error
  const coerce: <B, A>(A) => B = g(<S, T: S>(x: T): T => x);
}


// Conversely, we reject (but ideally shouldn't) a subtyping when it
// requires setting the lower tparams to particular types, or types in
// a particular relation to the upper tparams, other than setting the
// lower tparams to just equal the upper tparams.
{
  // This rejects because it requires (T => T) ~> (number => number)
  // for a generic T.
  (f: <T>(T) => T): (<S>(number) => number) => f; // should be ok, currently isn't

  // The same example would work with the upper type monomorphized
  // by substituting its parameter S, for any type S we choose:
  (f: <T>(T) => T): (number => number) => f; // ok

  // This rejects because it requires it to work with T = S,
  // whereas it can only work with T = S[].
  (f: <T>(T) => T): (<S>(S[]) => S[]) => f; // should be ok, currently isn't
}


// Test subtyping where lower has fewer or more parameters than upper.
// Currently these don't work; see #8767.
(f: <R, S>(R, S) => S): (<T>(T, T) => T) => f; // ok
(f: <R, S>(R, S) => S): (<T>(T, T) => boolean) => f; // error
(f: <R>(R, mixed) => R): (<S, T>(S, T) => S) => f; // ok
(f: <R>(R, mixed) => R): (<S, T>(S, T) => T) => f; // error


// Test subtyping where lower and upper have same number of parameters,
// but they don't line up when paired off in order.  These are rejected
// by the same approximation that causes us to reject fewer or more parameters.
(f: <S: string, T: number>() => { a: S, b: T })
  : <T: number, S: string>() => { a: S, b: T } => f; // ideally ok, known error


// Test subtyping where only one side is polymorphic.
(f: mixed => boolean): (<T>(T) => boolean) => f; // ok
(f: (number, number) => number): (<T>(T, T) => T) => f; // error
(f: <T>(T, T) => T): ((number, number) => number) => f; // ok
(f: <T>(T, T) => T): ((number, string) => number) => f; // error
