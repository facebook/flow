//
// Tests for subtyping PolyT ~> PolyT.

// Unsoundness issue #8766: we'd accept when the subtyping looked OK
// with the upper tparams set to their bounds, even if it wasn't OK
// for other possible arguments.
{
  (o: <T>() => unknown): (<S>() => S) => o; // should error

  function coerce<A, B>(x: A): B {
    const g = (o: <T>() => A): (<S>() => S) => o; // should error
    return g(<T>(): A => x)();
  }

  const a: number = coerce('a');
  a.toFixed();
}

// Test issue #8766, round 2.
{
  (f: <T>(T) => unknown): (<S>(unknown) => S) => f; // should error

  function coerce<A, B>(x: A): B {
    const g = (f: <T>(T) => unknown): (<S>(unknown) => S) => f; // should error
    return g(<T>(y: T): A => x)();
  }

  coerce('a') as empty;
  coerce<_, number>('a').toFixed();
}

// Test issue #8766, round 3 -- this time also testing for a subtle
// pitfall in implementing the rule ∀-local / S-All-Loc from the
// polymorphic-subtyping literature:
//   https://github.com/facebook/flow/pull/8767#issuecomment-949402649
{
  const g = (f: <S, T extends S>(T) => T): <S, T>(T) => S => f; // should error
  const coerce: <B, A>(A) => B = g(<S, T extends S>(x: T): T => x);
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
(f: <R>(R, unknown) => R): (<S, T>(S, T) => S) => f; // ok
(f: <R>(R, unknown) => R): (<S, T>(S, T) => T) => f; // error


// Test subtyping where lower and upper have same number of parameters,
// but they don't line up when paired off in order.  These are rejected
// by the same approximation that causes us to reject fewer or more parameters.
(f: <S extends string, T extends number>() => { a: S, b: T })
  : <T extends number, S extends string>() => { a: S, b: T } => f; // ideally ok, known error


// Test subtyping where only one side is polymorphic.
(f: unknown => boolean): (<T>(T) => boolean) => f; // ok
(f: (number, number) => number): (<T>(T, T) => T) => f; // error
(f: <T>(T, T) => T): ((number, number) => number) => f; // ok
(f: <T>(T, T) => T): ((number, string) => number) => f; // error
