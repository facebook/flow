// --- Literal string ---

type L1 = Lowercase<'FOO'>;
'foo' as L1; // OK
'FOO' as L1; // ERROR

// --- Already lowercase: identity ---

type L2 = Lowercase<'foo'>;
'foo' as L2; // OK

// --- Mixed case ---

type L3 = Lowercase<'FooBar'>;
'foobar' as L3; // OK
'FooBar' as L3; // ERROR

// --- Empty string ---

type L4 = Lowercase<''>;
'' as L4; // OK

// --- Distribution over union ---

type L5 = Lowercase<'FOO' | 'bar'>;
'foo' as L5; // OK
'bar' as L5; // OK
'BAR' as L5; // ERROR

// --- `Lowercase<string>` is the set of already-lowercase strings ---

type L6 = Lowercase<string>;
'anything' as L6; // OK
'ANYTHING' as L6; // ERROR (not lowercase)
{
  declare const x: L6;
  x as string; // OK
}

// --- Template literal: quasis AND interpolated `string` types are lowercased ---

type L7 = Lowercase<`ABC${string}`>;
'abc' as L7; // OK (inner empty)
'abcxyz' as L7; // OK (inner is lowercase)
'abcXYZ' as L7; // ERROR (inner not lowercase)
'ABCxyz' as L7; // ERROR (prefix wrong case)

// --- Idempotency ---

type L8 = Lowercase<Lowercase<'FOO'>>;
'foo' as L8; // OK

// --- Round trip ---

type L9 = Lowercase<Uppercase<'mixed'>>;
'mixed' as L9; // OK

// --- Generic preservation: `Lowercase<T>` survives substitution and the
//     constraint is enforced once T is bound. ---

type Y<T extends 'FOO' | 'BAR'> = Lowercase<T>;
'foo' as Y<'FOO'>; // OK
'FOO' as Y<'FOO'>; // ERROR
'anything' as Y<'FOO'>; // ERROR

// --- Arity errors ---

type Bad1 = Lowercase; // ERROR
type Bad2 = Lowercase<>; // ERROR
type Bad3 = Lowercase<'a', 'b'>; // ERROR

// --- Constraint ---

type Bad4 = Lowercase<42>; // ERROR
