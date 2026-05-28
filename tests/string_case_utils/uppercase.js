// --- Literal string ---

type U1 = Uppercase<'foo'>;
'FOO' as U1; // OK
'foo' as U1; // ERROR

// --- Already uppercase: identity ---

type U2 = Uppercase<'FOO'>;
'FOO' as U2; // OK

// --- Mixed case ---

type U3 = Uppercase<'fooBar'>;
'FOOBAR' as U3; // OK
'fooBar' as U3; // ERROR

// --- Empty string ---

type U4 = Uppercase<''>;
'' as U4; // OK
'A' as U4; // ERROR

// --- Distribution over union ---

type U5 = Uppercase<'foo' | 'bar'>;
'FOO' as U5; // OK
'BAR' as U5; // OK
'foo' as U5; // ERROR
'baz' as U5; // ERROR

// --- `Uppercase<string>` is the set of already-uppercase strings, so concrete
//     literals must already be uppercase to pass — but every member of it is
//     still a `string`. ---

type U7 = Uppercase<string>;
{
  declare const x: U7;
  x as string; // OK (narrower flows to wider)
}
'ANYTHING' as U7; // OK
'anything' as U7; // ERROR (not in canonical uppercase form)

// --- Template literal: quasis AND interpolated string types are uppercased.
//     `Uppercase<`abc${string}`>` is `\`ABC${Uppercase<string>}\``, so the
//     `${string}` portion is constrained to already be uppercase. ---

type U9 = Uppercase<`abc${string}`>;
'ABC' as U9; // OK (inner is empty string)
'ABCXYZ' as U9; // OK (inner is uppercase)
'ABCxyz' as U9; // ERROR (inner is not uppercase)
'abcXYZ' as U9; // ERROR (prefix has wrong case)

// --- Generic preservation: `Uppercase<T>` survives substitution and the
//     constraint is enforced once T is bound. ---

type X<T extends 'foo' | 'bar'> = Uppercase<T>;
'FOO' as X<'foo'>; // OK
'foo' as X<'foo'>; // ERROR
'anything' as X<'foo'>; // ERROR

// --- Nested casing ---

type U10 = Uppercase<Lowercase<'AbC'>>;
'ABC' as U10; // OK

// --- Idempotency ---

type U11 = Uppercase<Uppercase<'foo'>>;
'FOO' as U11; // OK

// --- Subtype of `string` ---

{
  declare const x: Uppercase<'foo'>;
  x as string; // OK
  x as 'FOO'; // OK
}

// --- Numbers/booleans inside template are still constrained on the value side ---

type U12 = Uppercase<`v${1 | 2}`>;
'V1' as U12; // OK
'V2' as U12; // OK
'V3' as U12; // ERROR

// --- Arity errors ---

type Bad1 = Uppercase; // ERROR (missing type arg)
type Bad2 = Uppercase<>; // ERROR (zero type args)
type Bad3 = Uppercase<'a', 'b'>; // ERROR (too many)

// --- Constraint: arg must extend string ---

type Bad4 = Uppercase<42>; // ERROR
type Bad5 = Uppercase<{a: 1}>; // ERROR

// --- Type sig export ---

declare export const u: Uppercase<'foo'>;
