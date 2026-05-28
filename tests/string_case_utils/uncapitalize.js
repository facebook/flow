// --- Literal string: single-uppercase source ---

type U1 = Uncapitalize<'Foo'>;
'foo' as U1; // OK
'Foo' as U1; // ERROR (still capital)
'fOO' as U1; // ERROR

// --- Multi-uppercase source: ONLY the first char is lowercased; rest preserved ---

type U1b = Uncapitalize<'FOO'>;
'fOO' as U1b; // OK
'foo' as U1b; // ERROR (Uncapitalize does not lower the tail)
'FOO' as U1b; // ERROR (first char must be lowercased)

// --- Already uncapitalized ---

type U2 = Uncapitalize<'foo'>;
'foo' as U2; // OK

// --- Single char ---

type U3 = Uncapitalize<'A'>;
'a' as U3; // OK

// --- Empty: no change ---

type U4 = Uncapitalize<''>;
'' as U4; // OK

// --- Non-letter first char ---

type U5 = Uncapitalize<'1ABC'>;
'1ABC' as U5; // OK

// --- Distribution over union ---

type U6 = Uncapitalize<'FOO' | 'BAR'>;
'fOO' as U6; // OK
'bAR' as U6; // OK
'FOO' as U6; // ERROR

// --- `Uncapitalize<string>` is the set of strings whose first char is already
//     lowercase (or non-letter) ---

type U7 = Uncapitalize<string>;
'anything' as U7; // OK
'Anything' as U7; // ERROR (first char is uppercase)

// --- Template literal: only first char of first non-empty quasi changes ---

type U8 = Uncapitalize<`ABC${string}`>;
'aBC' as U8; // OK (inner empty)
'aBCxyz' as U8; // OK (rest of string can be anything since inner is `string`)
'aBCXYZ' as U8; // OK
'ABCxyz' as U8; // ERROR (first char must be lowercase)

// --- Empty first quasi: leading character is the start of the interpolated
//     type, so the transform is pushed inside it. When the interpolation can
//     be empty (e.g. `string`), the result must also accept the suffix's first
//     char being lowercased. ---

type U8b = Uncapitalize<`${string}ABC`>;
'aBC' as U8b; // OK (interpolation is empty; A → a)
'ABC' as U8b; // ERROR (first char must be lowercase)
'xABC' as U8b; // OK (x is lowercase; rest of inner can be anything)
'XABC' as U8b; // ERROR (first char of inner is uppercase)

// --- Round trip ---

type U9 = Uncapitalize<Capitalize<'foo'>>;
'foo' as U9; // OK

// --- Arity ---

type Bad1 = Uncapitalize; // ERROR
type Bad2 = Uncapitalize<>; // ERROR
type Bad3 = Uncapitalize<'a', 'b'>; // ERROR

// --- Constraint ---

type Bad4 = Uncapitalize<42>; // ERROR
