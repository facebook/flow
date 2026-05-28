// --- Literal string ---

type C1 = Capitalize<'foo'>;
'Foo' as C1; // OK
'foo' as C1; // ERROR
'FOO' as C1; // ERROR

// --- Already capitalized ---

type C2 = Capitalize<'Foo'>;
'Foo' as C2; // OK

// --- Single char ---

type C3 = Capitalize<'a'>;
'A' as C3; // OK

// --- Empty string: no change ---

type C4 = Capitalize<''>;
'' as C4; // OK
'A' as C4; // ERROR

// --- Non-letter first char: no change ---

type C5 = Capitalize<'1abc'>;
'1abc' as C5; // OK

// --- Distribution over union ---

type C6 = Capitalize<'foo' | 'bar'>;
'Foo' as C6; // OK
'Bar' as C6; // OK
'foo' as C6; // ERROR

// --- `Capitalize<string>` is the set of strings whose first char is already
//     uppercase (or non-letter) ---

type C7 = Capitalize<string>;
'Anything' as C7; // OK
'anything' as C7; // ERROR (first char is lowercase)
'1abc' as C7; // OK (non-letter first char is unchanged by capitalize)

// --- Template literal: first quasi has first char capitalized; rest of string is unaffected ---

type C8 = Capitalize<`abc${string}`>;
'Abcxyz' as C8; // OK
'abcxyz' as C8; // ERROR (first char must be capital)

// --- Empty first quasi: leading character is the start of the interpolated
//     type, so the transform is pushed inside it. `Capitalize<\`${string}abc\`>`
//     becomes `\`${Capitalize<string>}abc\``. ---

type C9 = Capitalize<`${string}abc`>;
'Abc' as C9; // OK (capitalize<string> matches when first char is upper)
'abc' as C9; // ERROR (first char is lowercase)
'XYZabc' as C9; // OK (X is uppercase; rest of inner can be anything)
'xYZabc' as C9; // ERROR (first char of inner is lowercase)

// --- Round trip ---

type C10 = Capitalize<Uncapitalize<'Foo'>>;
'Foo' as C10; // OK

// --- Arity ---

type Bad1 = Capitalize; // ERROR
type Bad2 = Capitalize<>; // ERROR
type Bad3 = Capitalize<'a', 'b'>; // ERROR

// --- Constraint ---

type Bad4 = Capitalize<42>; // ERROR
