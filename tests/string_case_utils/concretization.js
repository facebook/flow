// Concretization: when the arg to a casing utility is hidden behind an
// indirection (indexed access, property access, ReturnType, etc.), `resolve`
// should concretize it before falling through to the deferred `StringMappingT`
// form. Without concretization, the eager singleton-string / union / template
// branches are missed and the argument is treated as opaque — which is unsound
// when the LHS happens to be in canonical form for the kind but not equal to
// the concretized arg.

// --- Indexed access on a tuple ---

type IA1 = Uppercase<['foo'][0]>;
'FOO' as IA1; // OK
'foo' as IA1; // ERROR
'BAR' as IA1; // ERROR (concretized arg is 'foo', so result is 'FOO')

// --- Property access on an object ---

type PA1 = Uppercase<{f: 'foo'}['f']>;
'FOO' as PA1; // OK
'BAR' as PA1; // ERROR

// --- Indexed access producing a union ---

type IA2 = Uppercase<['foo' | 'bar'][0]>;
'FOO' as IA2; // OK
'BAR' as IA2; // OK
'BAZ' as IA2; // ERROR

// --- Lowercase via property access ---

type PA2 = Lowercase<{f: 'FOO'}['f']>;
'foo' as PA2; // OK
'FOO' as PA2; // ERROR

// --- Capitalize via indexed access ---

type IA3 = Capitalize<['foo'][0]>;
'Foo' as IA3; // OK
'foo' as IA3; // ERROR

// --- Uncapitalize via property access ---

type PA3 = Uncapitalize<{f: 'FOO'}['f']>;
'fOO' as PA3; // OK
'FOO' as PA3; // ERROR

// --- Indexed access on a template literal singleton ---

type IA4 = Uppercase<[`a${'b'}c`][0]>;
'ABC' as IA4; // OK
'abc' as IA4; // ERROR

// --- Nested casing through indexed access ---

type N1 = Uppercase<[Lowercase<'AbC'>][0]>;
'ABC' as N1; // OK

// --- Concretization must apply through generic substitution: without it, the
//     arg stays an opaque EvalT after a generic is instantiated with an
//     indexed-access type. ---

type M<T extends string> = Uppercase<T>;
type AI1 = M<['foo'][0]>;
'FOO' as AI1; // OK
'foo' as AI1; // ERROR
'BAR' as AI1; // ERROR

type Mlow<T extends string> = Lowercase<T>;
type AI2 = Mlow<{f: 'FOO'}['f']>;
'foo' as AI2; // OK
'FOO' as AI2; // ERROR

// --- Cap/uncap with empty leading quasi: the placeholder's leading char comes
//     from the interpolated type. `can_be_empty_string` decides whether to emit
//     a spurious empty-arm union. Without concretization, an opaque interpolant
//     (alias / indexed access / property access) falls through to "can be
//     empty", widening the type and silently admitting strings starting with
//     the literal suffix. ---

type T_alias = 'Hello';
type LE1 = Capitalize<`${T_alias}_world`>;
'Hello_world' as LE1; // OK
'_world' as LE1;       // ERROR

type LE2 = Capitalize<`${['Hello'][0]}_world`>;
'Hello_world' as LE2; // OK
'_world' as LE2;       // ERROR

type LE3 = Capitalize<`${{f: 'Hello'}['f']}_world`>;
'Hello_world' as LE3; // OK
'_world' as LE3;       // ERROR

type T_alias_u = 'HELLO';
type LE4 = Uncapitalize<`${T_alias_u}_WORLD`>;
'hELLO_WORLD' as LE4; // OK
'_WORLD' as LE4;       // ERROR
