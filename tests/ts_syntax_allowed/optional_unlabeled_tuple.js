// Basic optional unlabeled tuple element
type T = [string, string?];
declare const t: T;
t as [string, string?]; // OK
[''] as T; // OK - optional element can be omitted
['', ''] as T; // OK
['', 1] as T; // ERROR - number incompatible with string

// Multiple optional elements
type S = [number?, number?];
declare const s: S;
[] as S; // OK
[1] as S; // OK
[1, 2] as S; // OK
[1, ''] as S; // ERROR

// Optional with union type
type U = [string | number?];
declare const u: U;
[''] as U; // OK
[1] as U; // OK
[] as U; // OK
[true] as U; // ERROR

// Optional with nullable type
type N = [?string?];
declare const n: N;
[null] as N; // OK
[undefined] as N; // OK
[''] as N; // OK
[] as N; // OK

// Optional after spread
type Sp = [...[number, number], string?];
declare const sp: Sp;
[1, 2] as Sp; // OK
[1, 2, ''] as Sp; // OK
[1, 2, 3] as Sp; // ERROR

// Conditional type in optional element
type C<T> = [T extends string ? number : boolean?];
declare const c: C<string>;
[1] as C<string>; // OK
[] as C<string>; // OK
[''] as C<string>; // ERROR

// Optional with user-defined type (identifier)
type Alias = string;
type W = [Alias?];
[''] as W; // OK
[] as W; // OK
[1] as W; // ERROR

// Required after optional should error
type Invalid = [string?, number]; // ERROR
