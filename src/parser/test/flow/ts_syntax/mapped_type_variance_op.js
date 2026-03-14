type T1 = { -readonly [P in keyof T]: T[P] };
type T2 = { +readonly [P in keyof T]: T[P] };
type T3 = { -readonly [K in keyof S]-?: S[K] };
type T4 = { readonly [P in keyof T]: T[P] };
