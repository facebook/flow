a as T;
a || b as T; // `a || (b as T)`
a && b as T; // `a && (b as T)`
a === b as T; // `a === (b as T)`
a < b as T; // `(a < b) as T`
a in b as T; // `(a in b) as T`
a + b as T; // `(a + b) as T`
a = b as T; // `a = (b as T)`
new a as T; // `(new as) as T)`
a as T && b; // `(a as T) && b`
a as T || b; // `(a as T) || b`
a as T === b; // `(a as T) === b`
a as const;
