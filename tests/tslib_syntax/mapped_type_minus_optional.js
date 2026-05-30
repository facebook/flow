type O = {foo: number, bar?: string};

// -? combined with -readonly
type ReadOnlyOptional = {+foo?: number, +bar?: string};
type Mutable = {-readonly [K in keyof ReadOnlyOptional]-?: ReadOnlyOptional[K]};
declare const m: Mutable;
m.foo as number; // OK - required, readable
m.foo = 5; // OK - writable

// -? combined with +readonly
type Frozen = {+readonly [K in keyof O]-?: O[K]};
declare const f: Frozen;
f.bar as string; // OK
f.bar = 'x'; // ERROR - readonly
