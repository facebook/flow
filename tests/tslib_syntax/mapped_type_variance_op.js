type O = {foo: number, bar: string};

// +readonly makes properties read-only (same as plain readonly)
type PlusReadOnly<T extends {...}> = {+readonly [K in keyof T]: T[K]};
declare const pro: PlusReadOnly<O>;
pro.foo as number; // OK - reading is fine
pro.foo = 3; // ERROR - writing to +readonly property

// -readonly makes properties read-write
type ReadOnlyO = {+foo: number, +bar: string};
type MinusReadOnly<T extends {...}> = {-readonly [K in keyof T]: T[K]};
declare const mro: MinusReadOnly<ReadOnlyO>;
mro.foo as number; // OK - reading is fine
mro.foo = 3; // OK - writing is fine, -readonly removed read-only constraint

// -readonly on already-mutable properties stays mutable
type MutableFromMutable<T extends {...}> = {-readonly [K in keyof T]: T[K]};
declare const mfm: MutableFromMutable<O>;
mfm.foo as number; // OK
mfm.foo = 3; // OK

// plain readonly still works as before
type PlainRO<T extends {...}> = {readonly [K in keyof T]: T[K]};
declare const pror: PlainRO<O>;
pror.foo as number; // OK
pror.foo = 3; // ERROR - writing to readonly property

// -readonly on writeonly properties preserves writeonly (no readonly to remove)
type WriteOnlyO = {-foo: number, -bar: string};
type MutableFromWO<T extends {...}> = {-readonly [K in keyof T]: T[K]};
declare const mfwo: MutableFromWO<WriteOnlyO>; // ERROR - input props are not readable
mfwo.foo as number; // ERROR - still writeonly, can't read
mfwo.foo = 3; // OK - still writable

// -readonly combined with optionality
type ReadOnlyO2 = {+foo: number, +bar: string};
type MutablePartial<T extends {...}> = {-readonly [K in keyof T]?: T[K]};
declare const mp: MutablePartial<ReadOnlyO2>;
mp.foo as number | void; // OK - optional
mp.foo = 3; // OK - writable
