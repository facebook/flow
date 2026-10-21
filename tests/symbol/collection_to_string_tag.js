declare const map: Map<string, number>;
declare const roMap: ReadonlyMap<string, number>;
declare const set: Set<string>;
declare const roSet: ReadonlySet<string>;

map[Symbol.toStringTag] as string; // OK
roMap[Symbol.toStringTag] as string; // OK
set[Symbol.toStringTag] as string; // OK
roSet[Symbol.toStringTag] as string; // OK

map[Symbol.toStringTag] as 'Set'; // ERROR
roSet[Symbol.toStringTag] as 'Map'; // ERROR

// The read-only bases declare `[Symbol.toStringTag]`, so a read-only view still
// satisfies `Readonly` of the mutable class.
roMap as Readonly<Map<string, number>>; // OK
roSet as Readonly<Set<string>>; // OK

interface TaggedMap { readonly [Symbol.toStringTag]: 'Map' }
map as TaggedMap; // ERROR: the tag is `string`
roMap as TaggedMap; // ERROR: the tag is `string`
set as TaggedMap; // ERROR
