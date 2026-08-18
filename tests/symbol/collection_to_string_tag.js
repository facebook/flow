declare const map: Map<string, number>;
declare const roMap: ReadonlyMap<string, number>;
declare const set: Set<string>;
declare const roSet: ReadonlySet<string>;

map[Symbol.toStringTag] as 'Map'; // OK
roMap[Symbol.toStringTag] as 'Map'; // OK
set[Symbol.toStringTag] as 'Set'; // OK
roSet[Symbol.toStringTag] as 'Set'; // OK

map[Symbol.toStringTag] as 'Set'; // ERROR
roSet[Symbol.toStringTag] as 'Map'; // ERROR

// The read-only bases declare `[Symbol.toStringTag]`, so a read-only view still
// satisfies `Readonly` of the mutable class.
roMap as Readonly<Map<string, number>>; // OK
roSet as Readonly<Set<string>>; // OK

interface TaggedMap { readonly [Symbol.toStringTag]: 'Map' }
map as TaggedMap; // OK
roMap as TaggedMap; // OK
set as TaggedMap; // ERROR
