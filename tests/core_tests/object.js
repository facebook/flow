type A = { foo: string, bar: number };
type B = { [k: number]: boolean };

var values = Object.values;
var entries = Object.entries;

declare var a: A;
declare var b: B;

// Sealed, non-dict object types

// OK
(values(a) : Array<string|number>);

// error: array type ~> void
(values(a) : void);

// error: string ~> void, number ~> void
(values(a) : Array<void>);

// OK
(entries(a) : Array<[string|number, string|number]>);

// error: array type ~> void
(entries(a) : void);

// error: tuple type ~> void
(entries(a) : Array<void>);

// error: string ~> void, number ~> void
(entries(a) : Array<[void, string|number]>);

// error: string ~> void, number ~> void
(entries(a) : Array<[string|number, void]>);


// Dictionary types

// OK
(values(b) : Array<boolean>);

// error: array type ~> void
(values(b) : void);

// error: boolean ~> void
(values(b) : Array<void>);

// OK
(entries(b) : Array<[string, boolean]>);

// error: number ~> string
(entries(b): Array<[number, boolean]>);

// error: array type ~> void
(entries(b) : void);

// error: tuple type ~> void
(entries(b) : Array<void>);

// error: string ~> void
(entries(b) : Array<[void, boolean]>);

// error: boolean ~> void
(entries(b) : Array<[string, void]>);
