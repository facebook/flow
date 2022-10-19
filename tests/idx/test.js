// @flow

declare function idx<IdxObject, IdxResult>(object: IdxObject, f: (_: $Facebookism$IdxWrapper<IdxObject>) => IdxResult): ?$Facebookism$IdxUnwrapper<IdxResult>;

// Objects
declare var obj1: {a: ?{b: {c: number}}};
obj1.a.b.c; // error
(idx(obj1, obj => obj.a.b.c): ?number); // ok
(idx(obj1, obj => obj["a"].b.c): ?number); // ok
(idx(obj1, obj => obj.a.b.c): number); // error: result must be MaybeT
(idx(obj1, obj => obj.a.b.c): ?string); // error: number ~> string
(idx(obj1, obj => obj["a"].b.c): number); // error: result must be MaybeT
idx(obj1, obj => obj.notAProp); // error: prop-missing
idx(obj1, obj => obj.a = null); // error: invalid-idx
declare var obj2: {a?: {b: {c: number}}};
(idx(obj2, obj => obj.a.b.c): ?number); // ok
(idx(obj2, obj => obj.a.b.c): number); // error: result must be MaybeT
declare var obj3: {a: null | {b: {c: number}}};
(idx(obj3, obj => obj.a.b.c): ?number); // ok
(idx(obj3, obj => obj.a.b.c): number); // error: result must be MaybeT
// Nested maybes/optionals should get unwrapped
declare var obj4: {a?: ?(?{b: number})};
(idx(obj4, obj => obj.a.b): ?number); // ok

// Unions
declare var ab: {a:string}|{b:number};
(idx(ab, _ => _.a): empty); // error
(idx(ab, _ => _.b): empty); // error
(idx(ab, _ => _.c): empty); // error

// Classes
class Foo1 { a: ?Foo1; b: ?number; }
class Foo2 { a: Foo2 | void; b: ?number; }
class Foo3 { a: Foo3 | null; b: ?number; }
(idx(new Foo1(), o => o.a.b): ?number); // ok
(idx(new Foo1(), o => o.a.b): number); // error: result must be MaybeT
idx(new Foo1(), o => o.a = null); // error: invalid-idx

// Arrays
declare var arr1: Array<?Array<number>>;
declare var arr2: Array<Array<number> | void>;
declare var arr3: Array<Array<number> | null>;
(idx(arr1, arr => arr[0][0]): ?number); // ok
(idx(arr2, arr => arr[0][0]): ?number); // ok
(idx(arr3, arr => arr[0][0]): ?number); // ok

// Non-objects
(idx(42, n => n): ?number); // ok
(idx(42, n => n): number); // error: result must be MaybeT
idx(42, n => n.nope); // error: prop-missing

// Weird edge cases
// Using an annotation obscures the type wrapper mechanism that idx() uses
// around the parameter it passes to the callback
(idx({}, (obj: Object) => obj.a.b.c): ?number); // ok
// Can't do anything with the callback parameter other than get elements and properties off of it
idx({}, obj => obj()); // error: invalid-idx
