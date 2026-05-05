// Object type with [Symbol.iterator]
type MyIterable = { [Symbol.iterator](): Iterator<number> };
declare const obj: MyIterable;
obj as Iterable<number>; // OK

// Interface with [Symbol.iterator]
interface IIterable { [Symbol.iterator](): Iterator<number> }
declare const iobj: IIterable;
iobj as Iterable<number>; // OK

// Declare class with [Symbol.iterator]
declare class DIterable { [Symbol.iterator](): Iterator<number> }
new DIterable() as Iterable<number>; // OK

// Error case - wrong type parameter
type Bad = { [Symbol.iterator](): Iterator<string> };
declare const bad: Bad;
bad as Iterable<number>; // ERROR

// Other well-known symbols in type positions
type Disposable = { [Symbol.dispose](): void };
type AsyncDisposable = { [Symbol.asyncDispose](): void };
type AsyncIterable2 = { [Symbol.asyncIterator](): AsyncIterator<number> };

// Field (non-method) with well-known symbol key
type WithField = { [Symbol.iterator]: () => Iterator<number> };

// Static well-known symbol in declare class
declare class DStaticIterable { static [Symbol.iterator](): Iterator<number> }
DStaticIterable as Iterable<number>; // OK
new DStaticIterable() as Iterable<number>; // ERROR: no instance @@iterator

// Variance on well-known symbol fields
type CovariantField = { +[Symbol.iterator]: () => Iterator<number> }; // OK

// Unrecognized Symbol.XXX should error during type checking, not parsing
type BadSymbol1 = { [Symbol.xxx](): string }; // ERROR: unsupported key
type BadSymbol2 = { [Symbol.xxx](x: unknown): boolean }; // ERROR: unsupported key

interface IBadSymbol { [Symbol.xxx](): string } // ERROR: unsupported key
declare class DBadSymbol { [Symbol.xxx](): string } // ERROR: unsupported key
