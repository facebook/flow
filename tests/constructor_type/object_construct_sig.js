// `{new(): T}` in a *Flow* file keeps its long-standing meaning: an object with
// a method named `new`. Only TypeScript spells a construct signature this way,
// so the desugar to an inline interface is gated on the file extension. See
// `tests/tslib_syntax/object_construct_sig*` for the TS side and for a Flow file
// consuming a TS-authored construct signature.

declare const c: {new (): {x: number}};
c.new().x as number; // OK — ordinary method
new c();             // ERROR: invalid-constructor

// Same for the variants that are not construct signatures in TS either
declare const quoted: {"new"(): {x: number}};
new quoted(); // ERROR: invalid-constructor

declare const optionalMethod: {new?(): {x: number}};
new optionalMethod(); // ERROR: invalid-constructor

declare const field: {new: () => {x: number}};
new field(); // ERROR: invalid-constructor

// The interface and arrow spellings do construct, in Flow files too — those are
// unambiguous syntax and stay ungated.
interface ICtor { new (): {x: number} }
declare const ic: ICtor;
new ic().x as number; // OK

type ArrowCtor = new () => {x: number};
declare const ac: ArrowCtor;
new ac().x as number; // OK

// An object type with a `new` method is not one of those
c as ICtor;     // ERROR — object type is not a construct signature here
c as ArrowCtor; // ERROR — likewise
