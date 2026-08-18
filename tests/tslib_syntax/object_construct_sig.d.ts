// TypeScript splits a class into an instance interface plus a constructor
// value, where the constructor is an inline object type carrying `new(...)`.
// This is how `lib.dom.d.ts` declares `URLSearchParams`, `URL`, `Blob`, etc.
export declare class Bar {
    b: string;
}

export declare const BarCtor: {
    prototype: Bar;
    new (x: string): Bar;
    of(): Bar;
};

// Construct signature with no other members
export declare const Plain: {new (): Bar};

export declare class Wrong {
    wrong: number;
}
export declare const WrongCtor: {
    prototype: Bar;
    new (): Wrong;
};
export declare const MismatchedPrototypeCtor: {
    prototype: Wrong;
    new (): Bar;
};

export interface InheritedCtorBase {
    new (): Bar;
}
export interface InheritedCtor extends InheritedCtorBase {}
export declare const Inherited: InheritedCtor;

// Repeated `new` members intersect, the way interface overloads do
export declare class Str {}
export declare class Num {}
export declare const Multi: {
    new (x: string): Str;
    new (x: number): Num;
};

// A call property and a construct signature can coexist
export declare const Dual: {
    (): string;
    new (): Bar;
};

// Interchangeable with the interface and arrow spellings
export interface ICtor {
    new (): Bar;
}
export type ArrowCtor = new () => Bar;
export declare const Obj: {new (): Bar};

// Not construct signatures, in TypeScript either
export declare const Quoted: {"new"(): Bar};
export declare const OptionalMethod: {new?(): Bar};
export declare const Field: {new: () => Bar};
