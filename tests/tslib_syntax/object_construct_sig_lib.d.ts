// The shape `lib.dom.d.ts` uses for every DOM constructor: an instance
// interface plus a `declare var` whose type is an object type carrying the
// construct signature. Lib files are checked through `type_annotation.rs`
// rather than the signature parser, so this covers that path.
interface TsCtorGlobal {
    size: number;
}

declare var TsCtorGlobal: {
    prototype: TsCtorGlobal;
    new (init?: string): TsCtorGlobal;
};

declare var TsCtorGlobalQuoted: {
    "new"(): TsCtorGlobal;
};
