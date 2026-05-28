// Sigexport of a casing utility applied to an indexed-access type. The
// type-sig merge path used to skip concretization, leaving `arg` as an
// opaque EvalT — see cross_file_b.js for the regression case.

export type X = Uppercase<['foo'][0]>;
export type Y = Lowercase<{f: 'FOO'}['f']>;
