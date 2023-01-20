// @flow
var num = require('./import');
function foo(x: number) { }
foo(0);
var a:string = num;

// test deduping of inferred types
function nullToUndefinedWrapper(f: (?Object | ?Object | ?string | ?string) => mixed) {}
nullToUndefinedWrapper((val) => val === null ? undefined : val);

declare function idx<IdxObject, IdxResult>(object: IdxObject, f: (_: $Facebookism$IdxWrapper<IdxObject>) => IdxResult): ?$Facebookism$IdxUnwrapper<IdxResult>;
declare var obj: {a?: {b: ?{c: null | {d: number}}}};
const idxResult = idx(obj, obj => obj.a.b.c.d);
