// @flow
var num = require('./import');
function foo(x) { }
foo(0);
var a:string = num;

function unannotated(x) {
  return x;
}

// test deduping of inferred types
const nullToUndefined = val => val === null ? undefined : val;

function f0(x: ?Object) { return nullToUndefined(x); }
function f1(x: ?Object) { return nullToUndefined(x); }
function f2(x: ?string) { return nullToUndefined(x); }
function f3(x: ?string) { return nullToUndefined(x); }

declare function idx<IdxObject, IdxResult>(object: IdxObject, f: (_: $Facebookism$IdxWrapper<IdxObject>) => IdxResult): ?$Facebookism$IdxUnwrapper<IdxResult>;
declare var obj: {a?: {b: ?{c: null | {d: number}}}};
const idxResult = idx(obj, obj => obj.a.b.c.d);
