// @flow
var num = require('./import');
function foo(x: number) { }
foo(0);
var a:string = num;

// test deduping of inferred types
function nullToUndefinedWrapper(f: (?Object | ?Object | ?string | ?string) => mixed) {}
nullToUndefinedWrapper((val) => val === null ? undefined : val);
