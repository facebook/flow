var x:string = NaN
var y:string = Number.MAX_VALUE;
var z:number = new TypeError().name;
var w:string = parseInt("...");

var a = new Map();
a.delete('foobar');

var b = undefined;
if (undefined) {
}

function f(s: string) : number {
  return s.codePointAt(0);
}
