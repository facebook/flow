class C { }
var M = { C: C };

var x:M.C = 0;

type foo = {bar: number};
type bar = foo.bar; // TODO: disallow this! (And delete the following test.)

var a: bar = 42;
var b: bar = 'asdf'; // Error: string ~> number
