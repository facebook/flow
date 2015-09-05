// You should be able to use a function as an object with a call property
var a: { (x: number): string } = function (x: number): string { return "hi"; };

// ...and it should notice when the return type is wrong
var b: { (x: number): number } = function (x: number): string { return "hi"; };

// ...or if the param type is wrong
var c: { (x: string): string } = function (x: number): string { return "hi"; };

// ...or if the arity is wrong
var d: { (): string } = function (x: number): string { return "hi"; };

// A function can be an object
var f : {} = function (x: number): string { return "hi"; };
var g : Object = function (x: number): string { return "hi"; };
