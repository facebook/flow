var x: Array<string> = ['1', '2'];
var y: Array<string> = ['3', ...x];
var z: Array<string> = [...'123'];

var z2: Array<'1' | '2' | '3'> = [...'123']; //error
var z3: Array<'123'> = [...'123']; //error

var A: Set<number> = new Set();
var B: number[] = [...A];
var C: Map<string, number> = new Map();
var D: Array<[string, number]> = [...C];

function* genE() {
  yield 1;
  return 2;
}
var E: number[] = [...genE()];

function* genF() {
  yield 1;
  yield '';
  return true;
}
var F: Array<number | string | bool> = [...genF()];

function* genG() {
  yield 1;
}
var G: Array<number> = [...genG()]; //Error, genG can return void
