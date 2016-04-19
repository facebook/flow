// import stringImport, { str } from './stringImport';
//
// var importSpread: Array<string> = [...stringImport];
// var importSpread2: Array<string> = [...str];
//
// var x: Array<string> = ['1', '2'];
// var y: Array<string> = ['3', ...x];
// var z: Array<string> = [...'123'];
//
// var z2: Array<'1' | '2' | '3'> = [...'123']; //error
// var z3: Array<'123'> = [...'123']; //error
//
// var A: Set<number> = new Set();
// var B: number[] = [...A];
// var C: Map<string, number> = new Map();
// var D: Array<[string, number]> = [...C];
//
// function* genE() {
//   yield 1;
//   return 2;
// }
// var E: number[] = [...genE()];
// var F: number[] = genE(); // error

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
var F: number[] = genE(); // error

function* genG() {
  yield 1;
  yield '';
  return true;
}
var G: Array<number | string> = [...genG()];
// var iterator2: $Iterator<number|string, any, any> = genG();
// var G2: Array<number | string> = [...iterator2];

// function* genH() {
//   yield 1;
// }
// var H: Array<number> = [...genH()]; //Error, genG can return void
