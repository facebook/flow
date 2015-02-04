/* @flow */
var A = require('./A');
var B = require('./B.js');
require('./C.js');
require('./D');

var Ta = requireType('./A');
var Tb = requireType('./B.js').B;
requireType('./E.js');
requireType('./F');

function testA(x: Ta): boolean {
  return x.get();
}

function testB(x: Tb): boolean {
  return x.get();
}

var a = new A();
var b = new B.B();
testA(a);
testB(b);
