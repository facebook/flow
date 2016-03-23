/* @flow */

function f<A,B>(a:A,b:B):A {return a+b} // error: mixed ~> incompatible instantiation
function f<A,B>(a:A,b:B):A {return b+a} // error: mixed ~> incompatible instantiation
function f<A,B>(a:A,b:B):B {return a+b} // error: mixed ~> incompatible instantiation
function f<A,B>(a:A,b:B):B {return b+a} // error: mixed ~> incompatible instantiation
