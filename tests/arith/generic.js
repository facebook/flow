/* @flow */

function f1<A>(a: A): A { return a + a; } // error
function f2<A,B>(a: A, b: B): A {return a + b; } // error
function f3<A,B>(a: A, b: B): A {return b + a; } // error
function f4<A,B>(a: A, b: B): B {return a + b; } // error
function f5<A,B>(a: A, b: B): B {return b + a; } // error
