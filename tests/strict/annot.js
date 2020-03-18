const A = require('./unknown_class');

class B extends A {
  foo(x:A):A { return x; }
  bar(x): void { }
  qux<X,Y>(x:X,y:Y):X { return x;}
}

module.exports = B;
