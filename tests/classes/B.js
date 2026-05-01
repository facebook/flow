const A = require('./A');

class B extends A { }

let b = new B();
b.foo as number; // error, number !~> function

module.exports = B;
