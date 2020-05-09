//@flow

function foo() {
  this.bar = 'bar';
}

foo.prototype = { baz : 'baz' };

(new foo).
//        ^
