// @flow

function f() {
  return this.x;
}
f.call({x: 1});

function withArg(y: number) {
  return this.x + y;
}
withArg.call({x: 1}, 1);

function withUnannotatedArg(y) {
  return this.x + y;
}
withUnannotatedArg.call({x: 1}, 1);

function unused() {
  return this.x;
}

function unusedTwoProps() {
  return this.x1 + this.z1;
}

function unusedMethod() {
  return this.x1(1);
}

let object = {
  fn: function (y: number) {
    return this.x + y;
  },
  x: 1,
}
object.fn(1)

class HasThis {
  x: number;
  methodA() {
    return this.x;
  }
  methodB() {
    (function () { // old style bind, expect any/$FlowFixMe
      return this.x;
    }).bind(this);
  }

  arrow = y => { // should annotate y but not `this`
    return this.x + (y: number);
  }
}

let arrow = () => { return this; }


