// @flow

function foo1(o: { x: number }) {
  if (o.p1) { // OK, this is an idiomatic way of testing property existence
    o.x;
  }
}

function foo2(o: { x: number }) {
  if (o.p2) { // OK
    o.p2.x; // error, since o.p2's type is unknown (e.g., could be `number`)
  }
}

function foo3(o: { x: number }) {
  o.p3.x; // usual error outside conditional
}

function foo4(o: $Exact<{ x: number }>) {
  if (o.p4) { // OK
    o.p4.x; // currently OK, should be unreachable
  } else {
    o.p4.x; // error
  }
}

function foo5() {
  const o = { };
  _foo5();
  if (o.p) { o.p(); }
  function _foo5() {
    o.p = function() { }
  }
}
