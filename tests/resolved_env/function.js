//@flow

function g() {
  return f();
}

function f() {
  return 42;
}

var x = function h() {
  h() as empty;
  return 42;
}; // err in old inference, no err in LTI

f() as empty; // err
g() as empty; // err

function h(): number {
  return i();
}

function i(): number {
  return h();
}

var y = function k(): number {
  k() as empty;
  return 42;
}; // err

h() as empty; // err
i() as empty; // err

const foo = (): number => foo(); // ok
const bar = function (): number {
  return bar();
}; // ok
const baz = function _(): number {
  return baz();
}; // ok
