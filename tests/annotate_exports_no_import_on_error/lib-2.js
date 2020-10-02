// @flow

class C {}
class D {}
declare function foo(): C;
declare function bar(): D;

module.exports = {
  D,
  foo,
  bar,
}
