// @flow
class C {
  static m() {};
  m() {};
}
(C.m: empty); //err
(new C().m: empty); // err
module.exports = C;
