// @flow
declare class C {
  static m(): void;
  m(): void;
}
(C.m: empty); //err
(new C().m: empty); // err
module.exports = C;
