class C {
  static m() {};
  m() {};
}
C.m as empty; //err
new C().m as empty; // err
module.exports = C;
