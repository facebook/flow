declare class C {
  static m(): void;
  m(): void;
}
C.m as empty; //err
new C().m as empty; // err
module.exports = C;
