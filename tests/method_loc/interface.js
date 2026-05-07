interface I {
  m(): void,
}
declare const o: I;
o.m as empty; // err
module.exports = o;
