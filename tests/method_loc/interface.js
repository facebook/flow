interface I {
  m(): void,
}
declare var o: I;
o.m as empty; // err
module.exports = o;
