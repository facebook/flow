// @flow
interface I {
  m(): void,
}
declare var o: I;
(o.m: empty); // err
module.exports = o;
