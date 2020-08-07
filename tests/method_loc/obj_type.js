// @flow
declare var o: { m(): void };
(o.m: empty); // err
module.exports = o;
