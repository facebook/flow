// @flow
const o = { m() {} };
(o.m: empty); // err
module.exports = o;
