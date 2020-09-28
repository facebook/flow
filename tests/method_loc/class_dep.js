// @flow
const C = require('./class');
(C.m: empty); //err
(new C().m: empty); // err
