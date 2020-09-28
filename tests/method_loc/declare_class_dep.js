// @flow
const C = require('./declare_class');
(C.m: empty); //err
(new C().m: empty); // err
