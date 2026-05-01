// @flow

const b = require('./b');
const c = require('./c');
b.x as typeof c.x;
