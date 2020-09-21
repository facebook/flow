// @flow
const f = require('./arrow_with_statics');
(f: empty); // err
(f.x: empty); // err
f.missing; // err
