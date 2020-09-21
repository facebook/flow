// @flow
const f = require('./function_decl_with_statics');
(f: empty); // err
(f.x: empty); // err
f.missing; // err
