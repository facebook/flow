const f = require('./function_decl_with_statics');
f as empty; // err
f.x as empty; // err
f.missing; // err
