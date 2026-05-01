const f = require('./arrow_with_statics');
f as empty; // err
f.x as empty; // err
f.missing; // err
