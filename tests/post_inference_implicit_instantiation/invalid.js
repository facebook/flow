//@flow

const Import = require('./invalid-export');

Import.store(); // Should not crash.
