//@flow

const {foo, Bar, baz, qux} = require('./jsdoc-exports');
const {
  DefaultedStringEnum,
  InitializedStringEnum,
  NumberEnum,
  BooleanEnum,
  SymbolEnum,
} = require('./jsdoc-objects');

/** a JSDoc in the same file */
function x() {}

(     );
// ^
