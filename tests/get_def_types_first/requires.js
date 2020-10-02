//@flow

const Foo = require('./module_exports');

  Foo;
// ^

const { Bar, baz, qux } = require('./module_exports2');

  Bar;
// ^
  baz;
// ^
  qux;
// ^
