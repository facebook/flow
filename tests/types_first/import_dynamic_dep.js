// @flow
const x = require('./import_dynamic');
x.then(y => {
  (y: empty); // error: number ~> empty
});
