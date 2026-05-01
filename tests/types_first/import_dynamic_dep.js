const x = require('./import_dynamic');
x.then(y => {
  y as empty; // error: number ~> empty
});
