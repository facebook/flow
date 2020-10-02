const D = require('./import');
class C extends D {
  constructor(): void { return super(); }
  foo(): number { return super.foo(); }
}
module.exports = C;
