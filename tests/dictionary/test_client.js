var o = require('./test');

o.foo = function (params) {
  params.count = params.oops;
  return params.count;
}
