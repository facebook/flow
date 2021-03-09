const hello = require('./test4');
const dummy = require('./test');
module.exports = {
  ...dummy,
  // TODO allow computed prop (T64194787)
  [hello]: 'world',
  ...dummy,
};
