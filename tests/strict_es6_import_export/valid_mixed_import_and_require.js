// @flow

import {named1} from './foo';

// Side effectful top level require allowed
require('./foo');

function foo() {
  // Non toplevel require allowed
  require('./foo')
}
