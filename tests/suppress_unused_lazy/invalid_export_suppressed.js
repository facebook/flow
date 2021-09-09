// @flow

// dependent of b with a suppressed signature verification error

import {b} from './b';

function bar() {
  // $FlowFixMe[invalid-export]
  const x = module.exports;
}
