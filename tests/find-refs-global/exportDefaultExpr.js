// @flow

import foo from './exportDefaultAsync';

export default (1, function foo() { foo(); });

// This refers to the imported symbol, since the `export default` statement does not introduce a
// top-level binding.
foo();
