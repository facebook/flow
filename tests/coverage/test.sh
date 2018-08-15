#!/bin/bash
# coverage of declare module
assert_ok "$FLOW" coverage --color declare_module.js

# should not crash
assert_ok "$FLOW" coverage --color crash.js

# should terminate
assert_ok "$FLOW" coverage --color non-termination.js

# assumes @flow weak
assert_ok "$FLOW" coverage no_pragma.js

# assumes @flow weak
assert_ok "$FLOW" coverage --all no_pragma.js

# should be 0%
assert_ok "$FLOW" coverage --respect-pragma no_pragma.js

# --all wins (and assumes @flow weak)
assert_ok "$FLOW" coverage --respect-pragma --all no_pragma.js
