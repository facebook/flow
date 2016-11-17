#!/bin/sh

FLOW=$1

# coverage of declare module
$FLOW coverage --color declare_module.js

# should not crash
$FLOW coverage --color crash.js

# should terminate
$FLOW coverage --color non-termination.js

# currently pretends @flow exists
# TODO: change --all to not be default. then should be 0%
$FLOW coverage no_pragma.js

# should pretend @flow exists
$FLOW coverage --all no_pragma.js

# should be 0%
$FLOW coverage --respect-pragma no_pragma.js

# --all wins
$FLOW coverage --respect-pragma --all no_pragma.js 2>&1
