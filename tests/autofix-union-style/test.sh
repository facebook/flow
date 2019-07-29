#!/bin/bash

spec_it () {
  assert_ok "$FLOW" autofix insert-type --in-place --strategy=specialize "$@"
}

spec_it a.js 8 9
spec_it a.js 10 9
spec_it a.js 12 9
spec_it a.js 14 9
spec_it a.js 16 16
spec_it a.js 18 16
spec_it a.js 20 17
spec_it a.js 22 17
spec_it a.js 24 15
spec_it a.js 26 9
spec_it a.js 28 9
spec_it a.js 30 19
spec_it a.js 32 9
spec_it a.js 34 9
spec_it a.js 36 19
spec_it a.js 38 14
spec_it a.js 40 31
spec_it a.js 44 9


echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js

echo "> flow status"
assert_ok "$FLOW" status
