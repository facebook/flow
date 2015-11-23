FLOW=$1

$FLOW get-def --strip-root --json ignore/test.js 3 2
$FLOW type-at-pos --strip-root --json ignore/test.js 3 2
$FLOW get-def --strip-root --json no_flow/test.js 3 2
$FLOW type-at-pos --strip-root --json no_flow/test.js 3 2
