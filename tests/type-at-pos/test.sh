FLOW=$1

$FLOW type-at-pos test.js 5 1 --strip-root --json
$FLOW type-at-pos test.js 5 1 --strip-root --raw
