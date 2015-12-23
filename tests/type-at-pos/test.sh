FLOW=$1

$FLOW type-at-pos test.js 5 1 --strip-root --json
$FLOW type-at-pos test.js 5 1 --strip-root --raw
$FLOW type-at-pos test.js 8 7 --strip-root --json
$FLOW type-at-pos generics.js 5 1 --strip-root --json
$FLOW type-at-pos generics.js 10 1 --strip-root --json
$FLOW type-at-pos generics.js 14 1 --strip-root --json
$FLOW type-at-pos generics.js 18 1 --strip-root --json
$FLOW type-at-pos generics.js 22 1 --strip-root --json
$FLOW type-at-pos generics.js 26 1 --strip-root --json
