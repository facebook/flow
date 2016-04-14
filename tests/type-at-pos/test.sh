FLOW=$1

$FLOW type-at-pos test.js 5 1 --strip-root --json
$FLOW type-at-pos test.js 5 1 --strip-root --raw
$FLOW type-at-pos test.js 8 7 --strip-root --json
$FLOW type-at-pos test.js 10 7 --strip-root --json
$FLOW type-at-pos test.js 12 7 --strip-root --json
$FLOW type-at-pos test.js 14 7 --strip-root --json
$FLOW type-at-pos generics.js 5 1 --strip-root --json
$FLOW type-at-pos generics.js 10 1 --strip-root --json
$FLOW type-at-pos generics.js 14 1 --strip-root --json
$FLOW type-at-pos generics.js 18 1 --strip-root --json
$FLOW type-at-pos generics.js 22 1 --strip-root --json
$FLOW type-at-pos generics.js 26 1 --strip-root --json
$FLOW type-at-pos optional.js 4 10 --strip-root --json
$FLOW type-at-pos optional.js 7 2 --strip-root --json
$FLOW type-at-pos optional.js 7 4 --strip-root --json
$FLOW type-at-pos optional.js 10 11 --strip-root --json
$FLOW type-at-pos optional.js 10 14 --strip-root --json
$FLOW type-at-pos optional.js 14 10 --strip-root --json
"$FLOW" type-at-pos destructuring.js 3 6 --strip-root --json
"$FLOW" type-at-pos destructuring.js 17 13 --strip-root --json
