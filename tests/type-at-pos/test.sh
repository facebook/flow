FLOW=$1

$FLOW type-at-pos test.js 5 1 --strip-root --pretty
$FLOW type-at-pos test.js 5 1 --strip-root --raw --pretty
$FLOW type-at-pos test.js 8 7 --strip-root --pretty
$FLOW type-at-pos test.js 10 7 --strip-root --pretty
$FLOW type-at-pos test.js 12 7 --strip-root --pretty
$FLOW type-at-pos test.js 14 7 --strip-root --pretty
$FLOW type-at-pos generics.js 5 1 --strip-root --pretty
$FLOW type-at-pos generics.js 10 1 --strip-root --pretty
$FLOW type-at-pos generics.js 14 1 --strip-root --pretty
$FLOW type-at-pos generics.js 18 1 --strip-root --pretty
$FLOW type-at-pos generics.js 22 1 --strip-root --pretty
$FLOW type-at-pos generics.js 26 1 --strip-root --pretty
$FLOW type-at-pos optional.js 4 10 --strip-root --pretty
$FLOW type-at-pos optional.js 7 2 --strip-root --pretty
$FLOW type-at-pos optional.js 7 4 --strip-root --pretty
$FLOW type-at-pos optional.js 10 11 --strip-root --pretty
$FLOW type-at-pos optional.js 10 14 --strip-root --pretty
$FLOW type-at-pos optional.js 14 10 --strip-root --pretty
"$FLOW" type-at-pos destructuring.js 3 6 --strip-root --pretty
"$FLOW" type-at-pos destructuring.js 17 13 --strip-root --pretty
"$FLOW" type-at-pos react.js 2 7 --strip-root --pretty
"$FLOW" type-at-pos trycatch.js 5 10 --strip-root --pretty

# printf "function_expression.js:4:8 = "
# "$FLOW" type-at-pos function_expressions.js 4 12 --strip-root --pretty
printf "function_expression.js:9:5 = "
"$FLOW" type-at-pos function_expressions.js 9 5 --strip-root --pretty
printf "function_expression.js:9:7 = "
"$FLOW" type-at-pos function_expressions.js 9 7 --strip-root --pretty
printf "predicates.js - null: "
"$FLOW" type-at-pos predicates.js 4 12 --strip-root --pretty
printf "predicates.js - undefined: "
"$FLOW" type-at-pos predicates.js 5 12 --strip-root --pretty
printf "predicates.js - Array: "
"$FLOW" type-at-pos predicates.js 6 6 --strip-root --pretty
printf "predicates.js - isArray: "
"$FLOW" type-at-pos predicates.js 6 15 --strip-root --pretty
printf "templates.js:2:7 = "
"$FLOW" type-at-pos templates.js 2 7 --strip-root --pretty
printf "object_special_cases.js:6:32 = "
"$FLOW" type-at-pos object_special_cases.js 6 32 --strip-root --pretty
