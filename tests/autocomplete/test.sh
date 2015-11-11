FLOW=$1

$FLOW autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js
$FLOW autocomplete --strip-root --json foo.js 10 5 < foo.js
$FLOW autocomplete --strip-root --json bar.js 4 5 < bar.js
$FLOW autocomplete --strip-root --json qux.js 6 3 < qux.js
$FLOW autocomplete --strip-root --json str.js 3 9 < str.js
$FLOW autocomplete --strip-root --json num.js 4 5 < num.js
$FLOW autocomplete --strip-root --json bool.js 3 6 < bool.js
$FLOW autocomplete --strip-root --json union.js 10 5 < union.js
$FLOW autocomplete --strip-root --json object_builtins.js 4 5 < object_builtins.js
$FLOW autocomplete --strip-root --json function_builtins.js 4 5 < function_builtins.js
$FLOW autocomplete --strip-root --json fun.js 4 5 < fun.js
