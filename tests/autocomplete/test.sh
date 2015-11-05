FLOW=$1

$FLOW autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js
$FLOW autocomplete --strip-root --json foo.js 10 5 < foo.js
$FLOW autocomplete --strip-root --json bar.js 4 5 < bar.js
$FLOW autocomplete --strip-root --json qux.js 6 3 < qux.js
