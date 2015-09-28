FLOW=$1

$FLOW autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js
$FLOW autocomplete --strip-root --json foo.js 10 5 < foo.js
