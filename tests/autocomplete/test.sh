FLOW=$1

echo -n "foo_parse_fail.js = "
$FLOW autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js

echo -n "foo.js = "
$FLOW autocomplete --strip-root --json foo.js 10 5 < foo.js

echo -n "bar.js = "
$FLOW autocomplete --strip-root --json bar.js 4 5 < bar.js

echo -n "qux.js = "
$FLOW autocomplete --strip-root --json qux.js 6 3 < qux.js

echo -n "str.js = "
$FLOW autocomplete --strip-root --json str.js 3 9 < str.js

echo -n "num.js = "
$FLOW autocomplete --strip-root --json num.js 4 5 < num.js

echo -n "bool.js = "
$FLOW autocomplete --strip-root --json bool.js 3 6 < bool.js

echo -n "union.js = "
$FLOW autocomplete --strip-root --json union.js 10 5 < union.js

echo -n "object_builtins.js = "
$FLOW autocomplete --strip-root --json object_builtins.js 4 5 < object_builtins.js

echo -n "function_builtins.js = "
$FLOW autocomplete --strip-root --json function_builtins.js 4 5 < function_builtins.js

echo -n "fun.js = "
$FLOW autocomplete --strip-root --json fun.js 4 5 < fun.js
