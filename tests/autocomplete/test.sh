FLOW=$1

printf "foo_parse_fail.js = "
$FLOW autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js

printf "foo.js = "
$FLOW autocomplete --strip-root --json foo.js 10 5 < foo.js

printf "bar.js = "
$FLOW autocomplete --strip-root --json bar.js 4 5 < bar.js

printf "qux.js = "
$FLOW autocomplete --strip-root --json qux.js 6 3 < qux.js

printf "str.js = "
$FLOW autocomplete --strip-root --json str.js 3 9 < str.js

printf "num.js = "
$FLOW autocomplete --strip-root --json num.js 4 5 < num.js

printf "bool.js = "
$FLOW autocomplete --strip-root --json bool.js 3 6 < bool.js

printf "union.js = "
$FLOW autocomplete --strip-root --json union.js 10 5 < union.js

printf "object_builtins.js = "
$FLOW autocomplete --strip-root --json object_builtins.js 4 5 < object_builtins.js

printf "function_builtins.js = "
$FLOW autocomplete --strip-root --json function_builtins.js 4 5 < function_builtins.js

printf "fun.js = "
$FLOW autocomplete --strip-root --json fun.js 4 5 < fun.js

printf "this.js = "
$FLOW autocomplete --strip-root --json fun.js 8 10 < this.js

printf "typeparams.js = "
$FLOW autocomplete --strip-root --json fun.js 6 16 < typeparams.js
