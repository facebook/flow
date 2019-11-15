#!/bin/bash

printf "Reading JS from stdin:\n"
assert_ok "$FLOW" ast --pretty < foo.js

printf "\nReading JSON from stdin:\n"
assert_ok "$FLOW" ast --pretty --type json < bar.json

printf "\nReading JS from a file:\n"
assert_ok "$FLOW" ast --pretty foo.js

printf "\nReading JSON from a file:\n"
assert_ok "$FLOW" ast --pretty bar.json

printf "\nUsing absolute --path with stdin:\n"
assert_ok "$FLOW" ast --pretty --path /some/path.js < foo.js

printf "\nUsing relative --path with stdin:\n"
assert_ok "$FLOW" ast --pretty --path my/relative/path.js < foo.js

printf "\nUsing --path is ignore without stdin:\n"
assert_ok "$FLOW" ast --pretty --path my/relative/path.js foo.js

printf "\nNon-ASCII characters without specifying offset style:\n"
assert_ok "$FLOW" ast --pretty nonascii.js

printf "\nNon-ASCII characters with utf8-bytes offset style:\n"
assert_ok "$FLOW" ast --pretty --offset-style utf8-bytes nonascii.js

printf "\nNon-ASCII characters with js-indices offset style:\n"
assert_ok "$FLOW" ast --pretty --offset-style js-indices nonascii.js
