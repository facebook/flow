#!/bin/sh -e

FLOW="$1"

printf "/**\r\n * @providesModule crlf\r\n */\r\n" > crlf.js
printf "/**\r\n * @providesModule foo\r\n */\r\nrequire('crlf');\r\n" > foo.js
"$FLOW" force-recheck crlf.js foo.js
"$FLOW" status
rm crlf.js foo.js
