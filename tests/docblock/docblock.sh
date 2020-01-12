#!/bin/bash -e
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "/**\r\n * @providesModule crlf\r\n */\r\n" > crlf.js
printf "/**\r\n * @providesModule foo\r\n */\r\nrequire('crlf');\r\n" > foo.js
"$FLOW" force-recheck crlf.js foo.js
"$FLOW" status
rm crlf.js foo.js
