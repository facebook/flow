# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all
all: js

.PHONY: clean
clean:
	rm -rf flow_parser.js

# copies flow_parser.js into place, either using a copy that was put in
# dist/flow_parser.js externally (e.g. during a Circle CI build), or by
# building it.
.PHONY: flow_parser.js
flow_parser.js:
	if [ -e dist/flow_parser.js ]; then \
		[ $@ -nt dist/flow_parser.js ] || cp dist/flow_parser.js $@; \
	elif [[ "${INTERNAL_BUILD}" -eq 1 ]]; then \
		buck2 build @//mode/opt //flow/src/parser:flow_parser.js --out $@; \
	else \
		$(MAKE) -C ../../src/parser js; \
		cp ../../src/parser/flow_parser.js $@; \
	fi

js: flow_parser.js
