# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all
all: js

.PHONY: clean
clean:
	rm -rf flowconfig_parser.js

# copies flowconfig_parser.js into place, either using a copy that was put in
# dist/flowconfig_parser.js externally (e.g. during a Circle CI build), or by
# building it.
.PHONY: flowconfig_parser.js
flowconfig_parser.js:
	if [ -e dist/flowconfig_parser.js ]; then \
		[ $@ -nt dist/flowconfig_parser.js ] || cp dist/flowconfig_parser.js $@; \
	else \
		cd ../..; \
		$(MAKE) flowconfig-js; \
		cp src/commands/config/flowconfig_parser.js $@; \
	fi

js: flowconfig_parser.js
