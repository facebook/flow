# Copyright (c) 2013-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all
all: js

clean:
	rm -rf flow_parser.js

js:
	$(MAKE) -C ../../src/parser js
	cp ../../src/parser/flow_parser.js flow_parser.js
