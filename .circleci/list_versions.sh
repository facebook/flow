#!/bin/bash -x
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "version" > "website/_data/flow_dot_js_versions.csv"
git ls-remote --tags 2>/dev/null | awk '{print $2}' | cut -d/ -f3 | \
  grep -e '^v[0-9]\{1,\}\.[0-9]\{1,\}\.[0-9]\{1,\}$' | \
  sort -s -t. -k 1,1nr -k 2,2nr -k 3,3nr | \
  grep -B1000000 '^v0\.37\.0$' >> "website/_data/flow_dot_js_versions.csv"
