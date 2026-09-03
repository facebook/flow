#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set +x # don't print the secrets!

# only run on tags
if [[ "$GITHUB_REF_NAME" = "" ]]; then exit 0; fi

VERSION="${GITHUB_REF_NAME#v}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

pushd "$SCRIPT_DIR/../packages/flow-bin"
make publish VERSION="$VERSION" NPM_TOKEN="$NPM_TOKEN"
popd > /dev/null
