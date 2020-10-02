#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

if [ $# -ne 1 ]; then
  echo "version number is required"
  exit 1
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT="$DIR/.."

VERSION="${1#v}" # strip leading v if present
OPAM_VERSION="${VERSION//-/'~'}"
REMOVE_TYPES_VERSION="2${VERSION#0}" # replace leading 0 with 2: 0.123.0 -> 2.123.0

pushd "$ROOT" || exit > /dev/null

set -x
sed -i 's/^\(let version = "\)[^"]*\("\)/\1'"$VERSION"'\2/' src/common/flow_version.ml
sed -i 's/^\(version: "\)[^"]*\("\)/\1'"$OPAM_VERSION"'\2/' flowtype.opam
sed -i 's/^\(version: "\)[^"]*\("\)/\1'"$OPAM_VERSION"'\2/' flow_parser.opam
sed -i 's/^\(version="\)[^"]*\("\)/\1'"$OPAM_VERSION"'\2/' src/parser/META
sed -i 's/\("version": "\)[^"]*\("\)/\1'"$VERSION"'\2/' packages/flow-parser-bin/package.json
sed -i 's/\("version": "\)[^"]*\("\)/\1'"$VERSION"'\2/' packages/flow-parser/package.json
sed -i 's/\("version": "\)[^"]*\("\)/\1'"$REMOVE_TYPES_VERSION"'\2/' packages/flow-remove-types/package.json
sed -i 's/\("flow-parser": "\)[^"]*\("\)/\1^'"$VERSION"'\2/' packages/flow-remove-types/package.json

popd || exit > /dev/null
