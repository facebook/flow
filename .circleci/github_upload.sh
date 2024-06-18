#!/bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SRC="$1"
DST="$2"
REPO="facebook/flow"

auth="Authorization: token $FLOW_BOT_TOKEN"
response=$(curl -sH "$auth" "https://api.github.com/repos/$REPO/releases/tags/$GITHUB_REF_NAME")
id=$(echo "$response" | grep '^  "id": ' | sed 's/\s*"id":\s*\(\d*\),.*/\1/')

curl -H "$auth" \
  -H "Accept: application/vnd.github.manifold-preview" \
  -H "Content-Type: application/zip" \
  --data-binary @"$SRC" \
  "https://uploads.github.com/repos/$REPO/releases/$id/assets?name=$DST"
