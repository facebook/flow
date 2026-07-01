#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# The server is started for us. `clock` carries a random per-process instance id,
# so redact it to keep the snapshot stable.
redact_clock() {
  sed 's/"clock":"[^"]*"/"clock":"<redacted>"/'
}

echo "=== single field -> flat array of values ==="
assert_ok "$FLOW" query '{"fields":["name"]}' | redact_clock

echo "=== multiple fields -> object per file ==="
assert_ok "$FLOW" query '{"fields":["name","flow.content_hash"]}' | redact_clock

echo "=== exists + new ==="
assert_ok "$FLOW" query '{"fields":["name","exists","new"]}' | redact_clock

echo "=== empty_on_fresh_instance -> no files on a fresh instance ==="
assert_ok "$FLOW" query '{"fields":["name"],"empty_on_fresh_instance":true}' | redact_clock
