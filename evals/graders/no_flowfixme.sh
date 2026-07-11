#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: no_flowfixme
# Checks that the output does not contain $FlowFixMe, $FlowIgnore, or $FlowExpectedError.
#
# Usage: no_flowfixme.sh <file>
# Exit code: 0 = pass (no suppressions), 1 = fail (suppressions found)

set -euo pipefail

FILE="$1"

if ! [[ -f "$FILE" ]]; then
  echo '{"pass": false, "reason": "file not found"}'
  exit 1
fi

MATCHES=$(grep -nE '\$Flow(FixMe|Ignore|ExpectedError)' "$FILE" 2>/dev/null || true)

if [[ -z "$MATCHES" ]]; then
  echo '{"pass": true}'
  exit 0
else
  COUNT=$(echo "$MATCHES" | wc -l | tr -d ' ')
  echo "{\"pass\": false, \"reason\": \"found $COUNT suppression(s)\", \"matches\": $(echo "$MATCHES" | jq -R -s 'split("\n") | map(select(. != ""))')}"
  exit 1
fi
