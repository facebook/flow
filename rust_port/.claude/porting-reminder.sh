#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

REMINDER='<system-reminder>
You will receive this reminder from time to time.
DO NOT take shortcuts. Tackle root cause rather than trying to hide the problem.
⚠️ Remember to follow the porting guidelines in CLAUDE.md.
When you received it, you MUST respond whether what you did recently conform to the guidelines,
and whether you wrongfully took any shortcuts.
You must also respond whether you faithfully followed all testing instructions.
If what you did do not conform to the guidelines or sloppy, you must fix them NOW.
</system-reminder>'

PROMISE_FILE="/tmp/i-promise"

emit_reminder() {
    jq -n --arg reminder "$REMINDER" \
        '{"hookSpecificOutput": {"hookEventName": "PostToolUse", "additionalContext": $reminder}}'
}

# When HOOK_ON_COMPLETION is set, block unless /tmp/i-promise exists.
# The AI must write /tmp/i-promise after self-reviewing its work against the guidelines.
if [ -n "$HOOK_ON_COMPLETION" ]; then
    if [ -f "$PROMISE_FILE" ]; then
        rm -f "$PROMISE_FILE"
        exit 0
    else
        BLOCK_MSG="$REMINDER"'
You MUST self-review your work now. Write /tmp/i-promise after confirming compliance.
You cannot complete until /tmp/i-promise exists.'
        jq -n --arg msg "$BLOCK_MSG" \
            '{"decision": "block", "reason": $msg}'
        exit 0
    fi
fi

# Counter file to track operations
COUNTER_FILE="/tmp/flow_rust_port_edit_counter"

# Initialize counter if file doesn't exist
if [ ! -f "$COUNTER_FILE" ]; then
    echo "0" > "$COUNTER_FILE"
fi

# Read current count
count=$(cat "$COUNTER_FILE")

# Increment counter
count=$((count + 1))
echo "$count" > "$COUNTER_FILE"

if [ $((count % 10)) -eq 0 ]; then
    emit_reminder
fi
