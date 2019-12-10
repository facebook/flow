#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nShould detect that rollout groups sum to less than 100%%:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.pct_sum_too_low" . 2>&1

printf "\nShould detect that rollout groups sum to more than 100%%:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.pct_sum_too_high" . 2>&1

printf "\nDuplicate rollout names are banned:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.duplicate_rollout_names" . 2>&1

printf "\nDuplicate group names are banned:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.duplicate_group_names" . 2>&1

printf "\nRollout names may only contain [a-zA-Z0-9._]:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.bad_rollout_name" . 2>&1

printf "\nGroup names may only contain [a-zA-Z0-9._]:\n";
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.bad_group_name" . 2>&1

printf "\n100%% on should always be on:\n";
assert_errors "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.100_pct_well_formed_exports" .

printf "\n0%% on should always be off:\n";
assert_ok "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.0_pct_well_formed_exports" .

printf "\nUnknown rollout:\n"
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.unknown_rollout" . 2>&1

printf "\nUnknown group:\n"
assert_exit 8 "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.unknown_group" . 2>&1

printf "\nDisable a .*.js ignore via rollout:\n"
assert_errors "$FLOW" check \
    --strip-root --no-flowlib --flowconfig-name ".flowconfig.0_pct_ignore" .