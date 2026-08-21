#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# A recheck whose file-watcher metadata says the mergebase changed takes a
# different branch of `type_service::recheck` than an ordinary recheck. That
# branch used to publish the heap from inside a `match` arm whose scrutinee still
# owned a `&transaction.handle()` temporary, so `Transaction::commit`'s
# `Arc::try_unwrap` saw two strong references and killed the server with
# "all transaction handles must be dropped before commit".

mkdir tmp
cp a.js tmp/

start_flow .
assert_errors "$FLOW" status --strip-root

printf "\nRecheck across a changed mergebase\n"
cp tmp1/a.js a.js
assert_ok "$FLOW" force-recheck --changed-mergebase a.js
# `--no-auto-start` so that a server killed by the commit is a failure rather
# than something `status` quietly replaces. It also blocks until the recheck
# finishes, so the log is complete below.
assert_errors "$FLOW" status --strip-root --no-auto-start

printf "\nServer log\n"
if grep -q "all transaction handles must be dropped before commit" "$FLOW_LOG_FILE"; then
  echo "committed a recheck transaction while a handle was still alive"
else
  echo "no transaction-handle panic"
fi

assert_ok "$FLOW" stop

cp tmp/a.js a.js
rm -rf tmp
