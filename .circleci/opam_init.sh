#!/bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

if [ ! -f ~/.opam/from_cache ]; then
  rm -rf ~/.opam
  opam init --bare --disable-sandboxing --no-setup --yes
fi
