#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

[ -d ~/.opam ] || opam init default 'https://github.com/fdopen/opam-repository-mingw.git#opam2' --bare --disable-sandboxing --no-setup
