#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dotslash

dotslash.export_fbcode_build(
    target="//flow/src/facebook/server_callable:server_callable_extract_metadata",
    oncall="staticresources",
    generated_dotslash_file="scripts/dotslash/server_callable_extract_metadata",
)
