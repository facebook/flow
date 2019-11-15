#!/usr/bin/env python3
# Copyright 2019-present Facebook. All Rights Reserved.

import dotslash

dotslash.export_fbcode_build(
    target="//flow/src/facebook/server_callable:server_callable_extract_metadata",
    oncall="staticresources",
    generated_dotslash_file="scripts/dotslash/server_callable_extract_metadata",
)
