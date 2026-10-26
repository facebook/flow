/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The `website/docs` tree, embedded at compile time.

use include_dir::Dir;
use include_dir::include_dir;

#[cfg(fbcode_build)]
pub static DOCS: Dir<'_> = include_dir!("$FLOW_DOCS_DIR");
#[cfg(not(fbcode_build))]
pub static DOCS: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../../website/docs");
