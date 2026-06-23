/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(fbcode_build)]
fn get_build_revision() -> &'static str {
    build_info::BuildInfo::get_revision()
}

#[cfg(not(fbcode_build))]
fn get_build_revision() -> &'static str {
    ""
}

pub fn build_revision() -> &'static str {
    get_build_revision()
}
