/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use flow_common_xx as xx;

#[cfg(fbcode_build)]
const CORE_JS: &str = include_str!("flowlib/core.js");
#[cfg(not(fbcode_build))]
const CORE_JS: &str = include_str!("../../../../lib/core.js");

#[cfg(fbcode_build)]
const REACT_JS: &str = include_str!("flowlib/react.js");
#[cfg(not(fbcode_build))]
const REACT_JS: &str = include_str!("../../../../lib/react.js");

/// The embedded flowlib file contents as (filename, contents) pairs.
/// Order matches the OCaml PPX behavior (directory listing order).
pub static CONTENTS: &[(&str, &str)] = &[("core.js", CORE_JS), ("react.js", REACT_JS)];

pub static HASH: LazyLock<String> = LazyLock::new(|| {
    let mut state = xx::State::new(0);
    for (file, contents) in CONTENTS {
        state.update(file.as_bytes());
        state.update(contents.as_bytes());
    }
    format!("{:016x}", state.digest())
});
