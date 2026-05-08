/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use flow_common_xx as xx;

#[cfg(fbcode_build)]
const PRELUDE_JS: &str = include_str!("prelude/prelude.js");
#[cfg(not(fbcode_build))]
const PRELUDE_JS: &str = include_str!("../../../../prelude/prelude.js");

pub static CONTENTS: &[(&str, &str)] = &[("prelude.js", PRELUDE_JS)];

pub static HASH: LazyLock<String> = LazyLock::new(|| {
    let mut state = xx::State::new(0);
    for (file, contents) in CONTENTS {
        state.update(file.as_bytes());
        state.update(contents.as_bytes());
    }
    format!("{:016x}", state.digest())
});
