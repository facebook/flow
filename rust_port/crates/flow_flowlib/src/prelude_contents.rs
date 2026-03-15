/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use flow_common_xx as xx;

pub static CONTENTS: &[(&str, &str)] = &[("prelude.js", include_str!("prelude/prelude.js"))];

pub static HASH: LazyLock<String> = LazyLock::new(|| {
    let mut state = xx::State::new(0);
    for (file, contents) in CONTENTS {
        state.update(file.as_bytes());
        state.update(contents.as_bytes());
    }
    format!("{:016x}", state.digest())
});
