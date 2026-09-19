/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use flow_common_xx as xx;

macro_rules! flowlib_file {
    ($name:ident, $filename:literal) => {
        #[cfg(fbcode_build)]
        const $name: &str = include_str!(concat!("flowlib/", $filename));
        #[cfg(not(fbcode_build))]
        const $name: &str = include_str!(concat!("../../../../lib/", $filename));
    };
}

flowlib_file!(LIB_DECORATORS_JS, "lib.decorators.js");
flowlib_file!(LIB_DECORATORS_LEGACY_JS, "lib.decorators.legacy.js");
flowlib_file!(LIB_ES5_JS, "lib.es5.js");
flowlib_file!(LIB_ES2015_COLLECTION_JS, "lib.es2015.collection.js");
flowlib_file!(LIB_ES2015_GENERATOR_JS, "lib.es2015.generator.js");
flowlib_file!(LIB_ES2015_ITERABLE_JS, "lib.es2015.iterable.js");
flowlib_file!(LIB_ES2015_PROMISE_JS, "lib.es2015.promise.js");
flowlib_file!(LIB_ES2015_PROXY_JS, "lib.es2015.proxy.js");
flowlib_file!(LIB_ES2015_REFLECT_JS, "lib.es2015.reflect.js");
flowlib_file!(LIB_ES2015_SYMBOL_JS, "lib.es2015.symbol.js");
flowlib_file!(LIB_ES2017_SHAREDMEMORY_JS, "lib.es2017.sharedmemory.js");
flowlib_file!(LIB_ES2018_ASYNCGENERATOR_JS, "lib.es2018.asyncgenerator.js");
flowlib_file!(LIB_ES2018_ASYNCITERABLE_JS, "lib.es2018.asynciterable.js");
flowlib_file!(LIB_ES2019_ARRAY_JS, "lib.es2019.array.js");
flowlib_file!(LIB_ES2020_BIGINT_JS, "lib.es2020.bigint.js");
flowlib_file!(LIB_ES2020_INTL_JS, "lib.es2020.intl.js");
flowlib_file!(LIB_ES2020_PROMISE_JS, "lib.es2020.promise.js");
flowlib_file!(
    LIB_ES2020_SYMBOL_WELLKNOWN_JS,
    "lib.es2020.symbol.wellknown.js"
);
flowlib_file!(LIB_ES2021_PROMISE_JS, "lib.es2021.promise.js");
flowlib_file!(LIB_ES2021_WEAKREF_JS, "lib.es2021.weakref.js");
flowlib_file!(LIB_ES2022_ERROR_JS, "lib.es2022.error.js");
flowlib_file!(LIB_ES2024_PROMISE_JS, "lib.es2024.promise.js");
flowlib_file!(LIB_ESNEXT_COLLECTION_JS, "lib.esnext.collection.js");
flowlib_file!(LIB_ESNEXT_FLOAT16_JS, "lib.esnext.float16.js");
flowlib_file!(MISC_JS, "misc.js");
flowlib_file!(REACT_JS, "react.js");
flowlib_file!(DOM_EXTRA_TO_BE_REMOVED_JS, "dom_extra_to_be_removed.js");

/// The embedded flowlib file contents as (filename, contents) pairs.
pub(super) static COMMON_CONTENTS: &[(&str, &str)] = &[
    ("lib.decorators.js", LIB_DECORATORS_JS),
    ("lib.decorators.legacy.js", LIB_DECORATORS_LEGACY_JS),
    ("lib.es2015.collection.js", LIB_ES2015_COLLECTION_JS),
    ("lib.es2015.generator.js", LIB_ES2015_GENERATOR_JS),
    ("lib.es2015.iterable.js", LIB_ES2015_ITERABLE_JS),
    ("lib.es2015.promise.js", LIB_ES2015_PROMISE_JS),
    ("lib.es2015.proxy.js", LIB_ES2015_PROXY_JS),
    ("lib.es2015.reflect.js", LIB_ES2015_REFLECT_JS),
    ("lib.es2015.symbol.js", LIB_ES2015_SYMBOL_JS),
    ("lib.es2017.sharedmemory.js", LIB_ES2017_SHAREDMEMORY_JS),
    ("lib.es2018.asyncgenerator.js", LIB_ES2018_ASYNCGENERATOR_JS),
    ("lib.es2018.asynciterable.js", LIB_ES2018_ASYNCITERABLE_JS),
    ("lib.es2019.array.js", LIB_ES2019_ARRAY_JS),
    ("lib.es2020.bigint.js", LIB_ES2020_BIGINT_JS),
    ("lib.es2020.intl.js", LIB_ES2020_INTL_JS),
    ("lib.es2020.promise.js", LIB_ES2020_PROMISE_JS),
    (
        "lib.es2020.symbol.wellknown.js",
        LIB_ES2020_SYMBOL_WELLKNOWN_JS,
    ),
    ("lib.es2021.promise.js", LIB_ES2021_PROMISE_JS),
    ("lib.es2021.weakref.js", LIB_ES2021_WEAKREF_JS),
    ("lib.es2022.error.js", LIB_ES2022_ERROR_JS),
    ("lib.es2024.promise.js", LIB_ES2024_PROMISE_JS),
    ("lib.es5.js", LIB_ES5_JS),
    ("lib.esnext.collection.js", LIB_ESNEXT_COLLECTION_JS),
    ("lib.esnext.float16.js", LIB_ESNEXT_FLOAT16_JS),
    ("misc.js", MISC_JS),
    ("react.js", REACT_JS),
];

pub static CONTENTS: LazyLock<Vec<(&str, &str)>> = LazyLock::new(|| {
    let mut contents = COMMON_CONTENTS.to_vec();
    contents.push(("dom_extra_to_be_removed.js", DOM_EXTRA_TO_BE_REMOVED_JS));
    contents
});

pub static HASH: LazyLock<String> = LazyLock::new(|| {
    let mut state = xx::State::new(0);
    for (file, contents) in CONTENTS.iter() {
        state.update(file.as_bytes());
        state.update(contents.as_bytes());
    }
    format!("{:016x}", state.digest())
});
