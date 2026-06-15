/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use flow_common_xx as xx;

#[cfg(fbcode_build)]
macro_rules! include_tslib {
    ($file:literal) => {
        include_str!(concat!("tslib/", $file))
    };
}

#[cfg(not(fbcode_build))]
macro_rules! include_tslib {
    ($file:literal) => {
        include_str!(concat!("../../../../tslib/", $file))
    };
}

pub static CONTENTS: &[(&str, &str)] = &[
    ("lib.d.ts", include_tslib!("lib.d.ts")),
    ("lib.decorators.d.ts", include_tslib!("lib.decorators.d.ts")),
    (
        "lib.decorators.legacy.d.ts",
        include_tslib!("lib.decorators.legacy.d.ts"),
    ),
    (
        "lib.dom.asynciterable.d.ts",
        include_tslib!("lib.dom.asynciterable.d.ts"),
    ),
    ("lib.dom.d.ts", include_tslib!("lib.dom.d.ts")),
    (
        "lib.dom.iterable.d.ts",
        include_tslib!("lib.dom.iterable.d.ts"),
    ),
    (
        "lib.es2015.collection.d.ts",
        include_tslib!("lib.es2015.collection.d.ts"),
    ),
    (
        "lib.es2015.core.d.ts",
        include_tslib!("lib.es2015.core.d.ts"),
    ),
    ("lib.es2015.d.ts", include_tslib!("lib.es2015.d.ts")),
    (
        "lib.es2015.generator.d.ts",
        include_tslib!("lib.es2015.generator.d.ts"),
    ),
    (
        "lib.es2015.iterable.d.ts",
        include_tslib!("lib.es2015.iterable.d.ts"),
    ),
    (
        "lib.es2015.promise.d.ts",
        include_tslib!("lib.es2015.promise.d.ts"),
    ),
    (
        "lib.es2015.proxy.d.ts",
        include_tslib!("lib.es2015.proxy.d.ts"),
    ),
    (
        "lib.es2015.reflect.d.ts",
        include_tslib!("lib.es2015.reflect.d.ts"),
    ),
    (
        "lib.es2015.symbol.d.ts",
        include_tslib!("lib.es2015.symbol.d.ts"),
    ),
    (
        "lib.es2015.symbol.wellknown.d.ts",
        include_tslib!("lib.es2015.symbol.wellknown.d.ts"),
    ),
    (
        "lib.es2016.array.include.d.ts",
        include_tslib!("lib.es2016.array.include.d.ts"),
    ),
    ("lib.es2016.d.ts", include_tslib!("lib.es2016.d.ts")),
    (
        "lib.es2016.full.d.ts",
        include_tslib!("lib.es2016.full.d.ts"),
    ),
    (
        "lib.es2016.intl.d.ts",
        include_tslib!("lib.es2016.intl.d.ts"),
    ),
    (
        "lib.es2017.arraybuffer.d.ts",
        include_tslib!("lib.es2017.arraybuffer.d.ts"),
    ),
    ("lib.es2017.d.ts", include_tslib!("lib.es2017.d.ts")),
    (
        "lib.es2017.date.d.ts",
        include_tslib!("lib.es2017.date.d.ts"),
    ),
    (
        "lib.es2017.full.d.ts",
        include_tslib!("lib.es2017.full.d.ts"),
    ),
    (
        "lib.es2017.intl.d.ts",
        include_tslib!("lib.es2017.intl.d.ts"),
    ),
    (
        "lib.es2017.object.d.ts",
        include_tslib!("lib.es2017.object.d.ts"),
    ),
    (
        "lib.es2017.sharedmemory.d.ts",
        include_tslib!("lib.es2017.sharedmemory.d.ts"),
    ),
    (
        "lib.es2017.string.d.ts",
        include_tslib!("lib.es2017.string.d.ts"),
    ),
    (
        "lib.es2017.typedarrays.d.ts",
        include_tslib!("lib.es2017.typedarrays.d.ts"),
    ),
    (
        "lib.es2018.asyncgenerator.d.ts",
        include_tslib!("lib.es2018.asyncgenerator.d.ts"),
    ),
    (
        "lib.es2018.asynciterable.d.ts",
        include_tslib!("lib.es2018.asynciterable.d.ts"),
    ),
    ("lib.es2018.d.ts", include_tslib!("lib.es2018.d.ts")),
    (
        "lib.es2018.full.d.ts",
        include_tslib!("lib.es2018.full.d.ts"),
    ),
    (
        "lib.es2018.intl.d.ts",
        include_tslib!("lib.es2018.intl.d.ts"),
    ),
    (
        "lib.es2018.promise.d.ts",
        include_tslib!("lib.es2018.promise.d.ts"),
    ),
    (
        "lib.es2018.regexp.d.ts",
        include_tslib!("lib.es2018.regexp.d.ts"),
    ),
    (
        "lib.es2019.array.d.ts",
        include_tslib!("lib.es2019.array.d.ts"),
    ),
    ("lib.es2019.d.ts", include_tslib!("lib.es2019.d.ts")),
    (
        "lib.es2019.full.d.ts",
        include_tslib!("lib.es2019.full.d.ts"),
    ),
    (
        "lib.es2019.intl.d.ts",
        include_tslib!("lib.es2019.intl.d.ts"),
    ),
    (
        "lib.es2019.object.d.ts",
        include_tslib!("lib.es2019.object.d.ts"),
    ),
    (
        "lib.es2019.string.d.ts",
        include_tslib!("lib.es2019.string.d.ts"),
    ),
    (
        "lib.es2019.symbol.d.ts",
        include_tslib!("lib.es2019.symbol.d.ts"),
    ),
    (
        "lib.es2020.bigint.d.ts",
        include_tslib!("lib.es2020.bigint.d.ts"),
    ),
    ("lib.es2020.d.ts", include_tslib!("lib.es2020.d.ts")),
    (
        "lib.es2020.date.d.ts",
        include_tslib!("lib.es2020.date.d.ts"),
    ),
    (
        "lib.es2020.full.d.ts",
        include_tslib!("lib.es2020.full.d.ts"),
    ),
    (
        "lib.es2020.intl.d.ts",
        include_tslib!("lib.es2020.intl.d.ts"),
    ),
    (
        "lib.es2020.number.d.ts",
        include_tslib!("lib.es2020.number.d.ts"),
    ),
    (
        "lib.es2020.promise.d.ts",
        include_tslib!("lib.es2020.promise.d.ts"),
    ),
    (
        "lib.es2020.sharedmemory.d.ts",
        include_tslib!("lib.es2020.sharedmemory.d.ts"),
    ),
    (
        "lib.es2020.string.d.ts",
        include_tslib!("lib.es2020.string.d.ts"),
    ),
    (
        "lib.es2020.symbol.wellknown.d.ts",
        include_tslib!("lib.es2020.symbol.wellknown.d.ts"),
    ),
    ("lib.es2021.d.ts", include_tslib!("lib.es2021.d.ts")),
    (
        "lib.es2021.full.d.ts",
        include_tslib!("lib.es2021.full.d.ts"),
    ),
    (
        "lib.es2021.intl.d.ts",
        include_tslib!("lib.es2021.intl.d.ts"),
    ),
    (
        "lib.es2021.promise.d.ts",
        include_tslib!("lib.es2021.promise.d.ts"),
    ),
    (
        "lib.es2021.string.d.ts",
        include_tslib!("lib.es2021.string.d.ts"),
    ),
    (
        "lib.es2021.weakref.d.ts",
        include_tslib!("lib.es2021.weakref.d.ts"),
    ),
    (
        "lib.es2022.array.d.ts",
        include_tslib!("lib.es2022.array.d.ts"),
    ),
    ("lib.es2022.d.ts", include_tslib!("lib.es2022.d.ts")),
    (
        "lib.es2022.error.d.ts",
        include_tslib!("lib.es2022.error.d.ts"),
    ),
    (
        "lib.es2022.full.d.ts",
        include_tslib!("lib.es2022.full.d.ts"),
    ),
    (
        "lib.es2022.intl.d.ts",
        include_tslib!("lib.es2022.intl.d.ts"),
    ),
    (
        "lib.es2022.object.d.ts",
        include_tslib!("lib.es2022.object.d.ts"),
    ),
    (
        "lib.es2022.regexp.d.ts",
        include_tslib!("lib.es2022.regexp.d.ts"),
    ),
    (
        "lib.es2022.string.d.ts",
        include_tslib!("lib.es2022.string.d.ts"),
    ),
    (
        "lib.es2023.array.d.ts",
        include_tslib!("lib.es2023.array.d.ts"),
    ),
    (
        "lib.es2023.collection.d.ts",
        include_tslib!("lib.es2023.collection.d.ts"),
    ),
    ("lib.es2023.d.ts", include_tslib!("lib.es2023.d.ts")),
    (
        "lib.es2023.full.d.ts",
        include_tslib!("lib.es2023.full.d.ts"),
    ),
    (
        "lib.es2023.intl.d.ts",
        include_tslib!("lib.es2023.intl.d.ts"),
    ),
    (
        "lib.es2024.arraybuffer.d.ts",
        include_tslib!("lib.es2024.arraybuffer.d.ts"),
    ),
    (
        "lib.es2024.collection.d.ts",
        include_tslib!("lib.es2024.collection.d.ts"),
    ),
    ("lib.es2024.d.ts", include_tslib!("lib.es2024.d.ts")),
    (
        "lib.es2024.full.d.ts",
        include_tslib!("lib.es2024.full.d.ts"),
    ),
    (
        "lib.es2024.object.d.ts",
        include_tslib!("lib.es2024.object.d.ts"),
    ),
    (
        "lib.es2024.promise.d.ts",
        include_tslib!("lib.es2024.promise.d.ts"),
    ),
    (
        "lib.es2024.regexp.d.ts",
        include_tslib!("lib.es2024.regexp.d.ts"),
    ),
    (
        "lib.es2024.sharedmemory.d.ts",
        include_tslib!("lib.es2024.sharedmemory.d.ts"),
    ),
    (
        "lib.es2024.string.d.ts",
        include_tslib!("lib.es2024.string.d.ts"),
    ),
    ("lib.es5.d.ts", include_tslib!("lib.es5.d.ts")),
    ("lib.es6.d.ts", include_tslib!("lib.es6.d.ts")),
    (
        "lib.esnext.array.d.ts",
        include_tslib!("lib.esnext.array.d.ts"),
    ),
    (
        "lib.esnext.collection.d.ts",
        include_tslib!("lib.esnext.collection.d.ts"),
    ),
    ("lib.esnext.d.ts", include_tslib!("lib.esnext.d.ts")),
    (
        "lib.esnext.decorators.d.ts",
        include_tslib!("lib.esnext.decorators.d.ts"),
    ),
    (
        "lib.esnext.disposable.d.ts",
        include_tslib!("lib.esnext.disposable.d.ts"),
    ),
    (
        "lib.esnext.error.d.ts",
        include_tslib!("lib.esnext.error.d.ts"),
    ),
    (
        "lib.esnext.float16.d.ts",
        include_tslib!("lib.esnext.float16.d.ts"),
    ),
    (
        "lib.esnext.full.d.ts",
        include_tslib!("lib.esnext.full.d.ts"),
    ),
    (
        "lib.esnext.intl.d.ts",
        include_tslib!("lib.esnext.intl.d.ts"),
    ),
    (
        "lib.esnext.iterator.d.ts",
        include_tslib!("lib.esnext.iterator.d.ts"),
    ),
    (
        "lib.esnext.promise.d.ts",
        include_tslib!("lib.esnext.promise.d.ts"),
    ),
    (
        "lib.esnext.sharedmemory.d.ts",
        include_tslib!("lib.esnext.sharedmemory.d.ts"),
    ),
    ("lib.scripthost.d.ts", include_tslib!("lib.scripthost.d.ts")),
];

pub static HASH: LazyLock<String> = LazyLock::new(|| {
    let mut state = xx::State::new(0);
    for (file, contents) in CONTENTS {
        state.update(file.as_bytes());
        state.update(contents.as_bytes());
    }
    format!("{:016x}", state.digest())
});
