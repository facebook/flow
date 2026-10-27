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

pub(super) const CSSTYPE_D_TS: &str = include_tslib!("csstype.d.ts");
pub(super) const LIB_DECORATORS_D_TS: &str = include_tslib!("lib.decorators.d.ts");
pub(super) const LIB_DECORATORS_LEGACY_D_TS: &str = include_tslib!("lib.decorators.legacy.d.ts");
pub(super) const LIB_DOM_ASYNCITERABLE_D_TS: &str = include_tslib!("lib.dom.asynciterable.d.ts");
pub(super) const LIB_DOM_D_TS: &str = include_tslib!("lib.dom.d.ts");
pub(super) const LIB_DOM_ITERABLE_D_TS: &str = include_tslib!("lib.dom.iterable.d.ts");
pub(super) const LIB_ES2015_GENERATOR_D_TS: &str = include_tslib!("lib.es2015.generator.d.ts");
pub(super) const LIB_ES2015_PROXY_D_TS: &str = include_tslib!("lib.es2015.proxy.d.ts");
pub(super) const LIB_ES2015_REFLECT_D_TS: &str = include_tslib!("lib.es2015.reflect.d.ts");
pub(super) const LIB_ES2015_SYMBOL_D_TS: &str = include_tslib!("lib.es2015.symbol.d.ts");
pub(super) const LIB_ES2015_SYMBOL_WELLKNOWN_D_TS: &str =
    include_tslib!("lib.es2015.symbol.wellknown.d.ts");
pub(super) const LIB_ES2016_ARRAY_INCLUDE_D_TS: &str =
    include_tslib!("lib.es2016.array.include.d.ts");
pub(super) const LIB_ES2016_INTL_D_TS: &str = include_tslib!("lib.es2016.intl.d.ts");
pub(super) const LIB_ES2017_ARRAYBUFFER_D_TS: &str = include_tslib!("lib.es2017.arraybuffer.d.ts");
pub(super) const LIB_ES2017_DATE_D_TS: &str = include_tslib!("lib.es2017.date.d.ts");
pub(super) const LIB_ES2017_INTL_D_TS: &str = include_tslib!("lib.es2017.intl.d.ts");
pub(super) const LIB_ES2017_OBJECT_D_TS: &str = include_tslib!("lib.es2017.object.d.ts");
pub(super) const LIB_ES2017_SHAREDMEMORY_D_TS: &str =
    include_tslib!("lib.es2017.sharedmemory.d.ts");
pub(super) const LIB_ES2017_STRING_D_TS: &str = include_tslib!("lib.es2017.string.d.ts");
pub(super) const LIB_ES2017_TYPEDARRAYS_D_TS: &str = include_tslib!("lib.es2017.typedarrays.d.ts");
pub(super) const LIB_ES2018_ASYNCGENERATOR_D_TS: &str =
    include_tslib!("lib.es2018.asyncgenerator.d.ts");
pub(super) const LIB_ES2018_ASYNCITERABLE_D_TS: &str =
    include_tslib!("lib.es2018.asynciterable.d.ts");
pub(super) const LIB_ES2018_INTL_D_TS: &str = include_tslib!("lib.es2018.intl.d.ts");
pub(super) const LIB_ES2018_PROMISE_D_TS: &str = include_tslib!("lib.es2018.promise.d.ts");
pub(super) const LIB_ES2018_REGEXP_D_TS: &str = include_tslib!("lib.es2018.regexp.d.ts");
pub(super) const LIB_ES2019_ARRAY_D_TS: &str = include_tslib!("lib.es2019.array.d.ts");
pub(super) const LIB_ES2019_INTL_D_TS: &str = include_tslib!("lib.es2019.intl.d.ts");
pub(super) const LIB_ES2019_STRING_D_TS: &str = include_tslib!("lib.es2019.string.d.ts");
pub(super) const LIB_ES2019_OBJECT_D_TS: &str = include_tslib!("lib.es2019.object.d.ts");
pub(super) const LIB_ES2019_SYMBOL_D_TS: &str = include_tslib!("lib.es2019.symbol.d.ts");
pub(super) const LIB_ES2020_BIGINT_D_TS: &str = include_tslib!("lib.es2020.bigint.d.ts");
pub(super) const LIB_ES2020_DATE_D_TS: &str = include_tslib!("lib.es2020.date.d.ts");
pub(super) const LIB_ES2020_INTL_D_TS: &str = include_tslib!("lib.es2020.intl.d.ts");
pub(super) const LIB_ES2020_NUMBER_D_TS: &str = include_tslib!("lib.es2020.number.d.ts");
pub(super) const LIB_ES2020_PROMISE_D_TS: &str = include_tslib!("lib.es2020.promise.d.ts");
pub(super) const LIB_ES2020_SHAREDMEMORY_D_TS: &str =
    include_tslib!("lib.es2020.sharedmemory.d.ts");
pub(super) const LIB_ES2020_STRING_D_TS: &str = include_tslib!("lib.es2020.string.d.ts");
pub(super) const LIB_ES2020_SYMBOL_WELLKNOWN_D_TS: &str =
    include_tslib!("lib.es2020.symbol.wellknown.d.ts");
pub(super) const LIB_ES2021_INTL_D_TS: &str = include_tslib!("lib.es2021.intl.d.ts");
pub(super) const LIB_ES2021_PROMISE_D_TS: &str = include_tslib!("lib.es2021.promise.d.ts");
pub(super) const LIB_ES2021_STRING_D_TS: &str = include_tslib!("lib.es2021.string.d.ts");
pub(super) const LIB_ES2021_WEAKREF_D_TS: &str = include_tslib!("lib.es2021.weakref.d.ts");
pub(super) const LIB_ES2023_COLLECTION_D_TS: &str = include_tslib!("lib.es2023.collection.d.ts");
pub(super) const LIB_ES2023_ARRAY_D_TS: &str = include_tslib!("lib.es2023.array.d.ts");
pub(super) const LIB_ES2023_INTL_D_TS: &str = include_tslib!("lib.es2023.intl.d.ts");
pub(super) const LIB_ES2022_ARRAY_D_TS: &str = include_tslib!("lib.es2022.array.d.ts");
pub(super) const LIB_ES2022_ERROR_D_TS: &str = include_tslib!("lib.es2022.error.d.ts");
pub(super) const LIB_ES2022_INTL_D_TS: &str = include_tslib!("lib.es2022.intl.d.ts");
pub(super) const LIB_ES2022_OBJECT_D_TS: &str = include_tslib!("lib.es2022.object.d.ts");
pub(super) const LIB_ES2022_REGEXP_D_TS: &str = include_tslib!("lib.es2022.regexp.d.ts");
pub(super) const LIB_ES2022_STRING_D_TS: &str = include_tslib!("lib.es2022.string.d.ts");
pub(super) const LIB_ES2024_ARRAYBUFFER_D_TS: &str = include_tslib!("lib.es2024.arraybuffer.d.ts");
pub(super) const LIB_ES2024_ARRAYBUFFER_JS: &str = concat!(
    "/* flowlint unsafe-getters-setters:off */",
    include_tslib!("lib.es2024.arraybuffer.d.ts"),
);
pub(super) const LIB_ES2024_COLLECTION_D_TS: &str = include_tslib!("lib.es2024.collection.d.ts");
pub(super) const LIB_ES2024_OBJECT_D_TS: &str = include_tslib!("lib.es2024.object.d.ts");
pub(super) const LIB_ES2024_PROMISE_D_TS: &str = include_tslib!("lib.es2024.promise.d.ts");
pub(super) const LIB_ES2024_REGEXP_D_TS: &str = include_tslib!("lib.es2024.regexp.d.ts");
pub(super) const LIB_ES2024_SHAREDMEMORY_D_TS: &str =
    include_tslib!("lib.es2024.sharedmemory.d.ts");
pub(super) const LIB_ES2024_SHAREDMEMORY_JS: &str = concat!(
    "/* flowlint unsafe-getters-setters:off */",
    include_tslib!("lib.es2024.sharedmemory.d.ts"),
);
pub(super) const LIB_ES2024_STRING_D_TS: &str = include_tslib!("lib.es2024.string.d.ts");
pub(super) const LIB_ESNEXT_ARRAY_D_TS: &str = include_tslib!("lib.esnext.array.d.ts");
pub(super) const LIB_ESNEXT_DECORATORS_D_TS: &str = include_tslib!("lib.esnext.decorators.d.ts");
pub(super) const LIB_ESNEXT_DISPOSABLE_D_TS: &str = include_tslib!("lib.esnext.disposable.d.ts");
pub(super) const LIB_ESNEXT_FLOAT16_D_TS: &str = include_tslib!("lib.esnext.float16.d.ts");
pub(super) const LIB_ESNEXT_ERROR_D_TS: &str = include_tslib!("lib.esnext.error.d.ts");
pub(super) const LIB_ESNEXT_INTL_D_TS: &str = include_tslib!("lib.esnext.intl.d.ts");
pub(super) const LIB_ESNEXT_PROMISE_D_TS: &str = include_tslib!("lib.esnext.promise.d.ts");
pub(super) const LIB_ESNEXT_SHAREDMEMORY_D_TS: &str =
    include_tslib!("lib.esnext.sharedmemory.d.ts");

pub static CONTENTS: &[(&str, &str)] = &[
    ("lib.d.ts", include_tslib!("lib.d.ts")),
    ("lib.decorators.d.ts", LIB_DECORATORS_D_TS),
    ("lib.decorators.legacy.d.ts", LIB_DECORATORS_LEGACY_D_TS),
    ("lib.dom.asynciterable.d.ts", LIB_DOM_ASYNCITERABLE_D_TS),
    ("lib.dom.d.ts", LIB_DOM_D_TS),
    ("lib.dom.iterable.d.ts", LIB_DOM_ITERABLE_D_TS),
    (
        "lib.es2015.collection.d.ts",
        include_tslib!("lib.es2015.collection.d.ts"),
    ),
    (
        "lib.es2015.core.d.ts",
        include_tslib!("lib.es2015.core.d.ts"),
    ),
    ("lib.es2015.d.ts", include_tslib!("lib.es2015.d.ts")),
    ("lib.es2015.generator.d.ts", LIB_ES2015_GENERATOR_D_TS),
    (
        "lib.es2015.iterable.d.ts",
        include_tslib!("lib.es2015.iterable.d.ts"),
    ),
    (
        "lib.es2015.promise.d.ts",
        include_tslib!("lib.es2015.promise.d.ts"),
    ),
    ("lib.es2015.proxy.d.ts", LIB_ES2015_PROXY_D_TS),
    ("lib.es2015.reflect.d.ts", LIB_ES2015_REFLECT_D_TS),
    ("lib.es2015.symbol.d.ts", LIB_ES2015_SYMBOL_D_TS),
    (
        "lib.es2015.symbol.wellknown.d.ts",
        LIB_ES2015_SYMBOL_WELLKNOWN_D_TS,
    ),
    (
        "lib.es2016.array.include.d.ts",
        LIB_ES2016_ARRAY_INCLUDE_D_TS,
    ),
    ("lib.es2016.d.ts", include_tslib!("lib.es2016.d.ts")),
    (
        "lib.es2016.full.d.ts",
        include_tslib!("lib.es2016.full.d.ts"),
    ),
    ("lib.es2016.intl.d.ts", LIB_ES2016_INTL_D_TS),
    ("lib.es2017.arraybuffer.d.ts", LIB_ES2017_ARRAYBUFFER_D_TS),
    ("lib.es2017.d.ts", include_tslib!("lib.es2017.d.ts")),
    ("lib.es2017.date.d.ts", LIB_ES2017_DATE_D_TS),
    (
        "lib.es2017.full.d.ts",
        include_tslib!("lib.es2017.full.d.ts"),
    ),
    ("lib.es2017.intl.d.ts", LIB_ES2017_INTL_D_TS),
    ("lib.es2017.object.d.ts", LIB_ES2017_OBJECT_D_TS),
    ("lib.es2017.sharedmemory.d.ts", LIB_ES2017_SHAREDMEMORY_D_TS),
    ("lib.es2017.string.d.ts", LIB_ES2017_STRING_D_TS),
    ("lib.es2017.typedarrays.d.ts", LIB_ES2017_TYPEDARRAYS_D_TS),
    (
        "lib.es2018.asyncgenerator.d.ts",
        LIB_ES2018_ASYNCGENERATOR_D_TS,
    ),
    (
        "lib.es2018.asynciterable.d.ts",
        LIB_ES2018_ASYNCITERABLE_D_TS,
    ),
    ("lib.es2018.d.ts", include_tslib!("lib.es2018.d.ts")),
    (
        "lib.es2018.full.d.ts",
        include_tslib!("lib.es2018.full.d.ts"),
    ),
    ("lib.es2018.intl.d.ts", LIB_ES2018_INTL_D_TS),
    ("lib.es2018.promise.d.ts", LIB_ES2018_PROMISE_D_TS),
    ("lib.es2018.regexp.d.ts", LIB_ES2018_REGEXP_D_TS),
    ("lib.es2019.array.d.ts", LIB_ES2019_ARRAY_D_TS),
    ("lib.es2019.d.ts", include_tslib!("lib.es2019.d.ts")),
    (
        "lib.es2019.full.d.ts",
        include_tslib!("lib.es2019.full.d.ts"),
    ),
    ("lib.es2019.intl.d.ts", LIB_ES2019_INTL_D_TS),
    ("lib.es2019.object.d.ts", LIB_ES2019_OBJECT_D_TS),
    ("lib.es2019.string.d.ts", LIB_ES2019_STRING_D_TS),
    ("lib.es2019.symbol.d.ts", LIB_ES2019_SYMBOL_D_TS),
    ("lib.es2020.bigint.d.ts", LIB_ES2020_BIGINT_D_TS),
    ("lib.es2020.d.ts", include_tslib!("lib.es2020.d.ts")),
    ("lib.es2020.date.d.ts", LIB_ES2020_DATE_D_TS),
    (
        "lib.es2020.full.d.ts",
        include_tslib!("lib.es2020.full.d.ts"),
    ),
    ("lib.es2020.intl.d.ts", LIB_ES2020_INTL_D_TS),
    ("lib.es2020.number.d.ts", LIB_ES2020_NUMBER_D_TS),
    ("lib.es2020.promise.d.ts", LIB_ES2020_PROMISE_D_TS),
    ("lib.es2020.sharedmemory.d.ts", LIB_ES2020_SHAREDMEMORY_D_TS),
    ("lib.es2020.string.d.ts", LIB_ES2020_STRING_D_TS),
    (
        "lib.es2020.symbol.wellknown.d.ts",
        LIB_ES2020_SYMBOL_WELLKNOWN_D_TS,
    ),
    ("lib.es2021.d.ts", include_tslib!("lib.es2021.d.ts")),
    (
        "lib.es2021.full.d.ts",
        include_tslib!("lib.es2021.full.d.ts"),
    ),
    ("lib.es2021.intl.d.ts", LIB_ES2021_INTL_D_TS),
    ("lib.es2021.promise.d.ts", LIB_ES2021_PROMISE_D_TS),
    ("lib.es2021.string.d.ts", LIB_ES2021_STRING_D_TS),
    ("lib.es2021.weakref.d.ts", LIB_ES2021_WEAKREF_D_TS),
    ("lib.es2022.array.d.ts", LIB_ES2022_ARRAY_D_TS),
    ("lib.es2022.d.ts", include_tslib!("lib.es2022.d.ts")),
    ("lib.es2022.error.d.ts", LIB_ES2022_ERROR_D_TS),
    (
        "lib.es2022.full.d.ts",
        include_tslib!("lib.es2022.full.d.ts"),
    ),
    ("lib.es2022.intl.d.ts", LIB_ES2022_INTL_D_TS),
    ("lib.es2022.object.d.ts", LIB_ES2022_OBJECT_D_TS),
    ("lib.es2022.regexp.d.ts", LIB_ES2022_REGEXP_D_TS),
    ("lib.es2022.string.d.ts", LIB_ES2022_STRING_D_TS),
    ("lib.es2023.array.d.ts", LIB_ES2023_ARRAY_D_TS),
    ("lib.es2023.collection.d.ts", LIB_ES2023_COLLECTION_D_TS),
    ("lib.es2023.d.ts", include_tslib!("lib.es2023.d.ts")),
    (
        "lib.es2023.full.d.ts",
        include_tslib!("lib.es2023.full.d.ts"),
    ),
    ("lib.es2023.intl.d.ts", LIB_ES2023_INTL_D_TS),
    ("lib.es2024.arraybuffer.d.ts", LIB_ES2024_ARRAYBUFFER_D_TS),
    ("lib.es2024.collection.d.ts", LIB_ES2024_COLLECTION_D_TS),
    ("lib.es2024.d.ts", include_tslib!("lib.es2024.d.ts")),
    (
        "lib.es2024.full.d.ts",
        include_tslib!("lib.es2024.full.d.ts"),
    ),
    ("lib.es2024.object.d.ts", LIB_ES2024_OBJECT_D_TS),
    ("lib.es2024.promise.d.ts", LIB_ES2024_PROMISE_D_TS),
    ("lib.es2024.regexp.d.ts", LIB_ES2024_REGEXP_D_TS),
    ("lib.es2024.sharedmemory.d.ts", LIB_ES2024_SHAREDMEMORY_D_TS),
    ("lib.es2024.string.d.ts", LIB_ES2024_STRING_D_TS),
    ("lib.es5.d.ts", include_tslib!("lib.es5.d.ts")),
    ("lib.es6.d.ts", include_tslib!("lib.es6.d.ts")),
    ("lib.esnext.array.d.ts", LIB_ESNEXT_ARRAY_D_TS),
    (
        "lib.esnext.collection.d.ts",
        include_tslib!("lib.esnext.collection.d.ts"),
    ),
    ("lib.esnext.d.ts", include_tslib!("lib.esnext.d.ts")),
    ("lib.esnext.decorators.d.ts", LIB_ESNEXT_DECORATORS_D_TS),
    ("lib.esnext.disposable.d.ts", LIB_ESNEXT_DISPOSABLE_D_TS),
    ("lib.esnext.error.d.ts", LIB_ESNEXT_ERROR_D_TS),
    ("lib.esnext.float16.d.ts", LIB_ESNEXT_FLOAT16_D_TS),
    (
        "lib.esnext.full.d.ts",
        include_tslib!("lib.esnext.full.d.ts"),
    ),
    ("lib.esnext.intl.d.ts", LIB_ESNEXT_INTL_D_TS),
    (
        "lib.esnext.iterator.d.ts",
        include_tslib!("lib.esnext.iterator.d.ts"),
    ),
    ("lib.esnext.promise.d.ts", LIB_ESNEXT_PROMISE_D_TS),
    ("lib.esnext.sharedmemory.d.ts", LIB_ESNEXT_SHAREDMEMORY_D_TS),
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
