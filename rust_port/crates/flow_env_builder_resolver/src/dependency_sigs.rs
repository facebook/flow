/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_common::options::JsxMode;
use flow_common::options::ReactRuntime;
use flow_parser::file_key::FileKey;
use flow_typing_errors::error_message::ErrorMessage;

pub trait Context {
    fn enable_enums(&self) -> bool;
    fn file(&self) -> FileKey;
    fn jsx(&self) -> JsxMode;
    fn react_runtime(&self) -> ReactRuntime;
    fn enable_const_params(&self) -> bool;
    fn stylex_shorthand_prop(&self) -> Option<&str>;
    fn add_exhaustive_check(&self, loc: ALoc, cases: (Vec<ALoc>, bool));
    fn exhaustive_check(&self, loc: &ALoc) -> Option<(Vec<ALoc>, bool)>;
}

pub trait Flow {
    type Cx;
    fn add_output(cx: &Self::Cx, error: ErrorMessage<ALoc>);
}
