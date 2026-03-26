/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::loc::Loc;

pub struct TextEdit {
    pub range: Loc,
    pub new_text: String,
}

pub struct CodeActionTextEdits {
    pub title: String,
    pub edits: Vec<TextEdit>,
    pub from: String,
}
