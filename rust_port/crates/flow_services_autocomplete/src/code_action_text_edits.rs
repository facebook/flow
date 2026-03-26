/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::TextEdit;

pub struct CodeActionTextEdits {
    pub title: String,
    pub edits: Vec<TextEdit>,
    pub from: String,
}
