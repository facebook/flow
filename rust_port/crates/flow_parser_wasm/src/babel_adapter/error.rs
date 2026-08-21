/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::loc::Loc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum BabelLoweringError {
    #[error("{message}")]
    Syntax { message: String, loc: Loc },
}

impl BabelLoweringError {
    pub(crate) fn syntax(loc: &Loc, message: impl Into<String>) -> Self {
        Self::Syntax {
            message: message.into(),
            loc: loc.clone(),
        }
    }
}
