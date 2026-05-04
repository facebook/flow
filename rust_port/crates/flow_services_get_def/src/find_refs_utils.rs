/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_common::docblock::Docblock;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;

pub type AstInfo = (Arc<ast::Program<Loc, Loc>>, Arc<FileSig>, Arc<Docblock>);
