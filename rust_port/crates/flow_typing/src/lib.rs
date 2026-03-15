/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(box_patterns)]
#![feature(never_type)]
#![feature(if_let_guard)]

pub mod cycles;
pub mod env_resolution;
pub mod marked;
pub mod merge;
pub mod ty_members;
pub mod ty_normalizer_flow;
pub mod ty_normalizer_imports;
pub mod type_inference;
pub mod typed_ast_finder;
