/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(never_type)]

pub mod bindings;
pub mod hoister;
pub mod infer_type_hoister;
pub mod property_assignment;
pub mod scope_api;
pub mod scope_builder;
#[cfg(test)]
mod scope_builder_tests;
pub mod ssa_api;
pub mod ssa_builder;
#[cfg(test)]
mod ssa_builder_tests;
pub mod test_utils;
