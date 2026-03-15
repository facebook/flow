/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(box_patterns)]
#![feature(never_type)]

pub mod env_api;
pub mod eq_test;
#[cfg(test)]
mod find_provider_test;
pub mod find_providers;
pub mod invalidation_api;
pub mod name_def;
pub mod name_def_types;
pub mod nonvoid_return;
pub mod pattern_helper;
pub mod provider_api;
pub mod refinement_key;
pub mod selector;
pub mod ssa_val;
