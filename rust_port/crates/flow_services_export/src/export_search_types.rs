/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::export_index::Kind;
use crate::export_index::Source;

#[derive(Debug, Clone)]
pub struct SearchResult {
    pub name: FlowSmolStr,
    pub source: Source,
    pub kind: Kind,
}

#[derive(Debug, Clone)]
pub struct SearchResultScored {
    pub search_result: SearchResult,
    pub score: i32,
    pub weight: i32,
}

#[derive(Debug, Clone)]
pub struct SearchResults {
    pub results: Vec<SearchResultScored>,
    pub is_incomplete: bool,
}

pub fn empty_search_results() -> SearchResults {
    SearchResults {
        results: Vec::new(),
        is_incomplete: false,
    }
}
