/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::export_index;
use crate::export_index::Export;
use crate::export_index::ExportIndex;
use crate::export_index::ExportMap;
use crate::export_index::Kind;
use crate::export_index::Source;
use crate::export_search_types::SearchResult;
use crate::export_search_types::SearchResultScored;
use crate::export_search_types::SearchResults;
use crate::fuzzy_path;
use crate::fuzzy_path::FuzzyPath;

#[derive(Debug, Clone)]
pub struct ExportSearch {
    index: ExportIndex,
    value_matcher: FuzzyPath,
    type_matcher: FuzzyPath,
}

pub type SearchOptions = fuzzy_path::Options;

pub fn default_options() -> SearchOptions {
    let mut opts = fuzzy_path::default_options();
    opts.first_match_can_be_weak = false;
    opts
}

struct Candidates {
    values: Vec<(FlowSmolStr, i32)>,
    types: Vec<(FlowSmolStr, i32)>,
}

fn summarize_exports(exports: &ExportMap<i32>) -> (bool, bool, i32) {
    let mut has_value = false;
    let mut has_type = false;
    let mut max_count: i32 = 0;
    for (Export(_file, kind), num) in exports {
        max_count = max_count.max(*num);
        match kind {
            Kind::DefaultType => {
                has_type = true;
            }
            Kind::Default => {
                has_value = true;
            }
            Kind::Named => {
                has_value = true;
            }
            Kind::NamedType => {
                has_type = true;
            }
            Kind::Namespace => {
                has_value = true;
            }
        }
    }
    (has_value, has_type, max_count)
}

fn partition_candidates(index: &ExportIndex) -> Candidates {
    let mut values: Vec<(FlowSmolStr, i32)> = Vec::new();
    let mut types: Vec<(FlowSmolStr, i32)> = Vec::new();
    for (name, exports) in index {
        let (has_value, has_type, max_count) = summarize_exports(exports);
        if has_value {
            values.push((name.dupe(), max_count));
        }
        if has_type {
            types.push((name.dupe(), max_count));
        }
    }
    Candidates { values, types }
}

pub fn init(index: ExportIndex) -> ExportSearch {
    let Candidates { values, types } = partition_candidates(&index);
    let value_matcher = FuzzyPath::init(values);
    let type_matcher = FuzzyPath::init(types);
    ExportSearch {
        index,
        value_matcher,
        type_matcher,
    }
}

pub fn merge_available_exports(
    old_available_exports_index: &ExportIndex,
    new_available_exports_index: &ExportIndex,
    search: &ExportSearch,
) -> ExportSearch {
    let (addition_index, removal_index) =
        export_index::diff(old_available_exports_index, new_available_exports_index);
    let (index, dead_candidates) = export_index::subtract(&removal_index, &search.index);
    let mut value_matcher = search.value_matcher.clone();
    value_matcher.remove_candidates(&dead_candidates);
    let mut type_matcher = search.type_matcher.clone();
    type_matcher.remove_candidates(&dead_candidates);
    let index = export_index::merge(&addition_index, &index);
    let Candidates { values, types } = partition_candidates(&addition_index);
    value_matcher.add_candidates(values);
    type_matcher.add_candidates(types);
    ExportSearch {
        index,
        value_matcher,
        type_matcher,
    }
}

pub fn subtract_count(removed_imports: &ExportIndex, search: &ExportSearch) -> ExportSearch {
    let index = export_index::subtract_count(removed_imports, &search.index);
    ExportSearch {
        index,
        value_matcher: search.value_matcher.clone(),
        type_matcher: search.type_matcher.clone(),
    }
}

// (*Merge_import *)
pub fn merge_export_import(new_index: &ExportIndex, search: &ExportSearch) -> ExportSearch {
    let index = export_index::merge_export_import(new_index, &search.index);
    ExportSearch {
        index,
        value_matcher: search.value_matcher.clone(),
        type_matcher: search.type_matcher.clone(),
    }
}

enum Query {
    Value(String),
    Type(String),
    BothValueAndType(String),
}

fn string_of_query(query: &Query) -> &str {
    match query {
        Query::Value(s) | Query::Type(s) | Query::BothValueAndType(s) => s,
    }
}

fn search_result_of_export(
    query: &Query,
    name: &str,
    source: Source,
    kind: Kind,
) -> Option<SearchResult> {
    match (query, &kind) {
        (
            Query::Value(_) | Query::BothValueAndType(_),
            Kind::Default | Kind::Named | Kind::Namespace,
        )
        | (Query::Type(_) | Query::BothValueAndType(_), Kind::DefaultType | Kind::NamedType) => {
            Some(SearchResult {
                name: FlowSmolStr::new(name),
                source,
                kind,
            })
        }
        (Query::Value(_), Kind::DefaultType | Kind::NamedType)
        | (Query::Type(_), Kind::Default | Kind::Named | Kind::Namespace) => None,
    }
}

fn compare_search_result(
    compare_score: &impl Fn(&SearchResultScored, &SearchResultScored) -> std::cmp::Ordering,
    a: &SearchResultScored,
    b: &SearchResultScored,
) -> std::cmp::Ordering {
    match compare_score(a, b) {
        std::cmp::Ordering::Equal => match b.weight.cmp(&a.weight) {
            std::cmp::Ordering::Equal => match a.search_result.name.cmp(&b.search_result.name) {
                std::cmp::Ordering::Equal => {
                    let a_export =
                        Export(a.search_result.source.clone(), a.search_result.kind.clone());
                    let b_export =
                        Export(b.search_result.source.clone(), b.search_result.kind.clone());
                    a_export.cmp(&b_export)
                }
                k => k,
            },
            k => k,
        },
        k => k,
    }
}

/// [take ~n:20 ~index matches] will return up to 20 search results,
/// where each match in [matches] might contribute multiple results.
/// sets [is_incomplete] if [n] is exceeded.
fn take(
    weighted: bool,
    n: usize,
    index: &ExportIndex,
    query: &Query,
    fuzzy_matches: &[fuzzy_path::MatchResult],
) -> SearchResults {
    let mut rev_all: Vec<SearchResultScored> = Vec::new();
    for m in fuzzy_matches {
        let exports = export_index::find(&m.value, index);
        for (Export(source, kind), count) in &exports {
            match search_result_of_export(query, &m.value, source.clone(), kind.clone()) {
                Some(search_result) => {
                    let weight = if weighted { *count } else { 0 };
                    rev_all.insert(
                        0,
                        SearchResultScored {
                            search_result,
                            score: m.score,
                            weight,
                        },
                    );
                }
                None => {}
            }
        }
    }

    let compare_score_fn = |a: &SearchResultScored, b: &SearchResultScored| -> std::cmp::Ordering {
        b.score.cmp(&a.score)
    };
    rev_all.sort_by(|a, b| compare_search_result(&compare_score_fn, a, b));
    let sorted = rev_all;

    let top_n: Vec<SearchResultScored> = sorted.iter().take(n).cloned().collect();

    let results = if weighted {
        let query_len = string_of_query(query).len();
        let is_exact =
            |result: &SearchResultScored| -> bool { result.search_result.name.len() == query_len };
        let mut exacts: Vec<SearchResultScored> = Vec::new();
        let mut rest: Vec<SearchResultScored> = Vec::new();
        let mut found_non_exact = false;
        for item in &top_n {
            if !found_non_exact && is_exact(item) {
                exacts.push(item.clone());
            } else {
                found_non_exact = true;
                rest.push(item.clone());
            }
        }
        let next_3: Vec<SearchResultScored> = rest.iter().take(3).cloned().collect();
        let rest: Vec<SearchResultScored> = rest.into_iter().skip(3).collect();
        let adjust_score = |result: &SearchResultScored| -> i32 {
            if is_exact(result) {
                result.score - 2
            } else {
                result.score
            }
        };
        let compare_score_adjusted =
            |a: &SearchResultScored, b: &SearchResultScored| -> std::cmp::Ordering {
                adjust_score(b).cmp(&adjust_score(a))
            };
        let mut top: Vec<SearchResultScored> = exacts;
        top.extend(next_3);
        top.sort_by(|a, b| compare_search_result(&compare_score_adjusted, a, b));
        top.extend(rest);
        top
    } else {
        top_n
    };

    let is_incomplete = sorted.len() > n;
    SearchResults {
        results,
        is_incomplete,
    }
}

fn search_internal(
    options: &SearchOptions,
    query: Query,
    search: &mut ExportSearch,
) -> SearchResults {
    let (matcher, query_txt) = match &query {
        Query::Value(txt) => (&mut search.value_matcher, txt.as_str()),
        Query::Type(txt) => (&mut search.type_matcher, txt.as_str()),
        Query::BothValueAndType(_) => {
            panic!("BothValueAndType query should be handled by search_both_values_and_types")
        }
    };

    let max_results = options.max_results;
    let search_options = if max_results < usize::MAX {
        let mut opts = options.clone();
        opts.max_results = max_results + 1;
        opts
    } else {
        options.clone()
    };

    let weighted = search_options.weighted;

    let fuzzy_matches = matcher.search(query_txt, &search_options);
    take(weighted, max_results, &search.index, &query, &fuzzy_matches)
}

pub fn search_values(
    options: Option<&SearchOptions>,
    query: &str,
    t: &mut ExportSearch,
) -> SearchResults {
    let opts = match options {
        Some(o) => o.clone(),
        None => default_options(),
    };
    search_internal(&opts, Query::Value(query.to_string()), t)
}

pub fn search_types(
    options: Option<&SearchOptions>,
    query: &str,
    t: &mut ExportSearch,
) -> SearchResults {
    let opts = match options {
        Some(o) => o.clone(),
        None => default_options(),
    };
    search_internal(&opts, Query::Type(query.to_string()), t)
}

pub fn search_both_values_and_types(
    options: Option<&SearchOptions>,
    query: &str,
    t: &mut ExportSearch,
) -> SearchResults {
    let options = match options {
        Some(o) => o.clone(),
        None => default_options(),
    };
    let max_results = options.max_results;
    let search_options = if max_results < usize::MAX {
        let mut opts = options.clone();
        opts.max_results = max_results + 1;
        opts
    } else {
        options.clone()
    };

    let weighted = search_options.weighted;
    let value_fuzzy_results = t.value_matcher.search(query, &search_options);
    let type_fuzzy_results = t.type_matcher.search(query, &search_options);

    let mut combined = value_fuzzy_results;
    combined.extend(type_fuzzy_results);
    let SearchResults {
        results,
        is_incomplete,
    } = take(
        weighted,
        max_results,
        &t.index,
        &Query::BothValueAndType(query.to_string()),
        &combined,
    );

    let mut seen: BTreeMap<Export, std::collections::BTreeSet<FlowSmolStr>> = BTreeMap::new();
    let results = results
        .into_iter()
        .filter(|item| {
            let name = &item.search_result.name;
            let source = &item.search_result.source;
            let key = Export(source.clone(), Kind::Named);
            let name_set = seen.entry(key).or_default();
            if name_set.contains(name) {
                false
            } else {
                name_set.insert(name.dupe());
                true
            }
        })
        .collect();

    SearchResults {
        results,
        is_incomplete,
    }
}

// let get_index t = t.index
pub fn get_index(t: &ExportSearch) -> &ExportIndex {
    &t.index
}

pub fn get(name: &str, t: &ExportSearch) -> ExportMap<i32> {
    export_index::find(name, &t.index)
}

pub fn get_values(name: &str, t: &ExportSearch) -> ExportMap<i32> {
    get(name, t)
        .into_iter()
        .filter(|(Export(_file_key, kind), _num)| export_index::kind_is_value(kind))
        .collect()
}

pub fn get_types(name: &str, t: &ExportSearch) -> ExportMap<i32> {
    get(name, t)
        .into_iter()
        .filter(|(Export(_file_key, kind), _num)| export_index::kind_is_type(kind))
        .collect()
}

impl std::fmt::Display for ExportSearch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.index)
    }
}
