/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(test)]
mod expect_test;
mod matcher_base;
mod score_match;
#[cfg(test)]
mod test;

use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(Debug, Clone)]
pub struct Options {
    pub first_match_can_be_weak: bool,
    pub num_threads: usize,
    pub max_results: usize,
    /// Whether to use the weights of each item
    pub weighted: bool,
}

#[derive(Debug, Clone)]
pub struct MatchResult {
    pub value: FlowSmolStr,
    pub score: i32,
}

#[derive(Debug, Clone)]
pub struct FuzzyPath {
    matcher: matcher_base::MatcherBase,
}

pub fn default_options() -> Options {
    Options {
        first_match_can_be_weak: true,
        num_threads: 1,
        max_results: usize::MAX,
        weighted: false,
    }
}

impl FuzzyPath {
    pub fn search(&mut self, query: &str, options: &Options) -> Vec<MatchResult> {
        self.matcher
            .find_matches(query, options)
            .into_iter()
            .map(|r| MatchResult {
                value: r.value,
                score: r.score as i32,
            })
            .collect()
    }

    pub fn add_candidate(&mut self, name: FlowSmolStr, weight: i32) {
        self.matcher.add_candidate(name, weight);
    }

    pub fn add_candidates(&mut self, to_add: Vec<(FlowSmolStr, i32)>) {
        for (value, weight) in to_add {
            self.matcher.add_candidate(value, weight);
        }
    }

    pub fn remove_candidate(&mut self, to_rem: &FlowSmolStr) {
        self.matcher.remove_candidate(to_rem);
    }

    pub fn remove_candidates(&mut self, to_rem: &[FlowSmolStr]) {
        for name in to_rem {
            self.remove_candidate(name);
        }
    }

    pub fn init(candidates: Vec<(FlowSmolStr, i32)>) -> Self {
        let mut fp = FuzzyPath {
            matcher: matcher_base::MatcherBase::new(),
        };
        fp.add_candidates(candidates);
        fp
    }
}

pub fn fuzzy_score(
    boost_full_match: bool,
    first_match_can_be_weak: bool,
    pattern: &str,
    word: &str,
) -> Option<i32> {
    let word_lower = word.to_ascii_lowercase();
    let pattern_lower = pattern.to_ascii_lowercase();
    let options = score_match::MatchOptions {
        boost_full_match,
        first_match_can_be_weak,
    };
    score_match::fuzzy_score(word, &word_lower, pattern, &pattern_lower, &options).map(|s| s as i32)
}
