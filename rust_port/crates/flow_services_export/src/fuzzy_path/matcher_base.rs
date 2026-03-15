/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BinaryHeap;
use std::collections::HashMap;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use super::score_match;

fn letter_bitmask(s: &str) -> u64 {
    let mut result: u64 = 0;
    for c in s.bytes() {
        if c >= b'a' && c <= b'z' {
            let index = (c - b'a') as u32;
            let count_bit = result >> (index * 2);
            // "Increment" the count_bit:
            // 00 -> 01
            // 01 -> 11
            // 11 -> 11
            let count_bit = ((count_bit << 1) | 1) & 3;
            result |= count_bit << (index * 2);
        } else if c == b'-' {
            result |= 1u64 << 52;
        } else if c == b'_' {
            result |= 1u64 << 53;
        } else if c >= b'0' && c <= b'9' {
            result |= 1u64 << ((c - b'0') as u32 + 54);
        }
    }
    result
}

fn push_heap(
    heap: &mut ResultHeap,
    weight: i32,
    score: i64,
    value: &FlowSmolStr,
    max_results: usize,
) {
    let result = MatchResult {
        score,
        weight,
        value: value.dupe(),
    };
    if heap.len() < max_results || result < *heap.peek().unwrap() {
        heap.push(result);
        if heap.len() > max_results {
            heap.pop();
        }
    }
}

#[derive(Debug, Clone)]
struct Candidate {
    value: FlowSmolStr,
    value_lower: String,
    /// A bitmask representing the counts of letters a-z contained in the string.
    /// Bits i*2 and i*2 + 1 represent a count of the i-th letter:
    /// 00 = 0
    /// 01 = 1
    /// 11 = 2
    /// With this scheme, if (bitmask(X) & bitmask(Y)) == bitmask(X) then we
    /// instantly know that Y contains at least all the letters in X.
    bitmask: u64,
    /// True if this was a match against lastQuery_.
    /// Since the most common use case for this library is for typeaheads,
    /// we can often avoid a ton of work by skiping past negatives.
    /// We'll use this only if the new query strictly extends lastQuery_.
    last_match: bool,
    /// Weight to give this value relative to other values.
    weight: i32,
}

pub(super) struct MatchResult {
    pub(super) score: i64,
    pub(super) weight: i32,
    pub(super) value: FlowSmolStr,
}

impl PartialEq for MatchResult {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score && self.weight == other.weight && self.value == other.value
    }
}

impl Eq for MatchResult {}

impl Ord for MatchResult {
    // Order small scores to the top of any priority queue.
    // We need a min-heap to maintain the top-N results.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.score == other.score {
            // In case of a tie, favor the higher weight.
            if self.weight == other.weight {
                // In case of a tie, favour shorter strings.
                let length = self.value.len() as i64 - other.value.len() as i64;
                // In the case of a tie, favor lexicographically-earlier
                if length == 0 {
                    self.value.cmp(&other.value)
                } else if length < 0 {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            } else {
                other.weight.cmp(&self.weight)
            }
        } else {
            other.score.cmp(&self.score)
        }
    }
}

impl PartialOrd for MatchResult {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

type ResultHeap = BinaryHeap<MatchResult>;

#[derive(Debug, Clone)]
pub(super) struct MatcherBase {
    candidates: Vec<Candidate>,
    lookup: HashMap<FlowSmolStr, usize>,
    last_query: String,
    last_query_first_match_can_be_weak: bool,
}

fn finalize(mut heap: ResultHeap) -> Vec<MatchResult> {
    let mut vec = Vec::new();
    while let Some(result) = heap.pop() {
        vec.push(result);
    }
    vec.reverse();
    vec
}

fn thread_worker(
    query: &str,
    query_case: &str,
    options: &score_match::MatchOptions,
    use_last_match: bool,
    max_results: usize,
    use_weights: bool,
    candidates: &mut [Candidate],
    start: usize,
    end: usize,
    result: &mut ResultHeap,
) {
    let bitmask = letter_bitmask(query_case);
    for candidate in candidates.iter_mut().take(end).skip(start) {
        if use_last_match && !candidate.last_match {
            continue;
        }
        if (bitmask & candidate.bitmask) == bitmask {
            if let Some(score) = score_match::fuzzy_score(
                &candidate.value,
                &candidate.value_lower,
                query,
                query_case,
                options,
            ) {
                let weight = if use_weights { candidate.weight } else { 0 };
                push_heap(result, weight, score, &candidate.value, max_results);
                candidate.last_match = true;
            } else {
                candidate.last_match = false;
            }
        }
    }
}

impl MatcherBase {
    pub(super) fn new() -> Self {
        MatcherBase {
            candidates: Vec::new(),
            lookup: HashMap::new(),
            last_query: String::new(),
            last_query_first_match_can_be_weak: false,
        }
    }

    pub(super) fn find_matches(
        &mut self,
        query: &str,
        options: &super::Options,
    ) -> Vec<MatchResult> {
        let mut max_results = options.max_results;
        let num_threads = options.num_threads;
        if max_results == 0 {
            max_results = usize::MAX;
        }

        let first_match_can_be_weak = options.first_match_can_be_weak;
        let use_weights = options.weighted;

        let match_options = score_match::MatchOptions {
            boost_full_match: true,
            first_match_can_be_weak,
        };

        // Ignore all whitespace in the query.
        let new_query: String = query.chars().filter(|c| !c.is_ascii_whitespace()).collect();
        if new_query.is_empty() {
            return Vec::new();
        }

        let query_case = new_query.to_ascii_lowercase();

        // If our current query is just an extension of the last query,
        // quickly ignore all previous non-matches as an optimization.
        let use_last_match = query_case.starts_with(&self.last_query)
            && first_match_can_be_weak == self.last_query_first_match_can_be_weak;
        self.last_query = query_case.clone();
        self.last_query_first_match_can_be_weak = first_match_can_be_weak;

        let mut combined: ResultHeap = ResultHeap::new();

        let candidates_len = self.candidates.len();
        if num_threads == 0 || candidates_len < 10000 {
            thread_worker(
                &new_query,
                &query_case,
                &match_options,
                use_last_match,
                max_results,
                use_weights,
                &mut self.candidates,
                0,
                candidates_len,
                &mut combined,
            );
        } else {
            let new_query_ref = new_query.as_str();
            let query_case_ref = query_case.as_str();
            let match_options_ref = &match_options;
            std::thread::scope(|s| {
                let mut handles = Vec::new();
                let mut remaining = &mut self.candidates[..];
                for i in 0..num_threads {
                    let mut chunk_size = candidates_len / num_threads;
                    if i < candidates_len % num_threads {
                        chunk_size += 1;
                    }
                    let (chunk, rest) = remaining.split_at_mut(chunk_size);
                    remaining = rest;
                    let chunk_len = chunk.len();
                    let handle = s.spawn(move || {
                        let mut thread_result: ResultHeap = ResultHeap::new();
                        thread_worker(
                            new_query_ref,
                            query_case_ref,
                            match_options_ref,
                            use_last_match,
                            max_results,
                            use_weights,
                            chunk,
                            0,
                            chunk_len,
                            &mut thread_result,
                        );
                        thread_result
                    });
                    handles.push(handle);
                }

                for handle in handles {
                    let mut thread_result = handle.join().unwrap();
                    while let Some(top) = thread_result.pop() {
                        push_heap(
                            &mut combined,
                            top.weight,
                            top.score,
                            &top.value,
                            max_results,
                        );
                    }
                }
            });
        }

        finalize(combined)
    }

    pub(super) fn add_candidate(&mut self, value: FlowSmolStr, weight: i32) {
        if self.lookup.contains_key(&value) {
            return;
        }
        let value_lower = value.to_ascii_lowercase();
        self.lookup.insert(value.dupe(), self.candidates.len());
        let bitmask = letter_bitmask(&value_lower);
        self.candidates.push(Candidate {
            value,
            value_lower,
            bitmask,
            last_match: true,
            weight,
        });
    }

    pub(super) fn remove_candidate(&mut self, value: &FlowSmolStr) {
        if let Some(&idx) = self.lookup.get(value) {
            if idx + 1 != self.candidates.len() {
                let last = self.candidates.len() - 1;
                self.candidates.swap(idx, last);
                self.lookup.insert(self.candidates[idx].value.dupe(), idx);
            }
            self.candidates.pop();
            self.lookup.remove(value);
        }
    }
}
