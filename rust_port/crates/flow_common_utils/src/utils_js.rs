/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::parse_error;
pub use parse_error::camelize;

pub fn swap<A, B>((a, b): (A, B)) -> (B, A) {
    (b, a)
}

pub fn mk_tuple<A, B>(x: A, y: B) -> (A, B) {
    (x, y)
}

pub fn mk_tuple_swapped<A, B>(x: A, y: B) -> (B, A) {
    (y, x)
}

// Useful for various places where a user might have typoed a string and the
// set of possible intended strings is known (i.e. variable names).
#[allow(clippy::needless_range_loop)]
pub fn typo_suggestion(possible_names: &[&FlowSmolStr], name: &str) -> Option<FlowSmolStr> {
    fn typo_suggestions(possible_names: &[&FlowSmolStr], name: &str) -> Vec<FlowSmolStr> {
        // Calculates the Levenshtein distance between the two strings, but with a
        // limit. See here for documentation on this algorithm:
        // https://en.wikipedia.org/wiki/Levenshtein_distance
        fn distance(a: &str, b: &str, limit: usize) -> Option<usize> {
            let alen = a.len();
            let blen = b.len();
            let limit = std::cmp::min(std::cmp::max(alen, blen), limit);
            if alen.abs_diff(blen) > limit {
                return None;
            }
            let mut matrix = vec![vec![limit + 1; blen + 1]; alen + 1];
            matrix[0][0] = 0;
            for i in 1..=std::cmp::max(alen, blen) {
                if i <= alen {
                    matrix[i][0] = i;
                }
                if i <= blen {
                    matrix[0][i] = i;
                }
            }
            let a_bytes = a.as_bytes();
            let b_bytes = b.as_bytes();
            for ai in 1..=alen {
                let bi_start = std::cmp::max(1, (ai as isize - limit as isize - 1).max(0) as usize);
                let bi_end = std::cmp::min(blen, ai + limit + 1);
                for bi in bi_start..=bi_end {
                    let prev_ai = a_bytes[ai - 1];
                    let prev_bi = b_bytes[bi - 1];
                    let cost = if prev_ai == prev_bi { 0 } else { 1 };
                    let closest = std::cmp::min(
                        std::cmp::min(
                            matrix[ai - 1][bi] + 1, // deletion
                            matrix[ai][bi - 1] + 1, // insertion
                        ),
                        matrix[ai - 1][bi - 1] + cost, // substitution
                    );
                    let closest = if ai > 1
                        && bi > 1
                        && prev_ai == b_bytes[bi - 2]
                        && a_bytes[ai - 2] == prev_bi
                    {
                        // transposition
                        std::cmp::min(matrix[ai][bi], matrix[ai - 2][bi - 2] + cost)
                    } else {
                        closest
                    };
                    matrix[ai][bi] = closest;
                }
            }
            let result = matrix[alen][blen];
            if result > limit { None } else { Some(result) }
        }

        fn fold_results(
            limit: usize,
            name: &str,
            results: (Vec<FlowSmolStr>, usize),
            poss_name: &FlowSmolStr,
        ) -> (Vec<FlowSmolStr>, usize) {
            match distance(name, poss_name, limit) {
                None => results,
                Some(dist) => {
                    let (mut curr_choice, curr_dist) = results;
                    if dist < curr_dist {
                        (vec![poss_name.dupe()], dist)
                    } else if dist == curr_dist {
                        curr_choice.push(poss_name.dupe());
                        (curr_choice, curr_dist)
                    } else {
                        (curr_choice, curr_dist)
                    }
                }
            }
        }

        let limit = match name.len() {
            1 | 2 => 0,
            3 | 4 => 1,
            5 | 6 => 2,
            _ => 3,
        };

        let (result, _) = possible_names
            .iter()
            .fold((vec![], usize::MAX), |acc, poss_name| {
                fold_results(limit, name, acc, poss_name)
            });
        result
    }

    let suggestions = typo_suggestions(possible_names, name);
    suggestions.into_iter().next()
}

pub fn in_flow_test() -> bool {
    match std::env::var("IN_FLOW_TEST").ok().as_deref() {
        Some("1") | Some("true") => true,
        Some(_) | None => false,
    }
}
