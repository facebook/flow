/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const MAX_LEN: usize = 128;

#[derive(Clone, Copy, PartialEq)]
enum Arrow {
    LeftLeft,
    Left,
    Diag,
}

const MIN_SCORE: i64 = -9007199254740991;

const MAX_SAFE_INTEGER: i64 = 9007199254740991;

// Convenience structure for passing around during recursion.
struct MatchInfo<'a> {
    haystack: &'a [u8],
    haystack_case: &'a [u8],
    needle: &'a [u8],
    needle_case: &'a [u8],
    boost_full_match: bool,
    first_match_can_be_weak: bool,
}

pub(super) struct MatchOptions {
    pub(super) boost_full_match: bool,
    /// If false, the first character of the needle must be a "strong" match:
    /// it must be the first character of the haystack, or immediately following
    /// a word boundary.  
    pub(super) first_match_can_be_weak: bool,
}

fn is_separator_at_pos(value: &[u8], index: usize) -> bool {
    if index >= value.len() {
        return false;
    }
    matches!(
        value[index],
        b'_' | b'-'
            | b'.'
            | b' '
            | b'/'
            | b'\\'
            | b'\''
            | b'"'
            | b':'
            | b'$'
            | b'<'
            | b'>'
            | b'('
            | b')'
            | b'['
            | b']'
            | b'{'
            | b'}'
    )
}

fn is_whitespace_at_pos(value: &[u8], index: usize) -> bool {
    if index >= value.len() {
        return false;
    }
    value[index] == b' ' || value[index] == b'\t'
}

fn is_uppercase_at_pos(pos: usize, word: &[u8], word_lower: &[u8]) -> bool {
    word[pos] != word_lower[pos]
}

// // needle is the pattern being searched for
// // haystack is the word being matched against
fn do_score(
    m: &MatchInfo,
    haystack_idx: usize,
    needle_idx: usize,
    new_match_start: bool,
    has_strong_first_match: &mut bool,
) -> i64 {
    let word = m.haystack;
    let word_lower = m.haystack_case;
    let word_len = m.haystack.len();
    let word_pos = haystack_idx;

    let pattern = m.needle;
    let pattern_lower = m.needle_case;
    let pattern_pos = needle_idx;

    if pattern_lower[pattern_pos] != word_lower[word_pos] {
        return MIN_SCORE;
    }

    let score: i64;
    let is_gap_location: bool;

    if word_pos == pattern_pos {
        // common prefix: `foobar <-> foobaz`
        //                            ^^^^^
        score = if pattern[pattern_pos] == word[word_pos] {
            7
        } else {
            5
        };
        is_gap_location = false;
    } else if is_uppercase_at_pos(word_pos, word, word_lower)
        && (word_pos == 0 || !is_uppercase_at_pos(word_pos - 1, word, word_lower))
    {
        // hitting upper-case: `foo <-> forOthers`
        //                              ^^ ^
        score = if pattern[pattern_pos] == word[word_pos] {
            7
        } else {
            5
        };
        is_gap_location = true;
    } else if is_separator_at_pos(word, word_pos)
        && (word_pos == 0 || !is_separator_at_pos(word, word_pos - 1))
    {
        // hitting a separator: `. <-> foo.bar`
        //                                ^
        score = 5;
        is_gap_location = false;
    } else if word_pos > 0
        && (is_separator_at_pos(word, word_pos - 1) || is_whitespace_at_pos(word, word_pos - 1))
    {
        // post separator: `foo <-> bar_foo`
        //                              ^^^
        score = 5;
        is_gap_location = true;
    } else {
        score = 1;
        is_gap_location = is_uppercase_at_pos(word_pos, word, word_lower);
    }

    let mut score = score;

    if score > 1 && pattern_pos == 0 {
        *has_strong_first_match = true;
    }

    if pattern_pos == 0 {
        // first character in pattern
        if word_pos > 0 {
            // the first pattern character would match a word character that is not at
            // the word start so introduce a penalty to account for the gap preceding
            // this match
            score -= if is_gap_location { 3 } else { 5 };
        }
    } else if new_match_start {
        // this would be the beginning of a new match (i.e. there would be a gap
        // before this location)
        score += if is_gap_location { 2 } else { 0 };
    } else {
        // this is part of a contiguous match, so give it a slight bonus, but do so
        // only if it would not be a preferred gap location
        score += if is_gap_location { 0 } else { 1 };
    }

    if word_pos + 1 == word_len {
        // we always penalize gaps, but this gives unfair advantages to a match that
        // would match the last character in the word so pretend there is a gap
        // after the last character in the word to normalize things
        score -= if is_gap_location { 3 } else { 5 };
    }

    score
}

fn is_pattern_in_word(
    pattern_low: &[u8],
    mut pattern_pos: usize,
    pattern_len: usize,
    word_low: &[u8],
    mut word_pos: usize,
    word_len: usize,
    min_word_match_pos: &mut [usize],
) -> bool {
    while pattern_pos < pattern_len && word_pos < word_len {
        if pattern_low[pattern_pos] == word_low[word_pos] {
            // Remember the min word position for each pattern position
            min_word_match_pos[pattern_pos] = word_pos;
            pattern_pos += 1;
        }
        word_pos += 1;
    }
    pattern_pos == pattern_len // pattern must be exhausted
}

fn fill_in_max_word_match_pos(
    pattern_len: usize,
    word_len: usize,
    pattern_low: &[u8],
    word_low: &[u8],
    max_word_match_pos: &mut [usize],
) {
    let mut pattern_pos = pattern_len - 1;
    let mut word_pos = word_len - 1;
    loop {
        if pattern_low[pattern_pos] == word_low[word_pos] {
            max_word_match_pos[pattern_pos] = word_pos;
            if pattern_pos == 0 {
                break;
            }
            pattern_pos -= 1;
        }
        if word_pos == 0 {
            break;
        }
        word_pos -= 1;
    }
}

fn do_fuzzy_score(m: &MatchInfo) -> Option<i64> {
    let first_match_can_be_weak = m.first_match_can_be_weak;

    let pattern_low = m.needle_case;
    let word = m.haystack;
    let word_low = m.haystack_case;

    let pattern_len = m.needle.len().min(MAX_LEN);
    let word_len = m.haystack.len().min(MAX_LEN);

    if pattern_len == 0 || word_len == 0 || pattern_len > word_len {
        return None;
    }

    let mut min_word_match_pos = [0usize; 2 * MAX_LEN];
    let mut max_word_match_pos = [0usize; 2 * MAX_LEN];

    // Run a simple check if the characters of pattern occur
    // (in order) at all in word. If that isn't the case we
    // stop because no match will be possible
    if !is_pattern_in_word(
        m.needle_case,
        0,
        pattern_len,
        m.haystack_case,
        0,
        word_len,
        &mut min_word_match_pos,
    ) {
        return None;
    }

    // Find the max matching word position for each pattern position
    // NOTE: the min matching word position was filled in above, in the `isPatternInWord` call
    fill_in_max_word_match_pos(
        pattern_len,
        word_len,
        m.needle_case,
        m.haystack_case,
        &mut max_word_match_pos,
    );

    // row 0 and column 0 are base cases, so these tables must be MAX_LEN + 1
    let mut arrows = [[Arrow::Diag; MAX_LEN + 1]; MAX_LEN + 1];
    let mut diag = [[0i64; MAX_LEN + 1]; MAX_LEN + 1];
    let mut table = [[0i64; MAX_LEN + 1]; MAX_LEN + 1];

    let mut has_strong_first_match = false;

    // There will be a match, fill in tables
    for pattern_pos in 0..pattern_len {
        let row = pattern_pos + 1;

        let min_wmp = min_word_match_pos[pattern_pos];
        let max_wmp = max_word_match_pos[pattern_pos];
        let next_max_wmp = if pattern_pos + 1 < pattern_len {
            max_word_match_pos[pattern_pos + 1]
        } else {
            word_len
        };

        let mut word_pos = min_wmp;
        while word_pos < next_max_wmp {
            let column = word_pos + 1;

            let mut score = MIN_SCORE;
            let mut can_come_diag = false;

            if word_pos <= max_wmp {
                score = do_score(
                    m,
                    word_pos,
                    pattern_pos,
                    diag[row - 1][column - 1] == 0,
                    &mut has_strong_first_match,
                );
            }

            let mut diag_score: i64 = 0;
            if score != MAX_SAFE_INTEGER {
                can_come_diag = true;
                diag_score = score + table[row - 1][column - 1];
            }

            let can_come_left = word_pos > min_wmp;
            let left_score = if can_come_left {
                table[row][column - 1] + if diag[row][column - 1] > 0 { -5 } else { 0 } // penalty for a gap start
            } else {
                0
            };

            let can_come_left_left = word_pos > min_wmp + 1 && diag[row][column - 1] > 0;
            let left_left_score = if can_come_left_left {
                table[row][column - 2] + if diag[row][column - 2] > 0 { -5 } else { 0 } // penalty for a gap start
            } else {
                0
            };

            if can_come_left_left
                && (!can_come_left || left_left_score >= left_score)
                && (!can_come_diag || left_left_score >= diag_score)
            {
                // always prefer choosing left left to jump over a diagonal because that means a match is earlier in the word
                table[row][column] = left_left_score;
                arrows[row][column] = Arrow::LeftLeft;
                diag[row][column] = 0;
            } else if can_come_left && (!can_come_diag || left_score >= diag_score) {
                // always prefer choosing left since that means a match is earlier in the word
                table[row][column] = left_score;
                arrows[row][column] = Arrow::Left;
                diag[row][column] = 0;
            } else if can_come_diag {
                table[row][column] = diag_score;
                arrows[row][column] = Arrow::Diag;
                diag[row][column] = diag[row - 1][column - 1] + 1;
            } else {
                // not possible
            }

            word_pos += 1;
        }
    }

    if !has_strong_first_match && !first_match_can_be_weak {
        return None;
    }

    let mut row = pattern_len;
    let mut column = word_len;
    let mut result = table[row][column];

    let mut backwards_diag_length: i32 = 0;
    let mut max_match_column: usize = 0;

    while row >= 1 {
        // Find the column where we go diagonally up
        let mut diag_column = column;
        loop {
            let arrow = arrows[row][diag_column];
            if arrow == Arrow::LeftLeft {
                diag_column -= 2;
            } else if arrow == Arrow::Left {
                diag_column -= 1;
            } else {
                // found the diagonal
                break;
            }
            if diag_column < 1 {
                break;
            }
        }

        // Overturn the "forwards" decision if keeping the "backwards" diagonal would give a better match
        if backwards_diag_length > 1 // only if we would have a contiguous match of 3 characters
            && pattern_low[row - 1] == word_low[column - 1] // only if we can do a contiguous match diagonally
            && !is_uppercase_at_pos(diag_column - 1, word, word_low) // only if the forwards chose diagonal is not an uppercase
            && (backwards_diag_length + 1) as i64 > diag[row][diag_column]
        // only if our contiguous match would be longer than the "forwards" contiguous match
        {
            diag_column = column;
        }

        if diag_column == column {
            // this is a contiguous match
            backwards_diag_length += 1;
        } else {
            backwards_diag_length = 1;
        }

        if max_match_column == 0 {
            // remember the last matched column
            max_match_column = diag_column;
        }

        row -= 1;
        column = diag_column - 1;
    }

    if word_len == pattern_len && m.boost_full_match {
        // the word matches the pattern with all characters!
        // giving the score a total match boost (to come up ahead other words)
        result += 2;
    }

    // Add 1 penalty for each skipped character in the word
    let skipped_chars_count = max_match_column as i64 - pattern_len as i64;
    result -= skipped_chars_count;

    Some(result)
}

pub(super) fn fuzzy_score(
    haystack: &str,
    haystack_lower: &str,
    needle: &str,
    needle_lower: &str,
    options: &MatchOptions,
) -> Option<i64> {
    if needle.is_empty() {
        return Some(0);
    }

    let m = MatchInfo {
        haystack: haystack.as_bytes(),
        haystack_case: haystack_lower.as_bytes(),
        needle: needle.as_bytes(),
        needle_case: needle_lower.as_bytes(),
        boost_full_match: options.boost_full_match,
        first_match_can_be_weak: options.first_match_can_be_weak,
    };

    do_fuzzy_score(&m)
}
