/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::Cell;
use std::cell::RefCell;

use dupe::Dupe;

use crate::file_key::FileKey;
use crate::loc_sig::LocSig;

thread_local! {
    pub static DESERIALIZE_FILE_KEY: RefCell<Option<FileKey>> = const { RefCell::new(None) };
    static LOC_SERDE_FULL_SOURCE: Cell<bool> = const { Cell::new(false) };
}

/// When called, serializes/deserializes `Loc` with the full `FileKey` path
/// as an `Option<String>` instead of the default `(bool, Position, Position)`
/// tuple. This is needed for socket RPC where `FileKey` must survive
/// cross-process round-trips.
pub fn with_full_source_serde<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    LOC_SERDE_FULL_SOURCE.set(true);
    let result = f();
    LOC_SERDE_FULL_SOURCE.set(false);
    result
}

/// line numbers are 1-indexed; column numbers are 0-indexed
/// The column offset are measured by bytes
#[derive(
    Debug,
    Copy,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let k = self.line.cmp(&other.line);
        if k == std::cmp::Ordering::Equal {
            self.column.cmp(&other.column)
        } else {
            k
        }
    }
}

// start is inclusive; end is exclusive
// If you are modifying this record, go look at ALoc.ml and make sure you understand the
// representation there.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct Loc {
    pub source: Option<FileKey>,
    pub start: Position,
    pub end: Position,
}

impl serde::Serialize for Loc {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if LOC_SERDE_FULL_SOURCE.get() {
            (&self.source, &self.start, &self.end).serialize(serializer)
        } else {
            (self.source.is_some(), &self.start, &self.end).serialize(serializer)
        }
    }
}

impl<'de> serde::Deserialize<'de> for Loc {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        if LOC_SERDE_FULL_SOURCE.get() {
            let (source, start, end) =
                <(Option<FileKey>, Position, Position)>::deserialize(deserializer)?;
            Ok(Loc { source, start, end })
        } else {
            let (has_source, start, end) = <(bool, Position, Position)>::deserialize(deserializer)?;
            let source = if has_source {
                DESERIALIZE_FILE_KEY.with(|k| k.borrow().clone())
            } else {
                None
            };
            Ok(Loc { source, start, end })
        }
    }
}

pub const LOC_NONE: Loc = Loc {
    source: None,
    start: Position { line: 0, column: 0 },
    end: Position { line: 0, column: 0 },
};

impl Default for Loc {
    fn default() -> Self {
        LOC_NONE
    }
}

impl Loc {
    pub fn is_none(&self) -> bool {
        self == &LOC_NONE
    }

    pub fn is_none_ignore_source(&self) -> bool {
        self.start == LOC_NONE.start && self.end == LOC_NONE.end
    }

    pub fn between(loc1: &Self, loc2: &Self) -> Self {
        Loc {
            source: loc1.source.dupe(),
            start: loc1.start,
            end: loc2.end,
        }
    }

    /// Returns the position immediately before the start of the given loc. If the
    /// given loc is at the beginning of a line, return the position of the first
    /// char on the same line.
    pub fn char_before(&self) -> Self {
        Self {
            source: self.source.dupe(),
            start: {
                let Position { line, column } = self.start;
                let column = if column > 0 { column - 1 } else { column };
                Position { line, column }
            },
            end: self.start,
        }
    }

    /// Returns the location of the first character in the given loc. Not accurate if the
    /// first line is a newline character, but is still consistent with loc orderings.
    pub fn first_char(&self) -> Self {
        Self {
            source: self.source.dupe(),
            start: self.start,
            end: Position {
                line: self.start.line,
                column: self.start.column + 1,
            },
        }
    }

    /// - If `a` spans (completely contains) `b`, then returns 0.
    /// - If `b` starts before `a` (even if it ends inside), returns < 0.
    /// - If `b` ends after `a` (even if it starts inside), returns > 0.
    pub fn span_compare(a: &Loc, b: &Loc) -> i32 {
        match a.source.cmp(&b.source) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Greater => 1,
            std::cmp::Ordering::Equal => match a.start.cmp(&b.start) {
                std::cmp::Ordering::Less | std::cmp::Ordering::Equal => match a.end.cmp(&b.end) {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal | std::cmp::Ordering::Greater => 0,
                },
                std::cmp::Ordering::Greater => 1,
            },
        }
    }

    /// Returns true if `other` is completely contained within `self`.
    pub fn contains(&self, other: &Loc) -> bool {
        Self::span_compare(self, other) == 0
    }

    /// Returns true if `self` intersects `other` at all */
    pub fn intersects(&self, other: &Loc) -> bool {
        self.source.cmp(&other.source) == std::cmp::Ordering::Equal
            && !(self.end < other.start || self.start > other.end)
    }

    /** [lines_intersect loc1 loc2] returns true if [loc1] and [loc2] cover any part of
    the same line, even if they don't actually intersect.

    For example, if [loc1] ends and then [loc2] begins later on the same line,
    [intersects loc1 loc2] is false, but [lines_intersect loc1 loc2] is true. */
    pub fn lines_intersect(&self, other: &Loc) -> bool {
        self.source.cmp(&other.source) == std::cmp::Ordering::Equal
            && !(self.end.line < other.start.line || self.start.line > other.end.line)
    }

    pub fn compare_ignore_source(&self, other: &Loc) -> std::cmp::Ordering {
        match self.start.cmp(&other.start) {
            std::cmp::Ordering::Equal => self.end.cmp(&other.end),
            k => k,
        }
    }

    pub fn to_string_no_source(&self) -> String {
        let line = self.start.line;
        let start = self.start.column + 1;
        let end_ = self.end.column;
        if line <= 0 {
            "0:0".to_string()
        } else if line == self.end.line && start == end_ {
            format!("{}:{}", line, start)
        } else if line != self.end.line {
            format!("{}:{},{}:{}", line, start, self.end.line, end_)
        } else {
            format!("{}:{}-{}", line, start, end_)
        }
    }

    pub fn start_pos_to_string_for_vscode_loc_uri_fragment(&self) -> String {
        let line = self.start.line;
        let start = self.start.column + 1;
        let (line, start) = if line <= 0 { (0, 0) } else { (line, start) };
        format!("#L{},{}", line, start)
    }

    pub fn mk(
        source: Option<FileKey>,
        start_line: i32,
        start_column: i32,
        end_line: i32,
        end_column: i32,
    ) -> Self {
        Self {
            source,
            start: Position {
                line: start_line,
                column: start_column,
            },
            end: Position {
                line: end_line,
                column: end_column,
            },
        }
    }

    pub fn source(&self) -> Option<&FileKey> {
        self.source.as_ref()
    }

    /// Produces a zero-width Loc, where start = end */
    pub fn cursor(source: Option<FileKey>, line: i32, column: i32) -> Self {
        Self {
            source,
            start: Position { line, column },
            end: Position { line, column },
        }
    }

    pub fn start_loc(&self) -> Self {
        Self {
            source: self.source.dupe(),
            start: self.start,
            end: self.start,
        }
    }

    pub fn end_loc(&self) -> Self {
        Self {
            source: self.source.dupe(),
            start: self.end,
            end: self.end,
        }
    }

    pub fn update_source<F: FnOnce(Option<FileKey>) -> Option<FileKey>>(self, f: F) -> Loc {
        Loc {
            source: f(self.source),
            start: self.start,
            end: self.end,
        }
    }
}

impl PartialOrd for Loc {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Loc {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let k = self.source.cmp(&other.source);
        if k == std::cmp::Ordering::Equal {
            self.compare_ignore_source(other)
        } else {
            k
        }
    }
}

impl LocSig for Loc {
    fn none() -> Self {
        LOC_NONE
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.cmp(other)
    }

    fn equal(&self, other: &Self) -> bool {
        self == other
    }

    fn debug_to_string(&self, include_source: bool) -> String {
        let source = if include_source {
            format!(
                "{:?}: ",
                match &self.source {
                    Some(src) => src.as_str(),
                    None => "<NONE>",
                }
            )
        } else {
            "".to_string()
        };
        let pos = format!(
            "({}, {}) to ({}, {})",
            self.start.line, self.start.column, self.end.line, self.end.column
        );
        source + &pos
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk(
        source: Option<FileKey>,
        start_line: i32,
        start_column: i32,
        end_line: i32,
        end_column: i32,
    ) -> Loc {
        let start = Position {
            line: start_line,
            column: start_column,
        };
        let end = Position {
            line: end_line,
            column: end_column,
        };
        Loc { source, start, end }
    }

    #[test]
    fn locs_equal() {
        let loc1 = mk(None, 1, 0, 1, 5);
        let loc2 = mk(None, 1, 0, 1, 5);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_before_loc2_same_line() {
        let loc1 = mk(None, 1, 0, 1, 2);
        let loc2 = mk(None, 1, 3, 1, 5);
        assert!(!loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_before_loc2_diff_line() {
        let loc1 = mk(None, 1, 0, 1, 2);
        let loc2 = mk(None, 2, 0, 2, 2);
        assert!(!loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_after_loc2_same_line() {
        let loc1 = mk(None, 1, 3, 1, 5);
        let loc2 = mk(None, 1, 0, 1, 2);
        assert!(!loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_after_loc2_diff_line() {
        let loc1 = mk(None, 2, 0, 2, 2);
        let loc2 = mk(None, 1, 0, 1, 2);
        assert!(!loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_in_loc2() {
        let loc1 = mk(None, 1, 1, 1, 3);
        let loc2 = mk(None, 1, 0, 1, 5);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_in_loc1() {
        let loc1 = mk(None, 1, 0, 1, 5);
        let loc2 = mk(None, 1, 1, 1, 3);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_intersects_end_of_loc1() {
        let loc1 = mk(None, 1, 0, 1, 5);
        let loc2 = mk(None, 1, 3, 1, 7);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_intersects_start_of_loc1() {
        let loc1 = mk(None, 1, 3, 1, 7);
        let loc2 = mk(None, 1, 0, 1, 5);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_intersects_end_of_loc1_multiline() {
        let loc1 = mk(None, 1, 0, 3, 0);
        let loc2 = mk(None, 2, 0, 4, 0);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_intersects_start_of_loc1_multiline() {
        let loc1 = mk(None, 2, 0, 4, 0);
        let loc2 = mk(None, 1, 0, 3, 0);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_extends_past_loc2() {
        let loc1 = mk(None, 1, 0, 2, 0);
        let loc2 = mk(None, 1, 0, 1, 3);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_extends_past_loc1() {
        let loc1 = mk(None, 1, 0, 1, 3);
        let loc2 = mk(None, 1, 0, 2, 0);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc1_starts_before_loc2() {
        let loc1 = mk(None, 1, 0, 2, 0);
        let loc2 = mk(None, 1, 3, 2, 0);
        assert!(loc1.intersects(&loc2));
    }

    #[test]
    fn loc2_starts_before_loc1() {
        let loc1 = mk(None, 1, 3, 2, 0);
        let loc2 = mk(None, 1, 0, 2, 0);
        assert!(loc1.intersects(&loc2));
    }
}
