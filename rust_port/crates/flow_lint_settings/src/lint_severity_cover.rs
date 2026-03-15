/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * An ExactCover.t represents an exact cover over 1 file.
 *
 * See https://en.wikipedia.org/wiki/Exact_cover; the short version is that an
 * ExactCover.t represents a set of adjacent, nonoverlapping, nonempty,
 * exhaustive ranges over 0 or more files.
 *
 * ExactCover.t is a read-only structure for efficient querying.
 *
 * ExactCover.builder is a write-only structure for efficient construction
 * (under some assuptions) that can be baked into an ExactCover.t.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;

use crate::lint_settings::LintParseError;
use crate::lint_settings::LintSettings;
use crate::lints::LintKind;
use crate::severity::Severity;

// A builder is a sorted list of non-overlapping, non-empty, adjacent ranges,
// with later ranges appearing at the head of the list and earlier ranges
// appearing at the tail of the list.
#[derive(Debug, Clone)]
pub struct Builder {
    next_entry_id: u32,
    entries: Vec<(u32, (Loc, LintSettings<Severity>))>,
}

impl Builder {
    pub fn new(source: FileKey, value: LintSettings<Severity>) -> Self {
        let full_range = Loc::mk(Some(source), 0, 0, i32::MAX / 2, i32::MAX / 2);
        Builder {
            next_entry_id: 1,
            entries: vec![(0, (full_range, value))],
        }
    }

    // Gets all ranges that intersect with the provided range.
    fn get_intersecting_entries(&self, query: &Loc) -> Vec<(u32, (Loc, LintSettings<Severity>))> {
        let mut acc = Vec::new();
        for (entry_id, candidate) in self.entries.iter() {
            if query.end <= candidate.0.start {
                // We still haven't reached the first intersecting range.
                continue;
            } else if candidate.0.end > query.start {
                // The current range is intersecting.
                acc.push((*entry_id, candidate.clone()));
            } else {
                // We've passed the last intersecting range.
                break;
            }
        }
        acc
    }

    // Adds the provided entry to the builder. (Assumes that there is no range in
    // the builder that overlaps with the provided entry.)
    fn add(&mut self, entry: (Loc, LintSettings<Severity>)) {
        // Find the insertion point where entry > head
        let mut insertion_idx = self.entries.len();
        for (i, (_, head)) in self.entries.iter().enumerate() {
            if entry.0 > head.0 {
                insertion_idx = i;
                break;
            }
        }
        let id = self.next_entry_id;
        self.next_entry_id += 1;
        self.entries.insert(insertion_idx, (id, entry));
    }

    // Removes the provided entry from the builder. (Assumes that the provided entry
    // exists in the builder.)
    fn remove(&mut self, entry_id: u32) {
        let pos = self
            .entries
            .iter()
            .position(|(old_entry_id, _)| entry_id == *old_entry_id)
            .expect("Remove is only called on entries that exist in the builder.");
        self.entries.remove(pos);
    }

    // Given a range over which to perform a modification, a modification function,
    // and a builder to work on, returns a builder modified by the modification
    // function over the provided range.
    fn update_range<F>(&mut self, new_range: &Loc, map_fun: F)
    where
        F: Fn(&LintSettings<Severity>) -> LintSettings<Severity>,
    {
        let original_intersecting_entries = self.get_intersecting_entries(new_range);
        for (original_entry_id, (old_range, old_value)) in original_intersecting_entries {
            let (overlap, remaining_ranges) = get_overlap(&old_range, new_range);
            let new_overlap = (overlap, map_fun(&old_value));
            self.remove(original_entry_id);
            self.add(new_overlap);
            for loc in remaining_ranges {
                self.add((loc, old_value.clone()));
            }
        }
    }

    /// Change the settings in the provided range by adding the provided settings list.
    /// In the settings list, the kind is the type of lint, the value is the value to set to,
    /// and the location is the position of the setting in the source code.
    pub fn update_settings(&mut self, range: &Loc, setting_list: Vec<(LintKind, (Severity, Loc))>) {
        let map_fun = |old_settings: &LintSettings<Severity>| {
            let mut new_settings = old_settings.clone();
            let entries: Vec<(LintKind, (Severity, Option<Loc>))> = setting_list
                .iter()
                .map(|(kind, (state, loc))| (*kind, (*state, Some(loc.clone()))))
                .collect();
            new_settings.set_all(entries);
            new_settings
        };

        self.update_range(range, map_fun);
    }

    /// Works similarly to update_settings, but takes two additional parameters: a running
    /// LintSettings object and an error handling function. The LintSettings object is updated with
    /// the new lint settings (in addition to the builder being updated), and if any redundant
    /// settings are encountered, the error function is called with an error message and the
    /// location of the error. Additionally, takes the lint settings in unflattened form so
    /// that errors can be properly reported.
    ///
    /// This function only checks for settings that are redundant because they don't change
    /// anything. It doesn't check for settings that are redundant because they are
    /// immediately overwritten. (That's done elsewhere.)  
    pub fn update_settings_and_running(
        &mut self,
        in_libdef: bool,
        mut running_settings: LintSettings<Severity>,
        mut parse_error_handler: impl FnMut((Loc, LintParseError)),
        range: &Loc,
        settings_list_list: Vec<Vec<(LintKind, (Severity, Loc))>>,
    ) -> LintSettings<Severity> {
        self.update_settings(
            range,
            settings_list_list.iter().flatten().cloned().collect(),
        );

        // Update running_settings and check for redundancy
        for settings_list in settings_list_list {
            if let Some((_, (_, loc))) = settings_list.first() {
                let loc = loc.dupe();
                let mut all_redundant = true;
                for (kind, (state, loc)) in settings_list {
                    let this_redundant = running_settings.get_value(kind) == &state;
                    running_settings.set_value(kind, (state, Some(loc)));
                    all_redundant = all_redundant && this_redundant;
                }
                if !in_libdef && all_redundant {
                    parse_error_handler((loc, LintParseError::RedundantArgument));
                }
            }
        }

        running_settings
    }

    pub fn bake(self) -> LintSeverityCover {
        let mut map = BTreeMap::new();
        for (_, (loc, value)) in self.entries {
            map.insert(WrappedLoc(loc), value);
        }
        LintSeverityCover(map)
    }
}

// Takes two overlapping ranges, old_range and new_range, and returns a tuple
// (intersection, remaining), where intersection is the intersection of
// old_range and new_range, and remaining is a list of 0-2 ranges that, when
// unioned with intersection, form old_range.
fn get_overlap(old_range: &Loc, new_range: &Loc) -> (Loc, Vec<Loc>) {
    let source = new_range.source.dupe();
    let (start, mut remaining) = if old_range.start < new_range.start {
        (
            new_range.start,
            vec![Loc {
                source: source.dupe(),
                start: old_range.start,
                end: new_range.start,
            }],
        )
    } else {
        (old_range.start, Vec::new())
    };
    let end = if old_range.end > new_range.end {
        remaining.push(Loc {
            source: source.dupe(),
            start: new_range.end,
            end: old_range.end,
        });
        new_range.end
    } else {
        old_range.end
    };
    let intersection = Loc { source, start, end };
    (intersection, remaining)
}

#[derive(Debug, Clone, Dupe)]
struct WrappedLoc(Loc);

impl PartialEq for WrappedLoc {
    fn eq(&self, other: &Self) -> bool {
        Loc::span_compare(&other.0, &self.0) == 0
    }
}

impl Eq for WrappedLoc {}

impl PartialOrd for WrappedLoc {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WrappedLoc {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Loc::span_compare(&other.0, &self.0).cmp(&0)
    }
}

// Supports O(log(n)) queries to get the value associated with a loc.
#[derive(Clone)]
pub struct LintSeverityCover(BTreeMap<WrappedLoc, LintSettings<Severity>>);

impl LintSeverityCover {
    // Gets the value associated with a certain location in the code. To resolve
    // ambiguity, this looks at the location of the first character in the provided
    // location. Errors if queried for a file not contained in this cover.
    pub fn find(&self, loc: &Loc) -> &LintSettings<Severity> {
        if let Some(v) = self.find_opt(loc) {
            return v;
        }
        panic!("Uncovered: {}", loc.debug_to_string(true));
    }

    pub fn find_opt(&self, loc: &Loc) -> Option<&LintSettings<Severity>> {
        let first_char = loc.first_char();
        self.0.get(&WrappedLoc(first_char))
    }

    // Gets the severity of the provided lint kind at the provided location. Errors
    // if queried for a file not contained in this cover.
    pub fn get_severity(&self, lint_kind: LintKind, loc: &Loc) -> Severity {
        *self.find(loc).get_value(lint_kind)
    }

    // True iff the severity for the provided lint kind has been explicitly set at
    // the provided location. Errors if queried for a file not contained in this
    // cover.
    pub fn is_explicit(&self, lint_kind: LintKind, loc: &Loc) -> bool {
        self.find(loc).is_explicit(lint_kind)
    }

    // Intended for debugging purposes.
    pub fn to_debug_string(&self) -> String {
        let mut acc = String::new();
        for (WrappedLoc(loc), settings) in &self.0 {
            acc.push_str(&format!(
                "{}: {}\n",
                loc.debug_to_string(true),
                settings.to_debug_string()
            ));
        }
        // Strip the trailing newline.
        if acc.ends_with('\n') {
            acc.pop();
        }
        acc
    }
}
