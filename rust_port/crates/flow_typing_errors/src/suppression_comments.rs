/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
   Suppression comments have the following syntax:

   <SUPPRESSOR> := $FlowIssue | $FlowFixMe | $FlowExpectedError | $FlowIgnore

   //<SUPPRESSOR>[CODE]...
*/

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::loc::Loc;

// Locs are just metadata here, should not affect behavior
#[derive(Debug, Clone, Default)]
pub struct CodeSet(BTreeMap<FlowSmolStr, Loc>);

impl CodeSet {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn singleton(code: String, loc: Loc) -> Self {
        let mut map = BTreeMap::new();
        map.insert(FlowSmolStr::new(code), loc);
        Self(map)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, code: String, loc: Loc) {
        self.0.insert(FlowSmolStr::new(code), loc);
    }

    pub fn union(mut self, other: Self) -> Self {
        for (k, v) in other.0 {
            self.0.entry(k).or_insert(v);
        }
        self
    }

    pub fn subset(&self, other: &Self) -> bool {
        self.0.keys().all(|k| other.0.contains_key(k))
    }

    pub fn diff(&self, other: &Self) -> Self {
        Self(
            self.0
                .iter()
                .filter_map(|(k, v)| {
                    if other.0.contains_key(k) {
                        None
                    } else {
                        Some((k.dupe(), v.dupe()))
                    }
                })
                .collect(),
        )
    }

    pub fn mem(&self, code: &str) -> bool {
        self.0.contains_key(code)
    }

    pub fn elements(&self) -> impl Iterator<Item = (&str, &Loc)> {
        self.0.iter().map(|(k, v)| (k.as_ref(), v))
    }

    pub fn from(items: impl IntoIterator<Item = (FlowSmolStr, Loc)>) -> Self {
        Self(items.into_iter().collect())
    }
}

#[derive(Debug, Clone)]
pub struct CodeMap<V>(BTreeMap<FlowSmolStr, (Loc, V)>);

impl<V> CodeMap<V> {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn singleton(code: FlowSmolStr, loc: Loc, value: V) -> Self {
        let mut map = BTreeMap::new();
        map.insert(code, (loc, value));
        Self(map)
    }

    pub fn for_all<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&str, &V) -> bool,
    {
        self.0.iter().all(|(k, (_, v))| f(k.as_ref(), v))
    }

    pub fn keys(&self) -> impl Iterator<Item = (&str, &Loc)> {
        self.0.iter().map(|(k, (loc, _))| (k.as_ref(), loc))
    }
}

impl<V> Default for CodeMap<V> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum ApplicableCodes {
    Specific(CodeSet),
    All {
        locs: BTreeSet<Loc>,
        warn_unused: bool,
    },
}

impl ApplicableCodes {
    pub fn locs(&self) -> Vec<Loc> {
        match self {
            ApplicableCodes::Specific(codes) => {
                codes.elements().map(|(_, loc)| loc.dupe()).collect()
            }
            ApplicableCodes::All {
                locs,
                warn_unused: true,
            } => locs.iter().cloned().collect(),
            ApplicableCodes::All {
                warn_unused: false, ..
            } => Vec::new(),
        }
    }

    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (ApplicableCodes::Specific(c1), ApplicableCodes::Specific(c2)) => {
                ApplicableCodes::Specific(c1.union(c2))
            }
            (
                ApplicableCodes::All {
                    locs: l1,
                    warn_unused: w1,
                },
                ApplicableCodes::All {
                    locs: l2,
                    warn_unused: w2,
                },
            ) => ApplicableCodes::All {
                locs: l1.union(&l2).cloned().collect(),
                warn_unused: w1 && w2,
            },
            (c1 @ ApplicableCodes::All { .. }, ApplicableCodes::Specific(_)) => c1,
            (ApplicableCodes::Specific(_), c2 @ ApplicableCodes::All { .. }) => c2,
        }
    }
}

fn consume_token<'a>(token: &str, s: &'a str) -> (&'a str, bool) {
    if let Some(rest) = s.strip_prefix(token) {
        (rest, true)
    } else {
        (s, false)
    }
}

// Consumes any element of [tokens] from the front of [str].
// If there was no match at all, returns [x, chopped], where
// [x] is [str] with all elements of [tokens] removed from the
// front, and [chopped] is whether or not any tokens were removed
fn consume_tokens<'a>(tokens: &[&str], s: &'a str) -> (&'a str, bool) {
    fn inner<'a>(tokens: &[&str], s: &'a str, overall_chopped: bool) -> (&'a str, bool) {
        let mut current = s;
        let mut any_chopped_this_round = false;

        for &token in tokens {
            let (next, chopped) = consume_token(token, current);
            if chopped {
                current = next;
                any_chopped_this_round = true;
            }
        }

        if !any_chopped_this_round {
            (s, overall_chopped)
        } else {
            inner(tokens, current, true)
        }
    }

    inner(tokens, s, false)
}

fn is_valid_code_char(c: char) -> bool {
    let ascii = c as u32;
    /* lowercase letters*/
    ascii == 45 || (ascii >= 97 && ascii <= 122)
}

#[derive(Debug, Clone, Copy)]
pub enum BadSuppressionKind {
    MissingCode,
    MalformedCode,
}

// After matching a TypeScript directive prefix, ensure the next character is
// not part of the directive identifier — otherwise the prefix isn't really a
// directive (e.g. `@ts-ignoree`). Anything that can't continue a `[a-zA-Z0-9_-]`
// identifier is a valid boundary, which permits the common
// `// @ts-ignore: reason` / `// @ts-expect-error: reason` description form.
fn ts_directive_boundary(remainder: &str) -> bool {
    match remainder.chars().next() {
        None => true,
        Some(c) => !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'),
    }
}

fn try_ts_directive(directive: &str, comment: &str) -> bool {
    let (rest, matched) = consume_token(directive, comment);
    matched && ts_directive_boundary(rest)
}

pub fn should_suppress(
    is_ts_file: bool,
    comment: &str,
    loc: &Loc,
) -> Result<Option<ApplicableCodes>, BadSuppressionKind> {
    let (comment, _) = consume_tokens(&[" ", "\n", "\t", "\r", "*"], comment);
    let ts_match = if is_ts_file {
        if try_ts_directive("@ts-expect-error", comment) {
            Some(ApplicableCodes::All {
                locs: BTreeSet::from([loc.dupe()]),
                warn_unused: true,
            })
        } else if try_ts_directive("@ts-ignore", comment) {
            Some(ApplicableCodes::All {
                locs: BTreeSet::from([loc.dupe()]),
                warn_unused: false,
            })
        } else {
            None
        }
    } else {
        None
    };
    match ts_match {
        Some(codes) => Ok(Some(codes)),
        None => {
            let (comment, is_suppressor) =
                consume_tokens(&["$FlowFixMe", "$FlowExpectedError"], comment);
            if !is_suppressor {
                Ok(None)
            } else {
                let (comment, has_preceding_spaces) =
                    consume_tokens(&[" ", "\n", "\t", "\r"], comment);
                let (comment, has_code) = consume_token("[", comment);
                if !has_code {
                    Err(BadSuppressionKind::MissingCode)
                } else {
                    match comment.find(']') {
                        None => Err(BadSuppressionKind::MalformedCode), /* Not a code if the bracket is not terminated */
                        Some(0) => Err(BadSuppressionKind::MalformedCode), /* $FlowFixMe[] is not a real code */
                        Some(index) => {
                            /* //$FlowFixMe [code] is invalid */
                            if has_preceding_spaces {
                                Err(BadSuppressionKind::MalformedCode)
                            } else {
                                let code = &comment[..index];
                                if code.chars().all(is_valid_code_char) {
                                    Ok(Some(ApplicableCodes::Specific(CodeSet::singleton(
                                        code.to_string(),
                                        loc.dupe(),
                                    ))))
                                } else {
                                    Err(BadSuppressionKind::MalformedCode)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
