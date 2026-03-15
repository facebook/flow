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

#[derive(Debug, Clone, Default)]
pub struct ApplicableCodes(pub CodeSet);

impl ApplicableCodes {
    pub fn locs(&self) -> Vec<Loc> {
        self.0.elements().map(|(_, loc)| loc.dupe()).collect()
    }

    pub fn join(self, other: Self) -> Self {
        Self(self.0.union(other.0))
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

pub fn should_suppress(
    comment: &str,
    loc: &Loc,
) -> Result<Option<ApplicableCodes>, BadSuppressionKind> {
    let (comment, _) = consume_tokens(&[" ", "\n", "\t", "\r", "*"], comment);

    let (comment, is_suppressor) = consume_tokens(&["$FlowFixMe", "$FlowExpectedError"], comment);

    if !is_suppressor {
        return Ok(None);
    }

    let (comment, has_preceding_spaces) = consume_tokens(&[" ", "\n", "\t", "\r"], comment);

    let (comment, has_code) = consume_token("[", comment);

    if !has_code {
        return Err(BadSuppressionKind::MissingCode);
    }

    let closing_bracket_index = match comment.find(']') {
        None => return Err(BadSuppressionKind::MalformedCode), /* Not a code if the bracket is not terminated */
        Some(0) => return Err(BadSuppressionKind::MalformedCode), /* $FlowFixMe[] is not a real code */
        Some(index) => index,
    };

    /* //$FlowFixMe [code] is invalid */
    if has_preceding_spaces {
        return Err(BadSuppressionKind::MalformedCode);
    }

    let code = &comment[..closing_bracket_index];

    if code.chars().all(is_valid_code_char) {
        Ok(Some(ApplicableCodes(CodeSet::singleton(
            code.to_string(),
            loc.dupe(),
        ))))
    } else {
        Err(BadSuppressionKind::MalformedCode)
    }
}
