/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::path::Path;
use std::path::PathBuf;

use regex::Regex;

use crate::sys_utils::normalize_filename_dir_sep;

#[derive(Debug, Clone)]
pub struct PathMatcher {
    /// Stems extracted from paths
    stems: Vec<PathBuf>,
    /// Map from stems to list of (original path, regexified path)
    stem_map: BTreeMap<PathBuf, Vec<(String, Regex)>>,
}

/// Given a path, return the max prefix not containing a wildcard or a terminal filename
fn path_stem(path: &Path) -> PathBuf {
    let wc = Regex::new(r"^[^*?]*[*?]").unwrap();
    // Strip filename
    let path = if path.exists() && !path.is_dir() {
        path.parent().unwrap_or(path)
    } else {
        path
    };
    let path_str = path.to_string_lossy();
    // Strip back to non-wildcard prefix
    let stem = if let Some(m) = wc.find(&path_str) {
        let matched = m.as_str();
        Path::new(matched)
            .parent()
            .unwrap_or(Path::new(""))
            .to_string_lossy()
            .into_owned()
    } else {
        path_str.into_owned()
    };

    PathBuf::from(stem)
}

/// Translate a path with wildcards into a regex
fn path_patt(path: &Path) -> Regex {
    let path_str = path.to_string_lossy();
    let str = normalize_filename_dir_sep(&path_str);
    let mut regex_string = String::new();
    // Because we accept both * and **, convert in 2 steps
    for (i, s) in str.split("**").enumerate() {
        if i > 0 {
            // For subsequent parts, we had a "**" delimiter before this text
            regex_string.push_str(".*");
        }
        // note: unix directory separators specifiers only. Windows directory
        // separators will already have been normalized to unix directory
        // separators
        let s = s.replace('*', "[^/]*");
        // Replace ? with . (matches any single character)
        let s = s.replace('?', ".");
        regex_string.push_str(&s);
    }
    Regex::new(&regex_string).unwrap()
}

/// Helper - eliminate noncanonical entries where possible.
/// No other normalization is done
fn fixup_path(path: &Path) -> PathBuf {
    let s = path.to_string_lossy();
    match path.canonicalize() {
        Ok(canonical) => {
            if canonical.to_string_lossy() == s {
                return path.to_path_buf();
            }
        }
        Err(_) => {}
    }
    let (root, entries): (String, Vec<&str>) = {
        let entries: Vec<&str> = if cfg!(windows) {
            s.split(['/', '\\']).collect()
        } else {
            s.split(std::path::MAIN_SEPARATOR).collect()
        };
        match entries.as_slice() {
            ["", rest @ ..] if !cfg!(windows) => {
                // "/foo" -> ("/", ["foo"])
                (std::path::MAIN_SEPARATOR.to_string(), rest.to_vec())
            }
            [root, rest @ ..] if cfg!(windows) && root.len() == 2 && root.as_bytes()[1] == b':' => {
                // "C:\\foo" -> ("C:\\", ["foo"])
                (
                    format!("{}{}", root, std::path::MAIN_SEPARATOR),
                    rest.to_vec(),
                )
            }
            // Relative path
            _ => ("".to_string(), entries),
        }
    };
    let mut processed_entries = Vec::new();
    for entry in entries {
        if entry == "." {
            continue;
        }
        if entry == ".." {
            if processed_entries.pop().is_none() {
                processed_entries.push(entry);
            }
        } else {
            processed_entries.push(entry);
        }
    }

    let mut result = Path::new(&root).to_path_buf();
    for entry in processed_entries {
        result = result.join(entry);
    }
    result
}

/// Filter a list of prefixes to only those with which f starts
fn find_prefixes<'a>(stems: &'a [PathBuf], f: &str) -> Vec<&'a PathBuf> {
    stems
        .iter()
        .filter(|prefix| {
            let prefix_str = prefix.to_string_lossy();
            f.starts_with(prefix_str.as_ref())
        })
        .collect()
}

/// Find a match for f in a list of patterns
fn match_patt<'a>(f: &str, patterns: &'a [(String, Regex)]) -> Option<&'a str> {
    for (path, patt) in patterns {
        if patt.is_match(f) {
            return Some(path);
        }
    }
    None
}

impl PathMatcher {
    pub fn empty() -> Self {
        Self {
            stems: Vec::new(),
            stem_map: BTreeMap::new(),
        }
    }

    pub fn stems(&self) -> &[PathBuf] {
        &self.stems
    }

    /// Add a path to the matcher, calculating the appropriate stem and pattern
    pub fn add_path(&mut self, path: &Path) {
        let path = fixup_path(path);
        let stem = path_stem(&path);
        let patt = path_patt(&path);
        let pstr = path.to_string_lossy().into_owned();

        if let Some(entries) = self.stem_map.get_mut(&stem) {
            entries.push((pstr, patt));
        } else {
            self.stem_map.insert(stem.clone(), vec![(pstr, patt)]);
            self.stems.push(stem);
        }
    }

    /// Check if a file path matches any pattern in the matcher
    pub fn matches(&self, f: &str) -> bool {
        let matching_stems = find_prefixes(&self.stems, f);
        let normalized_f = normalize_filename_dir_sep(f);

        matching_stems.iter().any(|stem| {
            if let Some(patts) = self.stem_map.get(*stem) {
                match_patt(&normalized_f, patts).is_some()
            } else {
                false
            }
        })
    }
}
