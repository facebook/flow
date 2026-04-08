/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use crate::lints::LintKind;

#[derive(Debug, Clone, Default)]
pub struct StrictModeSettings(BTreeSet<LintKind>);

impl StrictModeSettings {
    pub fn empty() -> Self {
        Self(BTreeSet::new())
    }

    pub fn fold<F, Acc>(&self, init: Acc, f: F) -> Acc
    where
        F: Fn(LintKind, Acc) -> Acc,
    {
        self.0.iter().fold(init, |acc, &kind| f(kind, acc))
    }

    pub fn iter<F>(&self, mut f: F)
    where
        F: FnMut(LintKind),
    {
        for &kind in self.0.iter() {
            f(kind);
        }
    }

    pub fn of_lines(lines: &[(u32, String)]) -> Result<Self, (u32, String)> {
        fn parse_line((line_number, line): &(u32, String)) -> Result<Vec<LintKind>, (u32, String)> {
            let line = line.trim();
            match LintKind::parse_from_str(line) {
                Some(kinds) => Ok(kinds),
                None => Err((
                    *line_number,
                    format!("Invalid strict mode lint \"{}\" encountered.", line),
                )),
            }
        }
        let mut settings = BTreeSet::new();
        for l in lines {
            let kinds = parse_line(l)?;
            settings.extend(kinds);
        }
        Ok(Self(settings))
    }
}
