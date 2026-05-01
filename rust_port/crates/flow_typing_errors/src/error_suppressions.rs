/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This is a data structure used to track what locations are being suppressed
// and which suppressions have yet to be used.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_common::files::FileOptions;
use flow_common::span_map::SpanMap;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::ErrorKind;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_lint_settings::lint_settings::LintSettings;
use flow_lint_settings::lint_severity_cover::LintSeverityCover;
use flow_lint_settings::severity::Severity;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::loc_sig::LocSig;

use crate::flow_error::ErrorSet;
use crate::intermediate_error::make_intermediate_error;
use crate::intermediate_error::to_printable_error;
use crate::intermediate_error_types::IntermediateError;
use crate::suppression_comments::ApplicableCodes;
use crate::suppression_comments::CodeMap;
use crate::suppression_comments::CodeSet;

#[derive(Debug, Clone)]
pub struct CodeLocSet(BTreeSet<(FlowSmolStr, Loc)>);

impl CodeLocSet {
    pub fn new() -> Self {
        Self(BTreeSet::new())
    }

    pub fn insert(&mut self, code: String, loc: Loc) {
        self.0.insert((FlowSmolStr::new(code), loc));
    }

    pub fn union(mut self, other: Self) -> Self {
        self.0.extend(other.0);
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &Loc)> {
        self.0.iter().map(|(k, v)| (k.as_ref(), v))
    }
}

impl Default for CodeLocSet {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct FileSuppressions {
    suppressions: SpanMap<(BTreeSet<Loc>, ApplicableCodes)>,
    lint_suppressions: BTreeSet<Loc>,
}

static EMPTY_FILE_SUPPRESSIONS: FileSuppressions = FileSuppressions::empty();

impl FileSuppressions {
    const fn empty() -> Self {
        Self {
            suppressions: SpanMap::empty(),
            lint_suppressions: BTreeSet::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.suppressions.is_empty() && self.lint_suppressions.is_empty()
    }

    fn add(&mut self, loc: Loc, codes: ApplicableCodes) {
        let suppression_loc = Loc {
            source: loc.source.dupe(),
            start: Position {
                line: loc.end.line + 1,
                column: 0,
            },
            end: Position {
                line: loc.end.line + 2,
                column: 0,
            },
        };

        let mut comment_locs = BTreeSet::new();
        comment_locs.insert(loc);

        self.suppressions.add(
            suppression_loc,
            (comment_locs, codes),
            Some(
                |(mut set1, codes1): (BTreeSet<Loc>, ApplicableCodes),
                 (set2, codes2): (BTreeSet<Loc>, ApplicableCodes)| {
                    set1.extend(set2);
                    (set1, codes1.join(codes2))
                },
            ),
        );
    }

    fn all_unused_locs(&self) -> BTreeSet<Loc> {
        let mut result: BTreeSet<Loc> = self.lint_suppressions.iter().duped().collect();
        self.suppressions.fold((), |_, _, (_, codes)| {
            for loc in codes.locs() {
                result.insert(loc);
            }
        });
        result
    }

    fn remove(&mut self, loc: &Loc, codes: &CodeSet) {
        match self.suppressions.find_opt(loc) {
            Some((locs, ApplicableCodes::Specific(existing_codes)))
                if codes.subset(existing_codes) =>
            {
                let new_codes = existing_codes.diff(codes);
                if new_codes.is_empty() {
                    self.suppressions.remove(loc);
                } else {
                    // We don't want to overwrite the original location with one that falls inside it
                    let orig_loc = self
                        .suppressions
                        .keys()
                        .find(|k| Loc::span_compare(k, loc) == 0)
                        .duped()
                        .unwrap();

                    self.suppressions.add(
                        orig_loc,
                        (locs.clone(), ApplicableCodes::Specific(new_codes)),
                        None::<fn(_, _) -> _>,
                    );
                }
            }
            Some((_, ApplicableCodes::All { .. })) => {
                // Wildcard suppressions are dropped entirely on any successful match —
                // there are no per-code remainders to preserve.
                self.suppressions.remove(loc);
            }
            None => {
                self.suppressions.remove(loc);
            }
            _ => {}
        }
    }

    fn union(&mut self, other: Self) {
        self.suppressions.union(other.suppressions);
        self.lint_suppressions.extend(other.lint_suppressions);
    }

    fn add_lint_suppression(&mut self, loc: Loc) {
        self.lint_suppressions.insert(loc);
    }

    fn remove_lint_suppression(&mut self, loc: &Loc) {
        self.lint_suppressions.remove(loc);
    }

    fn suppression_at_loc(&self, loc: &Loc) -> Option<&(BTreeSet<Loc>, ApplicableCodes)> {
        self.suppressions.find_opt(loc)
    }
}

impl Default for FileSuppressions {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug, Clone)]
pub struct ErrorSuppressions(BTreeMap<FileKey, FileSuppressions>);

impl ErrorSuppressions {
    pub fn empty() -> Self {
        Self(BTreeMap::new())
    }

    // Raises if the given loc has `source` set to `None`
    pub fn add(&mut self, loc: Loc, codes: ApplicableCodes) {
        let file = Self::file_of_loc_unsafe(&loc);
        let mut suppressions = FileSuppressions::empty();
        suppressions.add(loc, codes);
        self.0.entry(file).or_default().union(suppressions);
    }

    // Union the two collections of suppressions. If they both contain suppressions for a given file,
    // include both sets of suppressions.
    pub fn union(&mut self, other: Self) {
        for (file, suppressions) in other.0 {
            let entry = self.0.entry(file.dupe());
            entry.or_default().union(suppressions);
        }
    }

    pub fn add_lint_suppressions(&mut self, locs: BTreeSet<Loc>) {
        for loc in locs {
            let file = Self::file_of_loc_unsafe(&loc);
            self.0.entry(file).or_default().add_lint_suppression(loc);
        }
    }

    pub fn remove(&mut self, file: &FileKey) {
        self.0.remove(file);
    }

    // raises if `loc` has no filename
    fn file_of_loc_unsafe(loc: &Loc) -> FileKey {
        loc.source
            .as_ref()
            .unwrap_or_else(|| panic!("NoSource: {}", loc.debug_to_string(true)))
            .dupe()
    }

    // raises if `loc` has no filename
    #[allow(dead_code)]
    fn file_suppressions_of_loc<'a>(&'a self, loc: &Loc) -> &'a FileSuppressions {
        let file = Self::file_of_loc_unsafe(loc);
        self.0.get(&file).unwrap_or(&EMPTY_FILE_SUPPRESSIONS)
    }

    // raises if `loc` has no filename
    #[allow(dead_code)]
    fn suppression_at_loc(&self, loc: &Loc) -> Option<&(BTreeSet<Loc>, ApplicableCodes)> {
        self.file_suppressions_of_loc(loc).suppression_at_loc(loc)
    }

    // raises if `loc` has no filename
    // no-op if suppressions_map does not contain an entry for that file.
    #[allow(dead_code)]
    fn update_file_suppressions<F>(&mut self, f: F, loc: &Loc)
    where
        F: FnOnce(&mut FileSuppressions),
    {
        let file = Self::file_of_loc_unsafe(loc);
        if let Some(file_suppressions) = self.0.get_mut(&file) {
            f(file_suppressions);
        }
    }

    #[allow(dead_code)]
    fn remove_suppression_from_map(&mut self, loc: &Loc, codes: &CodeSet) {
        self.update_file_suppressions(|fs| fs.remove(loc, codes), loc);
    }

    #[allow(dead_code)]
    fn remove_lint_suppression_from_map(&mut self, loc: &Loc) {
        self.update_file_suppressions(|fs| fs.remove_lint_suppression(loc), loc);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ErrorCodeKind {
    Unsuppressable,
    RequireSpecific,
}

impl ErrorSuppressions {
    #[allow(dead_code)]
    fn check_loc(
        &self,
        specific_codes: &CodeMap<ErrorCodeKind>,
        result: Severity,
        unused: &mut Self,
        loc: &Loc,
    ) -> (Severity, BTreeSet<Loc>) {
        // We only want to check the starting position of the reason
        let loc = loc.first_char();

        let specific_codes_set = || {
            CodeSet::from(
                specific_codes
                    .keys()
                    .map(|(code, loc)| (FlowSmolStr::new(code), loc.dupe())),
            )
        };

        let suppression_applies = |codes: &ApplicableCodes| -> bool {
            match codes {
                ApplicableCodes::Specific(specific_codes2) => {
                    specific_codes.for_all(|code, kind| match kind {
                        ErrorCodeKind::Unsuppressable => false,
                        ErrorCodeKind::RequireSpecific => specific_codes2.mem(code),
                    })
                }
                ApplicableCodes::All { .. } => specific_codes.for_all(|_code, kind| match kind {
                    ErrorCodeKind::Unsuppressable => false,
                    ErrorCodeKind::RequireSpecific => true,
                }),
            }
        };

        match self.suppression_at_loc(&loc) {
            Some((locs, codes)) if suppression_applies(codes) => {
                let used: BTreeSet<Loc> = locs.iter().duped().collect();
                unused.remove_suppression_from_map(&loc, &specific_codes_set());
                (Severity::Off, used)
            }
            _ => (result, BTreeSet::new()),
        }
    }

    fn in_node_modules(root: &Path, file_options: Option<&FileOptions>, loc: &Loc) -> bool {
        match (loc.source.as_ref(), file_options) {
            (Some(file), Some(options)) => {
                flow_common::files::is_within_node_modules(root, options, &file.to_absolute())
            }
            _ => false,
        }
    }

    fn in_declarations(file_options: Option<&FileOptions>, loc: &Loc) -> bool {
        match (loc.source.as_ref(), file_options) {
            (Some(file), Some(options)) => {
                flow_common::files::is_declaration(options, &file.to_absolute())
            }
            _ => false,
        }
    }

    fn check<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(
        root: &Path,
        file_options: Option<&FileOptions>,
        node_modules_errors: bool,
        unsuppressable_error_codes: &BTreeSet<FlowSmolStr>,
        err: &IntermediateError<L>,
        suppressions: &Self,
        unused: &mut Self,
    ) -> Option<(Severity, BTreeSet<Loc>)> {
        let loc = &err.loc;
        let code_opt = err.error_code.as_ref().map(|code| {
            let string_code = FlowSmolStr::new(code.as_str());
            let kind = if unsuppressable_error_codes.contains(&string_code) {
                ErrorCodeKind::Unsuppressable
            } else {
                ErrorCodeKind::RequireSpecific
            };
            CodeMap::singleton(string_code, loc.dupe(), kind)
        });

        // Ignore lint errors from node modules (unless node_modules_errors is enabled),
        // and all errors from declarations directories.
        let ignore = match err.kind {
            ErrorKind::LintError(_) => {
                (!node_modules_errors && Self::in_node_modules(root, file_options, loc))
                    || Self::in_declarations(file_options, loc)
            }
            _ => Self::in_declarations(file_options, loc),
        };

        match (ignore, code_opt) {
            (true, _) => None,
            (_, None) => Some((Severity::Err, BTreeSet::new())),
            (_, Some(specific_codes)) => {
                let (result, used) =
                    suppressions.check_loc(&specific_codes, Severity::Err, unused, loc);
                let result = match err.kind {
                    ErrorKind::RecursionLimitError => {
                        // TODO: any related suppressions should not be considered used
                        Severity::Err
                    }
                    _ => result,
                };
                Some((result, used))
            }
        }
    }

    // Gets the locations of the suppression comments that are yet unused
    pub fn all_unused_locs(&self) -> BTreeSet<Loc> {
        self.0
            .values()
            .flat_map(|fs| fs.all_unused_locs())
            .collect()
    }

    // Filter out suppressed errors. also track which suppressions are used.
    pub fn filter_suppressed_errors<F, G>(
        &self,
        root: &Path,
        file_options: Option<&flow_common::files::FileOptions>,
        node_modules_errors: bool,
        unsuppressable_codes: &BTreeSet<FlowSmolStr>,
        loc_of_aloc: F,
        get_ast: G,
        errors: ErrorSet,
        unused: &mut Self,
    ) -> (
        ConcreteLocPrintableErrorSet,
        Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>,
    )
    where
        F: Fn(&ALoc) -> Loc,
        G: Fn(&FileKey) -> Option<Arc<ast::Program<Loc, Loc>>>,
    {
        errors.fold(
            (ConcreteLocPrintableErrorSet::new(), Vec::new()),
            |acc, error| {
                let (mut errors, mut suppressed) = acc;
                let intermediate_error = make_intermediate_error(&loc_of_aloc, false, &error);

                match Self::check(
                    root,
                    file_options,
                    node_modules_errors,
                    unsuppressable_codes,
                    &intermediate_error,
                    self,
                    unused,
                ) {
                    None => (errors, suppressed),
                    Some((severity, used)) => match severity {
                        Severity::Off => {
                            suppressed.push((intermediate_error, used));
                            (errors, suppressed)
                        }
                        _ => {
                            let intermediate_error =
                                if let Some(ref code) = intermediate_error.error_code {
                                    if unsuppressable_codes.contains(code.as_str()) {
                                        IntermediateError {
                                            unsuppressable: true,
                                            ..intermediate_error
                                        }
                                    } else {
                                        intermediate_error
                                    }
                                } else {
                                    intermediate_error
                                };

                            let printable_error = to_printable_error(
                                &loc_of_aloc,
                                &get_ast,
                                Some(root),
                                intermediate_error,
                            );
                            errors.add(printable_error);
                            (errors, suppressed)
                        }
                    },
                }
            },
        )
    }

    // Union the two collections of suppressions. If they both contain suppressions for a given file,
    // discard those included in the first argument.
    pub fn update_suppressions(&mut self, other: Self) {
        for (file, file_suppressions) in other.0 {
            if file_suppressions.is_empty() {
                self.0.remove(&file);
            } else {
                self.0.insert(file, file_suppressions);
            }
        }
    }
}

pub fn get_lint_settings<'a>(
    severity_cover: &'a FlowOrdMap<FileKey, LintSeverityCover>,
    loc: &Loc,
) -> Option<&'a LintSettings<Severity>> {
    let source = loc.source.as_ref()?;
    let cover = severity_cover.get(source)?;
    cover.find_opt(loc)
}

impl ErrorSuppressions {
    // Filter out lint errors which are definitely suppressed or were never
    // enabled in the first place.
    //
    // We use an PrintableErrorSet here (as opposed to a ConcretePrintableErrorSet) because this operation happens
    // during merge rather than during collation as filter_suppressed_errors does
    pub fn filter_lints(
        &mut self,
        errors: ErrorSet,
        // If needed, we will resolve abstract locations using these tables. Context.aloc_tables is most
        // likely the right thing to pass to this.
        aloc_tables: &HashMap<FileKey, flow_aloc::LazyALocTable>,
        include_suppressions: bool,
        severity_cover: &FlowOrdMap<FileKey, LintSeverityCover>,
    ) -> (ErrorSet, ErrorSet) {
        errors.fold((ErrorSet::new(), ErrorSet::new()), |acc, error| {
            let (mut errors_acc, mut warnings) = acc;
            let msg = error.msg_of_error();
            let kind = msg.kind_of_msg();

            match (kind, error.loc_of_error()) {
                (ErrorKind::LintError(lint_kind), Some(aloc)) => {
                    let loc = aloc.to_loc_with_tables(aloc_tables);

                    match get_lint_settings(severity_cover, &loc) {
                        None => {
                            // This shouldn't happen -- the primary location of a lint error
                            // should always be in the file where the error was found. Until we
                            // are more confident that this invariant holds, pass the lint warning
                            // back to the master process, where it will be filtered in the
                            // context of the full severity cover set.
                            warnings.add(error);
                            (errors_acc, warnings)
                        }
                        Some(lint_settings) => {
                            // Lint settings can only affect lint errors when located at the
                            // error's "primary" location. This is a nice property, since it means
                            // we can filter out some lint errors here instead of passing them
                            // back and filtering them later.
                            //
                            // Note that a lint error might still be filtered out later, since a
                            // lint error can be suppressed by a "regular" suppression comment.
                            match lint_settings.get_value(lint_kind) {
                                Severity::Off if include_suppressions => {
                                    // When --include-suppressions is active we only want to remove lints that were
                                    // never enabled in the first place, as opposed to those that are enabled but
                                    // suppressed. We also add them as an error regardless of what they were in the
                                    // first place.
                                    if lint_settings.is_explicit(lint_kind) {
                                        errors_acc.add(error);
                                    }
                                    (errors_acc, warnings)
                                }
                                Severity::Off => {
                                    if let Some(used_suppression) = lint_settings.get_loc(lint_kind)
                                    {
                                        self.remove_lint_suppression_from_map(&used_suppression);
                                    }
                                    (errors_acc, warnings)
                                }
                                Severity::Warn => {
                                    warnings.add(error);
                                    (errors_acc, warnings)
                                }
                                Severity::Err => {
                                    errors_acc.add(error);
                                    (errors_acc, warnings)
                                }
                            }
                        }
                    }
                }
                // Non-lint errors can be suppressed by any location present in the error.
                // A dependency location might be part of the error, and the corresponding
                // suppression is not available from this worker. We need to pass back all
                // errors to be filtered in the master process.
                _ => {
                    errors_acc.add(error);
                    (errors_acc, warnings)
                }
            }
        })
    }

    pub fn filter_by_file(&mut self, files: &FlowOrdSet<FileKey>) {
        self.0.retain(|file, _| files.contains(file));
    }
}

impl Default for ErrorSuppressions {
    fn default() -> Self {
        Self::empty()
    }
}
