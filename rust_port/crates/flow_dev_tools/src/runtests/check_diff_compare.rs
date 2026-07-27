/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::io;
use std::path::Path;

use similar::TextDiff;

use super::exists;

fn normalize_line_endings(text: &str) -> String {
    // Strip all \r characters.  This handles both \r\n (Windows line endings)
    // and standalone \r that can appear when Flow includes source lines from
    // files with \r\n endings — converting those to \n would create spurious
    // blank lines.
    text.replace('\r', "")
}

fn normalize_windows_escapes(text: &str) -> String {
    if !cfg!(windows) {
        return text.to_owned();
    }
    // Flow reads builtins files with \r\n line endings on Windows.
    // JSON output (e.g. type-at-pos documentation strings) contains literal
    // \r\n escape sequences where Linux has just \n.  Normalize these
    // BEFORE normalizeWindowsPaths runs, so that both sides compare equally
    // after backslash→forward-slash conversion.
    //
    // Only replace \r\n pairs, NOT standalone \r — standalone \r also
    // appears in file paths like <BUILTINS>\react.js where the \r is a
    // backslash followed by 'r', not a carriage-return escape.
    text.replace(r"\r\n", r"\n")
}

fn normalize_windows_paths(text: &str) -> String {
    if !cfg!(windows) {
        return text.to_owned();
    }
    // On Windows, Flow emits backslash path separators (e.g.
    // <BUILTINS>\core.js) but the .exp files use forward slashes.
    // Normalize both .exp and .out so that any backslashes (including those
    // in source-code snippets) are treated identically on both sides.
    text.replace('\\', "/")
}

fn substitute_version(text: &str, version: &str) -> String {
    text.replace("<VERSION>", version)
}

fn substitute_version_for_record(text: &str, version: &str) -> String {
    // Escape all regex-special characters in the version string, not just dots.
    // Semver can include + (build metadata) and - (pre-release) which are
    // regex quantifiers/special characters.
    text.replace(version, "<VERSION>")
}

pub(super) fn diff_output(exp_file: &Path, out_file: &Path, version: &str) -> io::Result<String> {
    if !exists(exp_file) {
        return Ok(format!(
            "Expected output file not found: {}\n",
            exp_file.display()
        ));
    }
    if !exists(out_file) {
        return Ok(format!(
            "Actual output file not found: {}\n",
            out_file.display()
        ));
    }
    let exp_raw = fs::read_to_string(exp_file)?;
    let out_raw = fs::read_to_string(out_file)?;

    let exp = normalize_windows_paths(&normalize_windows_escapes(&normalize_line_endings(
        &substitute_version(&exp_raw, version),
    )));
    let out = normalize_windows_paths(&normalize_windows_escapes(&normalize_line_endings(
        &out_raw,
    )));

    if exp == out {
        return Ok(String::new());
    }

    // Generate unified diff matching `diff -u --strip-trailing-cr`
    let patch = format!(
        "===================================================================\n{}",
        TextDiff::from_lines(&exp, &out)
            .unified_diff()
            .context_radius(3)
            .header(
                &format!("{}\t", exp_file.display()),
                &format!("{}\t", out_file.display()),
            )
    );

    Ok(patch)
}

pub(super) fn record_output(out_file: &Path, exp_file: &Path, version: &str) -> io::Result<()> {
    let out_raw = fs::read_to_string(out_file)?;
    let recorded = substitute_version_for_record(
        &normalize_windows_paths(&normalize_windows_escapes(&normalize_line_endings(
            &out_raw,
        ))),
        version,
    );
    fs::write(exp_file, recorded)
}
