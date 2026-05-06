/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::io;
use std::io::ErrorKind;
use std::io::IsTerminal;
use std::io::Write;
use std::panic;
use std::panic::AssertUnwindSafe;
use std::path::Path;
use std::process;
use std::sync::Arc;
use std::sync::LazyLock;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use flow_parser::ParseOptions;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser::parse_error::ParseError;
use regex::Regex;

mod file_utils {
    use super::*;

    pub(super) enum FileKind {
        Dir(String),
        File(String),
    }

    fn lstat_kind(file: &Path) -> Option<fs::FileType> {
        match fs::symlink_metadata(file) {
            Ok(metadata) => Some(metadata.file_type()),
            Err(err) if err.kind() == ErrorKind::NotFound => {
                eprintln!("File not found: {}", file.display());
                None
            }
            Err(err) => panic!("failed to stat {}: {}", file.display(), err),
        }
    }

    type FileSetEntry = (u8, String);

    pub(super) fn fold_files<T, Filter, Action>(
        max_depth: Option<usize>,
        filter: Filter,
        file_only: bool,
        paths: Vec<String>,
        mut action: Action,
        init: T,
    ) -> T
    where
        Filter: Fn(&str) -> bool,
        Action: FnMut(FileKind, T) -> T,
    {
        fn fold<T, Filter, Action>(
            depth: usize,
            max_depth: Option<usize>,
            filter: &Filter,
            file_only: bool,
            action: &mut Action,
            acc: T,
            dir: String,
        ) -> T
        where
            Filter: Fn(&str) -> bool,
            Action: FnMut(FileKind, T) -> T,
        {
            let acc = if !file_only && filter(&dir) {
                action(FileKind::Dir(dir.clone()), acc)
            } else {
                acc
            };

            if max_depth == Some(depth) {
                return acc;
            }

            let entries = fs::read_dir(&dir)
                .unwrap_or_else(|err| panic!("failed to read directory {}: {}", dir, err));
            let files: BTreeSet<FileSetEntry> = entries
                .map(|entry| {
                    entry.unwrap_or_else(|err| panic!("failed to read directory entry: {}", err))
                })
                .filter_map(|entry| {
                    let path = entry.path();
                    let abs = path.to_string_lossy().into_owned();
                    match lstat_kind(&path) {
                        Some(file_type) if file_type.is_file() => Some((0, abs)),
                        Some(file_type) if file_type.is_dir() => Some((1, abs)),
                        _ => None,
                    }
                })
                .collect();

            files.into_iter().fold(acc, |acc, (kind, file)| match kind {
                0 if filter(&file) => action(FileKind::File(file), acc),
                1 => fold(depth + 1, max_depth, filter, file_only, action, acc, file),
                _ => acc,
            })
        }

        paths.into_iter().fold(init, |acc, path| {
            fold(0, max_depth, &filter, file_only, &mut action, acc, path)
        })
    }

    pub(super) fn push_file(kind: FileKind, mut acc: Vec<String>) -> Vec<String> {
        match kind {
            FileKind::Dir(dir) => panic!("file_only should not yield directory {}", dir),
            FileKind::File(filename) => {
                acc.push(filename);
                acc
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VerboseMode {
    Quiet,
    Verbose,
    Normal,
}

enum ErrorReason {
    MissingParseError,
    UnexpectedParseError(Loc, ParseError),
    UncaughtExn(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TestName {
    // filename * strict
    filename: String,
    use_strict: bool,
}

struct TestResult {
    name: TestName,
    result: Result<(), ErrorReason>,
}

type SMap<T> = BTreeMap<String, T>;

type TestNameSet = BTreeSet<TestName>;

type Counts = (usize, usize, usize);
type FeatureAcc = (TestNameSet, TestNameSet, TestNameSet);
type FeatureMap = SMap<FeatureAcc>;
type Test = (TestName, Frontmatter, Arc<str>);

mod progress_bar {
    use super::*;

    pub(super) struct ProgressBar {
        count: usize,
        last_update: f64,
        total: usize,
        chunks: usize,
        frequency: f64,
    }

    impl ProgressBar {
        fn percentage(&self) -> String {
            format!("{:>3}%", self.count * 100 / self.total)
        }

        fn meter(&self) -> String {
            let chunks = self.chunks;
            let c = self.count * chunks / self.total;
            if c == 0 {
                " ".repeat(chunks)
            } else if c == chunks {
                "=".repeat(chunks)
            } else {
                format!("{}>{}", "=".repeat(c - 1), " ".repeat(chunks - c))
            }
        }

        pub(super) fn incr(&mut self) {
            self.count += 1;
        }

        fn to_string(&self, (passed, failed, errored): Counts) -> String {
            let total = (passed + failed) as f64;
            format!(
                "\r{} [{}] {}/{} -- Passed: {} ({:.2}%), Failed: {} ({:.2}%), Errored: {} ({:.2}%)",
                self.percentage(),
                self.meter(),
                self.count,
                self.total,
                passed,
                passed as f64 / total * 100.0,
                failed,
                failed as f64 / total * 100.0,
                errored,
                errored as f64 / total * 100.0,
            )
        }

        pub(super) fn print(&self, status: Counts) {
            print!("\r{}", self.to_string(status));
            io::stdout().flush().expect("stdout should flush");
        }

        fn now() -> f64 {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|duration| duration.as_secs_f64())
                .unwrap_or(0.0)
        }

        pub(super) fn print_throttled(&mut self, status: Counts) {
            let now = Self::now();
            if now - self.last_update > self.frequency {
                self.last_update = now;
                self.print(status);
            }
        }

        pub(super) fn clear(&self, status: Counts) {
            let len = self.to_string(status).len();
            let spaces = " ".repeat(len);
            print!("\r{}\r", spaces);
            io::stdout().flush().expect("stdout should flush");
        }

        pub(super) fn make(chunks: usize, frequency: f64, total: usize) -> Self {
            Self {
                count: 0,
                last_update: 0.0,
                total,
                chunks,
                frequency,
            }
        }
    }
}

use progress_bar::ProgressBar;

fn is_fixture(filename: &str) -> bool {
    let suffix = "FIXTURE.js";
    let slen = suffix.len();
    let len = filename.len();
    len >= slen && filename.as_bytes()[len - slen..] == *suffix.as_bytes()
}

fn files_of_path(path: &str) -> Vec<String> {
    let filter = |filename: &str| !is_fixture(filename);
    let mut files = file_utils::fold_files(
        None,
        filter,
        true,
        vec![path.to_owned()],
        file_utils::push_file,
        Vec::new(),
    );
    files.reverse();
    files
}

fn string_of_name(strip_root: Option<&str>, name: &TestName) -> String {
    let filename = match strip_root {
        Some(root) => name
            .filename
            .strip_prefix(root)
            .expect("test filename should be under strip root"),
        None => &name.filename,
    };
    let strict = if name.use_strict {
        "(strict mode)"
    } else {
        "(default)"
    };
    format!("{} {}", filename, strict)
}

fn print_name(strip_root: Option<&str>, name: &TestName) {
    let cr = if io::stdout().is_terminal() { "\r" } else { "" };
    println!("{}{}", cr, string_of_name(strip_root, name));
    io::stdout().flush().expect("stdout should flush");
}

fn print_error(err: &ErrorReason) {
    match err {
        ErrorReason::MissingParseError => println!("  Missing parse error"),
        ErrorReason::UnexpectedParseError(loc, err) => {
            println!("  {} at {}", err, loc.debug_to_string(false));
        }
        ErrorReason::UncaughtExn(msg) => println!("  Uncaught exception: {}", msg),
    }
    io::stdout().flush().expect("stdout should flush");
}

#[derive(Debug, Clone)]
struct Frontmatter {
    features: Vec<String>,
    flags: Strictness,
    negative: Option<Negative>,
}

#[derive(Debug, Clone)]
struct Negative {
    phase: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Strictness {
    Raw,
    OnlyStrict,
    NoStrict,
    BothStrictnesses,
}

static START_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"/\*---").expect("start regexp should compile"));
static END_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"---\*/").expect("end regexp should compile"));
static CR_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\r\n?").expect("cr regexp should compile"));
static FEATURES_REGEXP: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?m)^ *features: *\[(.*)\]").expect("features regexp should compile")
});
static FLAGS_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^ *flags: *\[(.*)\]").expect("flags regexp should compile"));
static ONLY_STRICT_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("onlyStrict").expect("onlyStrict regexp should compile"));
static NO_STRICT_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("noStrict").expect("noStrict regexp should compile"));
static RAW_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("raw").expect("raw regexp should compile"));
static NEGATIVE_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^ *negative:$").expect("negative regexp should compile"));
static PHASE_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^ +phase: *(.*)").expect("phase regexp should compile"));
static COMMA_SPACES_REGEXP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r", *").expect("comma regexp should compile"));

impl Frontmatter {
    fn matched_group(regex: &Regex, text: &str) -> Option<String> {
        regex
            .captures(text)
            .and_then(|captures| captures.get(1))
            .map(|matched| matched.as_str().to_owned())
    }

    fn opt_matched_group(regex: &Regex, text: &str) -> Option<String> {
        Self::matched_group(regex, text)
    }

    fn contains(needle: &Regex, haystack: &str) -> bool {
        needle.is_match(haystack)
    }

    fn of_string(source: &str) -> Option<Self> {
        let start_idx = START_REGEXP.find(source)?.end();
        let end_idx = END_REGEXP.find_at(source, start_idx)?.start();
        let text = &source[start_idx..end_idx];
        let text = CR_REGEXP.replace_all(text, "\n").into_owned();

        let features = match Self::opt_matched_group(&FEATURES_REGEXP, &text) {
            Some(str_) => COMMA_SPACES_REGEXP
                .split(&str_)
                .filter(|feature| !feature.is_empty())
                .map(str::to_owned)
                .collect(),
            None => Vec::new(),
        };

        let flags = match Self::opt_matched_group(&FLAGS_REGEXP, &text) {
            Some(flags) => {
                let only_strict = Self::contains(&ONLY_STRICT_REGEXP, &flags);
                let no_strict = Self::contains(&NO_STRICT_REGEXP, &flags);
                let raw = Self::contains(&RAW_REGEXP, &flags);
                match (only_strict, no_strict, raw) {
                    (true, false, false) => Strictness::OnlyStrict,
                    (false, true, false) => Strictness::NoStrict,
                    (false, false, true) => Strictness::Raw,
                    // validate other nonsense combos?
                    _ => Strictness::BothStrictnesses,
                }
            }
            None => Strictness::BothStrictnesses,
        };

        let negative = if Self::contains(&NEGATIVE_REGEXP, &text) {
            Some(Negative {
                phase: Self::matched_group(&PHASE_REGEXP, &text)?,
            })
        } else {
            None
        };

        Some(Self {
            features,
            flags,
            negative,
        })
    }

    fn negative_phase(&self) -> Option<&str> {
        match &self.negative {
            Some(Negative { phase }) => Some(phase),
            None => None,
        }
    }
}

fn split_by_strictness(frontmatter: Frontmatter) -> Vec<Frontmatter> {
    match frontmatter.flags {
        Strictness::BothStrictnesses => vec![
            Frontmatter {
                flags: Strictness::OnlyStrict,
                ..frontmatter.clone()
            },
            Frontmatter {
                flags: Strictness::NoStrict,
                ..frontmatter
            },
        ],
        Strictness::OnlyStrict | Strictness::NoStrict | Strictness::Raw => vec![frontmatter],
    }
}

fn parse_test(mut acc: Vec<Test>, filename: String) -> Vec<Test> {
    let content = fs::read_to_string(&filename)
        .unwrap_or_else(|err| panic!("failed to read {}: {}", filename, err));
    let content: Arc<str> = Arc::from(content);
    match Frontmatter::of_string(&content) {
        Some(frontmatter) => {
            for frontmatter in split_by_strictness(frontmatter) {
                let input = TestName {
                    filename: filename.clone(),
                    use_strict: frontmatter.flags == Strictness::OnlyStrict,
                };
                acc.push((input, frontmatter, content.clone()));
            }
            acc
        }
        None => acc,
    }
}

fn panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        return msg.to_string();
    }
    if let Some(msg) = payload.downcast_ref::<String>() {
        return msg.clone();
    }
    "<unknown panic>".to_string()
}

fn catch_unwind_silent<F, R>(f: F) -> std::thread::Result<R>
where
    F: FnOnce() -> R + panic::UnwindSafe,
{
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let result = panic::catch_unwind(f);
    panic::set_hook(default_hook);
    result
}

fn run_test(name: &TestName, frontmatter: &Frontmatter, content: &str) -> TestResult {
    let filename = &name.filename;
    let use_strict = name.use_strict;
    let parse_options = ParseOptions {
        types: false,
        use_strict,
        ..Default::default()
    };
    let result = match catch_unwind_silent(AssertUnwindSafe(|| {
        let (_, errors) = flow_parser::parse_program_file::<()>(
            false,
            None,
            Some(parse_options),
            FileKey::source_file_of_absolute(filename),
            Ok(content),
        );
        errors
    })) {
        Ok(errors) => match (errors.into_iter().next(), frontmatter.negative_phase()) {
            (None, Some("early" | "parse")) => {
                // expected a parse error, didn't get it
                Err(ErrorReason::MissingParseError)
            }
            (Some(_), Some("early" | "parse")) => {
                // expected a parse error, got one
                Ok(())
            }
            (None, Some(_)) | (None, None) => {
                // did not expect a parse error, didn't get one
                Ok(())
            }
            (Some(err), Some(_)) | (Some(err), None) => {
                // did not expect a parse error, got one incorrectly
                Err(ErrorReason::UnexpectedParseError(err.0, err.1))
            }
        },
        Err(payload) => Err(ErrorReason::UncaughtExn(panic_message(payload))),
    };

    TestResult {
        name: name.clone(),
        result,
    }
}

#[derive(Debug, Clone, Copy)]
enum TestStatus {
    Passed,
    Failed,
    Errored,
}

fn incr_result((passed, failed, errored): Counts, result: TestStatus) -> Counts {
    match result {
        TestStatus::Passed => (passed + 1, failed, errored),
        TestStatus::Failed => (passed, failed + 1, errored),
        TestStatus::Errored => (passed, failed, errored + 1),
    }
}

fn fold_test(
    verbose: bool,
    strip_root: Option<&str>,
    bar: &mut Option<ProgressBar>,
    (passed_acc, failed_acc, errored_acc, mut features_acc): (usize, usize, usize, FeatureMap),
    (name, frontmatter, content): Test,
) -> (usize, usize, usize, FeatureMap) {
    if verbose {
        print_name(strip_root, &name);
    }

    let passed = {
        let TestResult { name, result } = run_test(&name, &frontmatter, &content);
        match result {
            Ok(()) => TestStatus::Passed,
            Err(err) => {
                if let Some(bar) = bar.as_ref() {
                    bar.clear((passed_acc, failed_acc, errored_acc));
                }
                if !verbose {
                    print_name(strip_root, &name);
                }
                print_error(&err);
                match err {
                    ErrorReason::UncaughtExn(_) => TestStatus::Errored,
                    ErrorReason::MissingParseError | ErrorReason::UnexpectedParseError(_, _) => {
                        TestStatus::Failed
                    }
                }
            }
        }
    };

    let (passed_acc, failed_acc, errored_acc) =
        incr_result((passed_acc, failed_acc, errored_acc), passed);

    for feature in &frontmatter.features {
        let (feature_passed_acc, feature_failed_acc, feature_errored_acc) = features_acc
            .entry(feature.clone())
            .or_insert_with(|| (TestNameSet::new(), TestNameSet::new(), TestNameSet::new()));
        match passed {
            TestStatus::Passed => {
                feature_passed_acc.insert(name.clone());
            }
            TestStatus::Failed => {
                feature_failed_acc.insert(name.clone());
            }
            TestStatus::Errored => {
                feature_errored_acc.insert(name.clone());
            }
        }
    }

    if let Some(bar) = bar.as_mut() {
        bar.incr();
        match passed {
            TestStatus::Passed => bar.print_throttled((passed_acc, failed_acc, errored_acc)),
            TestStatus::Failed | TestStatus::Errored => {
                bar.print((passed_acc, failed_acc, errored_acc));
            }
        }
    }

    (passed_acc, failed_acc, errored_acc, features_acc)
}

fn print_usage(usage_msg: &str) {
    eprintln!("{}", usage_msg);
    eprintln!("  -q  Enables quiet mode");
    eprintln!("  -v  Enables verbose mode");
    eprintln!("  -s  Print paths relative to root directory");
    eprintln!("  -f  List failed/errored tests by feature tag");
}

fn main_impl() -> i32 {
    let mut verbose_ref = VerboseMode::Normal;
    let mut path_ref = None;
    let mut strip_root_ref = false;
    let mut features_ref = false;

    let usage_msg = "Runs flow parser on test262 tests. Options available:";
    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-q" => verbose_ref = VerboseMode::Quiet,
            "-v" => verbose_ref = VerboseMode::Verbose,
            "-s" => strip_root_ref = true,
            "-f" => features_ref = true,
            _ if arg.starts_with('-') => {
                print_usage(usage_msg);
                return 1;
            }
            _ => path_ref = Some(arg),
        }
    }

    let mut path = match path_ref {
        Some(path) if !path.is_empty() => path,
        Some(_) | None => {
            eprintln!("Invalid usage");
            return 1;
        }
    };
    if !path.ends_with('/') {
        path.push('/');
    }

    let strip_root = if strip_root_ref {
        Some(path.as_str())
    } else {
        None
    };

    let features = features_ref;
    let quiet = verbose_ref == VerboseMode::Quiet;
    let verbose = verbose_ref == VerboseMode::Verbose;
    let mut files = files_of_path(&path);
    files.sort();
    let tests = files.into_iter().fold(Vec::new(), parse_test);
    let test_count = tests.len();

    let mut bar = if quiet || !io::stdout().is_terminal() {
        None
    } else {
        Some(ProgressBar::make(40, 0.1, test_count))
    };

    let (passed, failed, errored, results_by_feature) =
        tests.into_iter().fold((0, 0, 0, SMap::new()), |acc, test| {
            fold_test(verbose, strip_root, &mut bar, acc, test)
        });

    if let Some(bar) = bar.as_ref() {
        bar.clear((passed, failed, errored));
    }

    let total = (passed + failed + errored) as f64;
    println!("\n=== Summary ===");
    println!(
        "Passed:  {} ({:.2}%)",
        passed,
        passed as f64 / total * 100.0
    );
    println!(
        "Failed:  {} ({:.2}%)",
        failed,
        failed as f64 / total * 100.0
    );
    println!(
        "Errored: {} ({:.2}%)",
        errored,
        errored as f64 / total * 100.0
    );

    if !results_by_feature.is_empty() {
        println!("\nFeatures:");
        for (name, (passed_set, failed_set, errored_set)) in &results_by_feature {
            let passed_cnt = passed_set.len();
            let failed_cnt = failed_set.len();
            let errored_cnt = errored_set.len();
            if failed_cnt > 0 || errored_cnt > 0 || verbose {
                let total = passed_cnt + failed_cnt + errored_cnt;
                let total_f = total as f64;
                println!(
                    "  {}: {}/{} ({:.2}%)",
                    name,
                    passed_cnt,
                    total,
                    passed_cnt as f64 / total_f * 100.0,
                );
                if features {
                    for name in failed_set {
                        println!("    [F] {}", string_of_name(strip_root, name));
                    }
                    for name in errored_set {
                        println!("    [E] {}", string_of_name(strip_root, name));
                    }
                }
            }
        }
    }

    0
}

fn main() {
    process::exit(main_impl());
}
