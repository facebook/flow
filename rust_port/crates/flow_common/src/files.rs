/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::RwLock;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use lazy_static::lazy_static;
use regex::Regex;

use crate::path_matcher::PathMatcher;
use crate::sys_utils::normalize_filename_dir_sep;

// Sharded cache for std::fs::canonicalize() to avoid kernel dcache lock contention
// when many threads call realpath concurrently.
const CANONICALIZE_CACHE_SHARDS: usize = 128;

struct CanonicalizeCache {
    shards: [Mutex<HashMap<PathBuf, Option<PathBuf>>>; CANONICALIZE_CACHE_SHARDS],
}

impl CanonicalizeCache {
    fn new() -> Self {
        Self {
            shards: std::array::from_fn(|_| Mutex::new(HashMap::new())),
        }
    }

    fn get_or_insert(&self, path: &Path) -> std::io::Result<PathBuf> {
        let mut hasher = std::hash::DefaultHasher::new();
        path.hash(&mut hasher);
        let shard_idx = (hasher.finish() as usize) % CANONICALIZE_CACHE_SHARDS;

        let mut shard = self.shards[shard_idx].lock().unwrap();
        if let Some(cached) = shard.get(path) {
            match cached {
                Some(p) => Ok(p.clone()),
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "cached canonicalize failure",
                )),
            }
        } else {
            let result = std::fs::canonicalize(path);
            let cached_value = result.as_ref().ok().cloned();
            shard.insert(path.to_path_buf(), cached_value);
            result
        }
    }
}

static CANONICALIZE_CACHE: OnceLock<CanonicalizeCache> = OnceLock::new();

/// Cached version of std::fs::canonicalize(). Thread-safe, uses sharded locking.
pub fn cached_canonicalize(path: &Path) -> std::io::Result<PathBuf> {
    CANONICALIZE_CACHE
        .get_or_init(CanonicalizeCache::new)
        .get_or_insert(path)
}

// utilities for supported filenames

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LibDir {
    Prelude(PathBuf),
    Flowlib(PathBuf),
}
#[derive(Debug, Clone)]
pub struct FileOptions {
    pub default_lib_dir: Option<LibDir>,
    pub ignores: Vec<((String, Option<String>), Regex)>,
    pub untyped: Vec<(String, Regex)>,
    pub declarations: Vec<(String, Regex)>,
    pub implicitly_include_root: bool,
    pub includes: PathMatcher,
    pub haste_paths_excludes: Vec<Regex>,
    pub haste_paths_includes: Vec<Regex>,
    pub lib_paths: Vec<(Option<String>, PathBuf)>,
    pub module_declaration_dirnames: Vec<String>,
    pub module_file_exts: Vec<FlowSmolStr>,
    pub module_resource_exts: HashSet<FlowSmolStr>,
    pub multi_platform: bool,
    pub multi_platform_extensions: Vec<FlowSmolStr>,
    pub multi_platform_extension_group_mapping: Vec<(FlowSmolStr, Vec<FlowSmolStr>)>,
    pub node_resolver_dirnames: Vec<String>,
}

impl Default for FileOptions {
    fn default() -> Self {
        Self {
            default_lib_dir: None,
            ignores: Vec::new(),
            untyped: Vec::new(),
            declarations: Vec::new(),
            implicitly_include_root: true,
            includes: PathMatcher::empty(),
            haste_paths_excludes: Vec::new(),
            haste_paths_includes: Vec::new(),
            lib_paths: Vec::new(),
            module_declaration_dirnames: Vec::new(),
            module_file_exts: Vec::new(),
            module_resource_exts: HashSet::new(),
            multi_platform: false,
            multi_platform_extensions: Vec::new(),
            multi_platform_extension_group_mapping: Vec::new(),
            node_resolver_dirnames: vec!["node_modules".to_string()],
        }
    }
}

pub const GLOBAL_FILE_NAME: &str = "(global)";
pub const FLOW_EXT: &str = ".flow";

pub fn has_flow_ext(file: &FileKey) -> bool {
    file.check_suffix(FLOW_EXT)
}

pub fn chop_flow_ext(file: &FileKey) -> FileKey {
    if has_flow_ext(file) {
        file.chop_suffix(FLOW_EXT)
    } else {
        file.dupe()
    }
}

pub fn has_ts_ext(file: &FileKey) -> bool {
    file.check_suffix(".ts")
        || file.check_suffix(".tsx")
        || file.check_suffix(".mts")
        || file.check_suffix(".cts")
}

pub const DTS_EXT: &str = ".d.ts";

pub const DMTS_EXT: &str = ".d.mts";

pub const DCTS_EXT: &str = ".d.cts";

pub fn has_dts_ext(file: &FileKey) -> bool {
    file.check_suffix(DTS_EXT) || file.check_suffix(DMTS_EXT) || file.check_suffix(DCTS_EXT)
}

pub fn js_to_dts(path: &str) -> Option<String> {
    if let Some(stem) = path.strip_suffix(".js") {
        Some(format!("{}{}", stem, DTS_EXT))
    } else if let Some(stem) = path.strip_suffix(".mjs") {
        Some(format!("{}{}", stem, DMTS_EXT))
    } else {
        path.strip_suffix(".cjs")
            .map(|stem| format!("{}{}", stem, DCTS_EXT))
    }
}

pub fn has_declaration_ext(file: &FileKey) -> bool {
    has_flow_ext(file)
}

pub fn chop_declaration_ext(file: &FileKey) -> FileKey {
    if has_flow_ext(file) {
        chop_flow_ext(file)
    } else {
        file.dupe()
    }
}

pub fn is_prefix(prefix: &str, path: &str) -> bool {
    if prefix.ends_with(std::path::MAIN_SEPARATOR) {
        return path.starts_with(prefix);
    }
    if path == prefix {
        return true;
    }
    if let Some(rest) = path.strip_prefix(prefix) {
        return rest.starts_with(std::path::MAIN_SEPARATOR);
    }
    false
}

pub fn haste_name_opt(options: &FileOptions, file: &FileKey) -> Option<String> {
    fn is_valid_haste_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '$' || c == '_' || c == '.' || c == '-'
    }

    fn extract_haste_name(normalized_file_name: &str) -> String {
        use std::path::Path;

        // Get basename
        let basename = Path::new(normalized_file_name)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(normalized_file_name);

        // Chop .flow suffix if present
        let without_flow = basename.strip_suffix(".flow").unwrap_or(basename);

        // Chop .js suffix if present
        match without_flow.strip_suffix(".js") {
            None => normalized_file_name.to_string(),
            Some(name) => {
                // Validate all characters
                if !name.is_empty() && name.chars().all(is_valid_haste_char) {
                    name.to_string()
                } else {
                    normalized_file_name.to_string()
                }
            }
        }
    }

    // Standardize \ to / in path for Windows
    let abs_path = file.to_absolute();
    let normalized_file_name = normalize_filename_dir_sep(&abs_path);

    let matches_includes = options
        .haste_paths_includes
        .iter()
        .any(|r| r.is_match(&normalized_file_name));
    let matches_excludes = options
        .haste_paths_excludes
        .iter()
        .any(|r| r.is_match(&normalized_file_name));

    if matches_includes && !matches_excludes {
        Some(extract_haste_name(&normalized_file_name))
    } else {
        None
    }
}

pub fn grouped_platform_extension_opt<'a>(
    options: &'a FileOptions,
    filename: &str,
) -> Option<&'a (FlowSmolStr, Vec<FlowSmolStr>)> {
    options
        .multi_platform_extension_group_mapping
        .iter()
        .find(|(group_ext, _platforms)| {
            options
                .module_file_exts
                .iter()
                .any(|module_ext| filename.ends_with(&format!("{}{}", group_ext, module_ext)))
        })
}

pub fn relative_interface_mref_of_possibly_platform_specific_file(
    options: &FileOptions,
    file: &FileKey,
) -> Option<String> {
    if !options.multi_platform {
        return None;
    }

    options.module_file_exts.iter().find_map(|module_file_ext| {
        if !file.check_suffix(module_file_ext) {
            return None;
        }
        let file_without_module_file_ext = file.chop_suffix(module_file_ext);
        let platform_match = options
            .multi_platform_extensions
            .iter()
            .find_map(|platform_ext| {
                if file_without_module_file_ext.check_suffix(platform_ext) {
                    let base_file = file_without_module_file_ext.chop_suffix(platform_ext);
                    let base = base_file.as_str();
                    let basename = std::path::Path::new(base)
                        .file_name()
                        .and_then(|s| s.to_str())?;
                    Some(format!("./{}.js", basename))
                } else {
                    None
                }
            });

        if platform_match.is_some() {
            return platform_match;
        }
        if let Some((group_ext, _)) = grouped_platform_extension_opt(options, file.as_str()) {
            let base_file = file_without_module_file_ext.chop_suffix(group_ext);
            let base = base_file.as_str();
            let basename = std::path::Path::new(base)
                .file_name()
                .and_then(|s| s.to_str())?;
            Some(format!("./{}.js", basename))
        } else {
            None
        }
    })
}

pub fn platform_specific_extensions_and_indices_opt(
    options: &FileOptions,
    filename: &str,
) -> Option<Vec<(usize, FlowSmolStr)>> {
    let single_match =
        options
            .multi_platform_extensions
            .iter()
            .enumerate()
            .find_map(|(idx, platform_ext)| {
                if options.module_file_exts.iter().any(|module_ext| {
                    filename.ends_with(&format!("{}{}", platform_ext, module_ext))
                }) {
                    Some((idx, platform_ext.dupe()))
                } else {
                    None
                }
            });

    if let Some(result) = single_match {
        return Some(vec![result]);
    }

    if let Some((_group_ext, platforms)) = grouped_platform_extension_opt(options, filename) {
        Some(
            platforms
                .iter()
                .filter_map(|p| {
                    let ext = FlowSmolStr::from(format!(".{}", p));
                    options
                        .multi_platform_extensions
                        .iter()
                        .position(|e| e == &ext)
                        .map(|idx| (idx, ext))
                })
                .collect(),
        )
    } else {
        None
    }
}

pub fn chop_platform_suffix_for_file(options: &FileOptions, file: &FileKey) -> FileKey {
    let platform_extensions: Vec<FlowSmolStr> = options
        .multi_platform_extensions
        .iter()
        .chain(
            options
                .multi_platform_extension_group_mapping
                .iter()
                .map(|(ext, _)| ext),
        )
        .cloned()
        .collect();

    platform_extensions
        .into_iter()
        .find_map(|platform_ext| {
            options.module_file_exts.iter().find_map(|module_ext| {
                let ext = format!("{}{}", platform_ext, module_ext);
                if file.check_suffix(&ext) {
                    let chopped = file.chop_suffix(&ext);
                    Some(chopped.with_suffix(module_ext))
                } else {
                    None
                }
            })
        })
        .unwrap_or_else(|| file.dupe())
}

pub fn chop_platform_suffix_for_haste_module(options: &FileOptions, module_name: &str) -> String {
    let platform_extensions: Vec<FlowSmolStr> = options
        .multi_platform_extensions
        .iter()
        .chain(
            options
                .multi_platform_extension_group_mapping
                .iter()
                .map(|(ext, _)| ext),
        )
        .cloned()
        .collect();

    platform_extensions
        .into_iter()
        .find_map(|platform_ext| module_name.strip_suffix(platform_ext.as_str()))
        .unwrap_or(module_name)
        .to_owned()
}

pub fn is_json_file(filename: &str) -> bool {
    if let Some(ext) = Path::new(filename).extension() {
        ext == "json"
    } else {
        false
    }
}

// This is the set of file extensions which we watch for changes
pub fn get_all_watched_extensions(options: &FileOptions) -> HashSet<String> {
    let mut result: HashSet<_> = options
        .module_file_exts
        .iter()
        .map(|s| s.as_str().to_owned())
        .collect();
    result.extend(
        options
            .module_resource_exts
            .iter()
            .map(|s| s.as_str().to_owned()),
    );
    result
}

pub fn is_valid_path(options: &FileOptions, path: &str) -> bool {
    fn get_extension(s: &str) -> Option<String> {
        let ext = Path::new(s).extension()?.to_str()?;
        Some(format!(".{}", ext))
    }

    // Given a file foo.bar.baz.bat, checks the extensions .bat, .baz.bat, and .bar.baz.bat
    fn check_ext(file_exts: &HashSet<String>, basename: &str) -> bool {
        fn helper(
            file_exts: &HashSet<String>,
            basename: &str,
            acc: &str,
            ext: Option<String>,
        ) -> bool {
            let Some(ext) = ext else { return false };

            let acc = format!("{}{}", ext, acc);
            if file_exts.contains(&acc) {
                return true;
            }

            if let Some(chopped) = basename.strip_suffix(&ext) {
                let ext = get_extension(chopped);
                helper(file_exts, chopped, &acc, ext)
            } else {
                false
            }
        }

        let acc = "";
        if let Some(basename) = basename.strip_suffix(FLOW_EXT) {
            helper(file_exts, basename, acc, get_extension(basename))
        } else {
            helper(file_exts, basename, acc, get_extension(basename))
        }
    }

    let basename = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap();
    if basename.starts_with('.') {
        return false;
    }
    let file_exts = get_all_watched_extensions(options);
    check_ext(&file_exts, basename) || basename == "package.json"
}

pub fn is_node_module(options: &FileOptions, path: &str) -> bool {
    let basename = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();
    options.node_resolver_dirnames.contains(&basename)
}

pub fn is_flow_file(options: &FileOptions, path: &str) -> bool {
    is_valid_path(options, path) && !Path::new(path).is_dir()
}

pub fn make_path_absolute(root: &Path, path_str: &str) -> PathBuf {
    let path = Path::new(path_str);
    if path.is_relative() {
        root.join(path)
    } else {
        PathBuf::from(path)
    }
}

lazy_static! {
    pub static ref ABSOLUTE_PATH_REGEXP: Regex = Regex::new(r"^(/|[A-Za-z]:[/\\])").unwrap();
}

pub const PROJECT_ROOT_TOKEN: &str = "<PROJECT_ROOT>";
pub const BUILTIN_ROOT_TOKEN: &str = "<BUILTINS>";
fn can_prune_dir(options: &FileOptions) -> bool {
    options
        .ignores
        .iter()
        .all(|((pattern, _), _rx)| !pattern.starts_with('!') && !pattern.ends_with('$'))
}

pub fn is_in_flowlib(options: &FileOptions, path: &str) -> bool {
    match &options.default_lib_dir {
        None => false,
        Some(libdir) => {
            let root = match libdir {
                LibDir::Prelude(path) | LibDir::Flowlib(path) => path,
            };
            is_prefix(&root.to_string_lossy(), path)
        }
    }
}

pub fn ordered_and_unordered_lib_paths(
    options: &FileOptions,
) -> (Vec<(Option<String>, String)>, BTreeSet<String>) {
    let (libs, filter): (
        Vec<(Option<String>, PathBuf)>,
        Box<dyn Fn(&str) -> bool + '_>,
    ) = match &options.default_lib_dir {
        None => (
            options.lib_paths.clone(),
            Box::new(|path: &str| is_valid_path(options, path)),
        ),
        Some(libdir) => {
            let libs = &options.lib_paths;
            let root_path = match libdir {
                LibDir::Prelude(path) | LibDir::Flowlib(path) => path,
            };
            let root = cached_canonicalize(root_path).unwrap_or_else(|_| root_path.clone());
            let root_resolved = root.to_string_lossy().to_string();
            let filter: Box<dyn Fn(&str) -> bool + '_> = Box::new(move |path: &str| {
                is_prefix(&root_resolved, path) || is_valid_path(options, path)
            });
            let mut new_libs = vec![(None, root)];
            new_libs.extend(libs.iter().cloned());
            (new_libs, filter)
        }
    };

    let libs: Vec<(Option<String>, String)> = if libs.is_empty() {
        Vec::new()
    } else {
        libs.iter()
            .flat_map(|(scoped_dir_opt, lib)| {
                let lib_str = lib.to_string_lossy().to_string();
                let filter_prime = |path: &str| -> bool {
                    (path == lib_str || filter(path)) && !is_json_file(path)
                };

                let mut files: std::collections::BTreeSet<String> =
                    std::collections::BTreeSet::new();
                if lib.exists() {
                    for entry in jwalk::WalkDir::new(lib)
                        .parallelism(jwalk::Parallelism::RayonNewPool(2))
                        .into_iter()
                        .filter_map(|e| e.ok())
                    {
                        let path = entry.path();
                        if entry.file_type().is_dir() {
                            continue;
                        }
                        if !entry.file_type().is_file() {
                            continue;
                        }
                        if let Ok(real_path) = cached_canonicalize(&path) {
                            let path_str = real_path.to_string_lossy().to_string();
                            if filter_prime(&path_str) {
                                files.insert(path_str);
                            }
                        }
                    }
                }
                files
                    .into_iter()
                    .map(|lib| (scoped_dir_opt.clone(), lib))
                    .collect::<Vec<_>>()
            })
            .collect()
    };

    let all_libs_set: BTreeSet<String> = libs.iter().map(|(_, lib)| lib.clone()).collect();
    (libs, all_libs_set)
}

fn is_matching_path(path: &str, pattern: &str, rx: &Regex, current: bool) -> bool {
    if pattern.starts_with('!') {
        current && !rx.is_match(path)
    } else {
        current || rx.is_match(path)
    }
}

fn is_matching(path: &str, pattern_list: &[(String, Regex)]) -> bool {
    pattern_list.iter().fold(false, |current, (pattern, rx)| {
        is_matching_path(path, pattern, rx, current)
    })
}

fn is_matching_ignore(
    path: &str,
    pattern_list: &[((String, Option<String>), Regex)],
) -> (bool, Option<String>) {
    pattern_list.iter().fold(
        (false, None),
        |(matched_already, current_backup), ((pattern, backup), rx)| {
            let matches = is_matching_path(path, pattern, rx, matched_already);
            let new_backup = if matches && current_backup.is_none() {
                backup.clone()
            } else {
                current_backup
            };
            (matches, new_backup)
        },
    )
}

/// true if a file path matches an `[ignore]` entry in config
pub fn is_ignored(options: &FileOptions, path: &str) -> (bool, Option<String>) {
    // On Windows, the path may use \ instead of /, but let's standardize the
    // ignore regex to use /
    let path = normalize_filename_dir_sep(path);
    is_matching_ignore(&path, &options.ignores)
}

/// true if a file path matches an `[untyped]` entry in config
pub fn is_untyped(options: &FileOptions, path: &str) -> bool {
    // On Windows, the path may use \ instead of /, but let's standardize the
    // ignore regex to use /
    let path = normalize_filename_dir_sep(path);
    is_matching(&path, &options.untyped)
}

/// true if a file path matches a `[declarations]` entry in config
pub fn is_declaration(options: &FileOptions, path: &str) -> bool {
    // On Windows, the path may use \ instead of /, but let's standardize the
    // ignore regex to use /
    let path = normalize_filename_dir_sep(path);
    is_matching(&path, &options.declarations)
}

/// true if a file path matches an `[include]` path in config
pub fn is_included(options: &FileOptions, f: &str) -> bool {
    options.includes.matches(f)
}

pub fn wanted(
    options: &FileOptions,
    include_libdef: bool,
    lib_fileset: &BTreeSet<String>,
    path: &str,
) -> bool {
    if include_libdef {
        !is_ignored(options, path).0 || lib_fileset.contains(path)
    } else {
        !is_ignored(options, path).0 && !lib_fileset.contains(path)
    }
}

// Calls `next` repeatedly until it is resolved, returning a SSet of results
pub fn get_all(next: &mut dyn FnMut() -> Vec<String>) -> BTreeSet<String> {
    fn get_all_rec(
        next: &mut dyn FnMut() -> Vec<String>,
        mut accum: BTreeSet<String>,
    ) -> BTreeSet<String> {
        let result = next();
        if result.is_empty() {
            accum
        } else {
            for x in result {
                accum.insert(x);
            }
            get_all_rec(next, accum)
        }
    }
    get_all_rec(next, BTreeSet::new())
}

lazy_static! {
    pub static ref DIR_SEP: Regex = Regex::new(r"[/\\]").unwrap();
}

lazy_static! {
    pub static ref CURRENT_DIR_NAME: Regex = Regex::new(r"\.").unwrap();
}

lazy_static! {
    pub static ref PARENT_DIR_NAME: Regex = Regex::new(r"\.\.").unwrap();
}

pub fn watched_paths(options: &FileOptions) -> Vec<PathBuf> {
    let mut stems = options.includes.stems().to_vec();
    stems.sort();
    stems.dedup_by(|b, a| a.starts_with(b));
    stems
}

const MAX_FILES: usize = 1000;

/// Creates a "next" function for finding the files in a given FlowConfig root.
/// This means all the files under the root (if the implicit behavior is enabled)
/// and all the included files, minus the ignored files and the libs.
///
/// If `all` is true, ignored files and libs are also returned.
/// If `include_libdef` is true, libdef files are also included.
/// If subdir is set, then we return the subset of files under subdir.
pub fn make_next_files(
    root: &Path,
    subdir: Option<&Path>,
    all: bool,
    sort: bool,
    options: Arc<FileOptions>,
    include_libdef: bool,
    all_unordered_libs: Arc<BTreeSet<String>>,
    node_modules_containers: &RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>,
    mut send_chunked: impl FnMut(Vec<PathBuf>),
) {
    fn wanted_filter(
        options: &FileOptions,
        path: &Path,
        all: bool,
        include_libdef: bool,
        all_unordered_libs: &BTreeSet<String>,
    ) -> bool {
        let path_str = path.to_string_lossy();
        all || wanted(options, include_libdef, all_unordered_libs, &path_str)
    }

    fn realpath_filter(
        options: &FileOptions,
        path: &Path,
        all: bool,
        include_libdef: bool,
        all_unordered_libs: &BTreeSet<String>,
    ) -> bool {
        let path_str = path.to_string_lossy();
        (is_valid_path(options, &path_str)
            || (include_libdef && all_unordered_libs.contains(&path_str.to_string())))
            && wanted_filter(options, path, all, include_libdef, all_unordered_libs)
    }

    fn no_subdir_path_filter(
        options: &FileOptions,
        root: &Path,
        path: &Path,
        all: bool,
        include_libdef: bool,
        all_unordered_libs: &BTreeSet<String>,
    ) -> bool {
        ((options.implicitly_include_root && path.starts_with(root)) || {
            let path_str = path.to_string_lossy();
            is_included(options, &path_str)
                || (include_libdef && all_unordered_libs.contains(&path_str.to_string()))
        }) && realpath_filter(options, path, all, include_libdef, all_unordered_libs)
    }

    fn subdir_path_filter(
        options: &FileOptions,
        root: &Path,
        subdir: &Path,
        path: &Path,
        all: bool,
        include_libdef: bool,
        all_unordered_libs: &BTreeSet<String>,
    ) -> bool {
        path.starts_with(subdir)
            && ((options.implicitly_include_root && path.starts_with(root)) || {
                let path_str = path.to_string_lossy();
                is_included(options, &path_str)
                    || (include_libdef && all_unordered_libs.contains(&path_str.to_string()))
            })
            && realpath_filter(options, path, all, include_libdef, all_unordered_libs)
    }

    let starting_point_paths = {
        let mut starting_point_paths: Vec<PathBuf> = if include_libdef {
            all_unordered_libs.iter().map(PathBuf::from).collect()
        } else {
            Vec::new()
        };
        if let Some(subdir) = subdir {
            starting_point_paths.push(subdir.to_path_buf());
        } else {
            starting_point_paths.extend(watched_paths(&options));
        }
        starting_point_paths
    };

    let can_prune = can_prune_dir(&options);
    let mut chunk = Vec::new();
    for starting_point_path in starting_point_paths {
        if is_node_module(&options, &starting_point_path.to_string_lossy()) {
            let dirname = starting_point_path
                .parent()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_default();
            let basename = starting_point_path
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_default();
            let mut containers = node_modules_containers.write().unwrap();
            containers
                .entry(FlowSmolStr::new(&dirname))
                .or_default()
                .insert(FlowSmolStr::new(&basename));
        }
        let duped_options = options.dupe();
        let duped_all_unordered_libs = all_unordered_libs.dupe();
        for entry in jwalk::WalkDir::new(starting_point_path)
            .sort(sort)
            .follow_links(true)
            .parallelism(jwalk::Parallelism::RayonNewPool(2))
            .process_read_dir(move |_depth, path, _read_dir_state, children| {
                if can_prune
                    && !wanted_filter(
                        &duped_options,
                        path,
                        all,
                        include_libdef,
                        &duped_all_unordered_libs,
                    )
                {
                    children.clear();
                }
            })
        {
            let Ok(entry) = entry else {
                continue;
            };
            if entry.file_type().is_dir() {
                let path = entry.path();
                let path_str = path.to_string_lossy();
                if is_node_module(&options, &path_str) {
                    let dirname = Path::new(&*path_str)
                        .parent()
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or_default();
                    let basename = Path::new(&*path_str)
                        .file_name()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_default();
                    let mut containers = node_modules_containers.write().unwrap();
                    containers
                        .entry(FlowSmolStr::new(&dirname))
                        .or_default()
                        .insert(FlowSmolStr::new(&basename));
                }
                continue;
            }
            if !entry.file_type().is_file() {
                continue;
            }
            let path = entry.path();

            if !match subdir {
                Some(subdir) => subdir_path_filter(
                    &options,
                    root,
                    subdir,
                    &path,
                    all,
                    include_libdef,
                    &all_unordered_libs,
                ),
                None => no_subdir_path_filter(
                    &options,
                    root,
                    &path,
                    all,
                    include_libdef,
                    &all_unordered_libs,
                ),
            } {
                continue;
            }
            // Note: try_exists() check removed because canonicalize() already
            // verifies the path exists (returns Err for non-existent paths).
            if let Ok(real_path) = cached_canonicalize(&path)
                && (path == real_path
                    || realpath_filter(
                        &options,
                        &real_path,
                        all,
                        include_libdef,
                        &all_unordered_libs,
                    ))
            {
                chunk.push(real_path);
                if chunk.len() == MAX_FILES {
                    send_chunked(chunk);
                    chunk = Vec::new();
                }
            }
        }
    }
    if !chunk.is_empty() {
        send_chunked(chunk);
    }
}

pub fn expand_project_root_token(root: &Path, s: &str) -> String {
    let root_str = root.to_string_lossy();
    let root = normalize_filename_dir_sep(&root_str);
    s.replace(PROJECT_ROOT_TOKEN, &root)
}

pub fn expand_project_root_token_as_relative(s: &str) -> String {
    let s = s.replace(PROJECT_ROOT_TOKEN, "");
    let s = if s.starts_with('/') || s.starts_with('\\') {
        s[1..].to_string()
    } else {
        s
    };
    normalize_filename_dir_sep(&s).into_owned()
}

pub fn expand_builtin_root_token(flowlib_dir: &Path, s: &str) -> String {
    let flowlib_str = flowlib_dir.to_string_lossy();
    let flowlib_dir_str = normalize_filename_dir_sep(&flowlib_str);
    s.replace(BUILTIN_ROOT_TOKEN, &flowlib_dir_str)
}

fn is_windows_root(root: &str) -> bool {
    cfg!(windows)
        && root.len() == 2
        && root.as_bytes()[1] == b':'
        && root.as_bytes()[0].is_ascii_alphabetic()
}

// Split on both '/' and '\\' to match OCaml behavior
pub fn normalize_path(dir: &str, file: &str) -> String {
    let parts: Vec<&str> = file.split(&['/', '\\'][..]).collect();
    normalize_path_parts(dir, &parts)
}

fn normalize_path_parts(dir: &str, parts: &[&str]) -> String {
    match parts {
        [".", rest @ ..] => {
            // ./<names> => dir/names
            normalize_path_parts(dir, rest)
        }
        ["..", rest @ ..] => {
            // ../<names> => parent(dir)/<names>
            let parent = Path::new(dir)
                .parent()
                .unwrap_or(Path::new(""))
                .to_string_lossy();
            normalize_path_parts(&parent, rest)
        }
        ["", rest @ ..] if !rest.is_empty() => {
            // /<names> => /<names>
            construct_path(std::path::MAIN_SEPARATOR_STR, rest)
        }
        [root, rest @ ..] if is_windows_root(root) => {
            // c:\<names> => C:\<names>
            let root = root.to_uppercase();
            construct_path(&format!("{}{}", root, std::path::MAIN_SEPARATOR), rest)
        }
        _ => {
            // <names> => dir/<names>
            construct_path(dir, parts)
        }
    }
}

pub fn construct_path(dir: &str, parts: &[&str]) -> String {
    parts.iter().fold(dir.to_string(), |acc, part| {
        Path::new(&acc).join(part).to_string_lossy().into_owned()
    })
}

pub fn split_path(path: &str) -> Vec<String> {
    fn split_path_inner(path: &str, acc: &mut Vec<String>) {
        let dir = Path::new(path)
            .parent()
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.to_string());

        if path == dir {
            // Reached root
            // For relative paths like ".", stop
            if !Path::new(&dir).is_relative() {
                // For absolute paths, add the root
                match acc.first() {
                    // "/" becomes ["/"]
                    None => acc.insert(0, dir),
                    Some(_) => {
                        // "/path/to/foo.js" becomes ["/path"; "to"; "foo.js"]
                        let new_first =
                            Path::new(&dir).join(&acc[0]).to_string_lossy().into_owned();
                        acc[0] = new_first;
                    }
                }
            }
        } else {
            let basename = Path::new(path)
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            acc.insert(0, basename);
            split_path_inner(&dir, acc);
        }
    }

    let mut result = Vec::new();
    split_path_inner(path, &mut result);
    result
}

fn make_relative(root: &[String], file: &[String]) -> Vec<String> {
    match (root, file) {
        ([dir1, root_rest @ ..], [dir2, file_rest @ ..]) if dir1 == dir2 => {
            make_relative(root_rest, file_rest)
        }
        (root, file) => {
            let mut result = vec!["..".to_string(); root.len()];
            result.extend_from_slice(file);
            result
        }
    }
}

pub fn relative_path(root: &Path, file: &str) -> String {
    let file_path = Path::new(file);
    match file_path.strip_prefix(root) {
        Ok(relative) => relative.to_string_lossy().into_owned(),
        Err(_) => {
            // Fallback to component-based relative path computation
            let root_components = split_path(&root.to_string_lossy());
            let file_components = split_path(file);
            make_relative(&root_components, &file_components).join("/")
        }
    }
}

pub fn absolute_path(root: &Path, file: &str) -> String {
    let root_components: Vec<String> = split_path(&root.to_string_lossy());
    let root_components_rev: Vec<String> = root_components.iter().rev().cloned().collect();

    // Let's avoid creating paths like "/path/to/foo/."
    if file == "." || file.is_empty() {
        return root.to_string_lossy().into_owned();
    }

    let file_components = split_path(file);

    fn make_absolute(root_rev: &[String], file: &[String]) -> Vec<String> {
        match (root_rev, file) {
            ([_, root_rest @ ..], file) if !file.is_empty() && file[0] == ".." => {
                make_absolute(root_rest, &file[1..])
            }
            (root_rev, file) => {
                let mut result: Vec<String> = root_rev.iter().rev().cloned().collect();
                result.extend_from_slice(file);
                result
            }
        }
    }

    make_absolute(&root_components_rev, &file_components).join(std::path::MAIN_SEPARATOR_STR)
}

// helper to get the full path to the "flow-typed" library dir
pub fn get_flowtyped_path(root: &Path) -> PathBuf {
    make_path_absolute(root, "flow-typed")
}

pub fn filename_from_string(
    options: &FileOptions,
    consider_libdefs: bool,
    all_unordered_libs: &std::collections::BTreeSet<String>,
    path: &str,
) -> flow_parser::file_key::FileKey {
    use flow_parser::file_key::FileKey;

    let ext = Path::new(path)
        .extension()
        .and_then(|s| s.to_str())
        .map(|s| format!(".{}", s));

    match ext.as_deref() {
        Some(".json") => FileKey::json_file_of_absolute(path),
        Some(ext) if options.module_resource_exts.contains(ext) => {
            FileKey::resource_file_of_absolute(path)
        }
        _ => {
            if consider_libdefs && all_unordered_libs.contains(path) {
                FileKey::lib_file_of_absolute(path)
            } else {
                FileKey::source_file_of_absolute(path)
            }
        }
    }
}

pub fn mkdirp(path_str: &str, _perm: u32) {
    let parts: Vec<&str> = path_str
        .split(&['/', '\\'][..])
        .filter(|s| !s.is_empty())
        .collect();
    // If path_str is absolute, then path_prefix will be something like C:\ on
    // Windows and / on Linux
    let path_prefix = ABSOLUTE_PATH_REGEXP
        .find(path_str)
        .map(|m| m.as_str().to_string())
        .unwrap_or_default();
    // On Windows, the Str.split above will mean the first part of an absolute
    // path will be something like C:, so let's remove that
    let parts: &[&str] = match parts.as_slice() {
        [first_part, rest @ ..]
            if format!("{}{}", first_part, std::path::MAIN_SEPARATOR) == path_prefix =>
        {
            rest
        }
        _ => &parts,
    };
    parts.iter().fold(path_prefix, |path_str, part| {
        let new_path_str = Path::new(&path_str)
            .join(part)
            .to_string_lossy()
            .into_owned();
        #[cfg(unix)]
        {
            use std::os::unix::fs::DirBuilderExt;
            match std::fs::DirBuilder::new().mode(_perm).create(&new_path_str) {
                Ok(()) => {}
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
                Err(e) => panic!("mkdirp: mkdir {} failed: {}", new_path_str, e),
            }
        }
        #[cfg(not(unix))]
        {
            match std::fs::create_dir(&new_path_str) {
                Ok(()) => {}
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
                Err(e) => panic!("mkdirp: mkdir {} failed: {}", new_path_str, e),
            }
        }
        new_path_str
    });
}

pub fn generate_is_within_node_modules_fn(
    root: &Path,
    options: &FileOptions,
) -> impl Fn(&str) -> bool {
    let root_components = split_path(&root.to_string_lossy());
    let node_resolver_dirnames: HashSet<String> =
        options.node_resolver_dirnames.iter().cloned().collect();

    move |path: &str| -> bool {
        let file_components = split_path(path);
        let relative_parts = make_relative(&root_components, &file_components);
        relative_parts
            .iter()
            .any(|part| node_resolver_dirnames.contains(part))
    }
}

pub fn is_within_node_modules(root: &Path, options: &FileOptions, path: &str) -> bool {
    let checker = generate_is_within_node_modules_fn(root, options);
    checker(path)
}

pub fn imaginary_realpath(path: &str) -> String {
    // Realpath fails on non-existent paths. So let's find the longest prefix which exists. We
    // recurse using Path::exists, which is just a single stat, as opposed to canonicalize which
    // stats /foo, then /foo/bar, then /foo/bar/baz, etc
    fn find_real_prefix(path: &str, rev_suffix: &mut Vec<String>) -> String {
        let basename = Path::new(path)
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_default();
        rev_suffix.insert(0, basename);
        let prefix = Path::new(path)
            .parent()
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_else(|| ".".to_string());
        // Sys.file_exists should always return true for / and for . so we should never get into
        // infinite recursion. Let's assert that
        assert!(prefix != path);
        if Path::new(&prefix).exists() {
            prefix
        } else {
            find_real_prefix(&prefix, rev_suffix)
        }
    }

    let mut rev_suffix = Vec::new();
    let real_prefix = find_real_prefix(path, &mut rev_suffix);
    let abs = std::fs::canonicalize(&real_prefix)
        .unwrap_or_else(|_| panic!("Realpath failed for existent path {}", real_prefix));
    let abs_str = abs.to_string_lossy().into_owned();
    rev_suffix.iter().fold(abs_str, |acc, part| {
        Path::new(&acc).join(part).to_string_lossy().into_owned()
    })
}

pub fn module_file_exts(options: &FileOptions) -> Vec<&str> {
    options
        .module_file_exts
        .iter()
        .map(|s| s.as_str())
        .collect()
}

pub fn node_resolver_dirnames(options: &FileOptions) -> &[String] {
    &options.node_resolver_dirnames
}

pub fn module_declaration_dirnames(options: &FileOptions) -> &[String] {
    &options.module_declaration_dirnames
}

pub fn canonicalize_filenames(
    cwd: &str,
    handle_imaginary: &dyn Fn(&str) -> String,
    filenames: &[String],
) -> Vec<String> {
    filenames
        .iter()
        .map(|filename| {
            let filename = crate::sys_utils::expanduser(filename);
            let filename = normalize_path(cwd, &filename);
            match cached_canonicalize(Path::new(&filename)) {
                Ok(abs) => abs.to_string_lossy().to_string(),
                Err(_) => handle_imaginary(&filename),
            }
        })
        .collect()
}
