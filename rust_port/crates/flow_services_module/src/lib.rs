/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::RwLock;

use dupe::Dupe;
use flow_common::bitset::Bitset;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_projects::FlowProjects;
use flow_common::options::ModuleSystem;
use flow_common::options::Options;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::Dependency;
use flow_heap::parse::Parse;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parser_utils::package_json::PackageJson;
use lazy_static::lazy_static;
use regex::Regex;
use vec1::Vec1;

fn choose_provider_and_warn_about_duplicates(
    options: &Options,
    m: &FlowSmolStr,
    errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    providers: Vec<FileKey>,
    fallback: impl FnOnce() -> Option<FileKey>,
) -> Option<FileKey> {
    fn warn_duplicate_providers(
        m: &FlowSmolStr,
        provider: &FileKey,
        duplicates: Vec<FileKey>,
        acc: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    ) {
        let Ok(duplicates) = Vec1::try_from_vec(duplicates) else {
            return;
        };
        acc.insert(m.clone(), (provider.dupe(), duplicates));
    }

    let (mut definitions, mut implementations): (Vec<_>, Vec<_>) =
        providers.into_iter().partition(files::has_declaration_ext);

    if implementations.is_empty() && definitions.is_empty() {
        fallback()
    } else if definitions.is_empty() {
        let impl_ = implementations.remove(0);
        let duplicates = implementations;
        warn_duplicate_providers(m, &impl_, duplicates, errmap);
        Some(impl_)
    } else if implementations.is_empty() {
        let defn = definitions.remove(0);
        let duplicates = definitions;
        warn_duplicate_providers(m, &defn, duplicates, errmap);
        Some(defn)
    } else {
        let impl_ = implementations.remove(0);
        let dup_impls = implementations;
        let defn = definitions.remove(0);
        let dup_defns = definitions;

        let def_with_flow_ext_chopped = files::chop_declaration_ext(&defn);
        let file_options = &options.file_options;
        let impl_with_platform_suffix_chopped =
            files::chop_platform_suffix_for_file(file_options, &impl_);

        if def_with_flow_ext_chopped != impl_with_platform_suffix_chopped {
            if files::chop_platform_suffix_for_file(file_options, &def_with_flow_ext_chopped)
                != impl_with_platform_suffix_chopped
            {
                warn_duplicate_providers(m, &defn, vec![impl_.dupe()], errmap);
            }
        }

        warn_duplicate_providers(m, &impl_, dup_impls, errmap);
        warn_duplicate_providers(m, &defn, dup_defns, errmap);

        Some(defn)
    }
}

fn module_name_candidates(options: &Options, name: &str) -> Vec1<String> {
    fn map_name(
        name: &str,
        expand_project_root_token: impl Fn(&str) -> String,
        mapped_names: &mut Vec<String>,
        (regexp, template): &(Regex, String),
    ) {
        let new_name = {
            let result = regexp.replace_all(name, template.as_str());
            expand_project_root_token(&result)
        };

        if new_name != name {
            mapped_names.push(new_name);
        }
    }

    let mappers = &options.module_name_mappers;
    let root = &options.root;
    let expand_project_root_token = |s: &str| files::expand_project_root_token(root, s);

    let mut mapped_names = Vec::new();
    for mapper in mappers.iter() {
        map_name(name, expand_project_root_token, &mut mapped_names, mapper);
    }

    Vec1::from_vec_push(mapped_names, name.to_owned())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageIncompatibleReason {
    New,
    BecameInvalid,
    BecameValid,
    NameChanged(Option<FlowSmolStr>, Option<FlowSmolStr>),
    MainChanged(Option<FlowSmolStr>, Option<FlowSmolStr>),
    /// The `types`/`typings` property changed from the former to the latter
    TypesChanged(Option<FlowSmolStr>, Option<FlowSmolStr>),
    /// The `haste_commonjs` property changed to this value
    HasteCommonjsChanged(bool),
    ExportsChanged,
    Unknown,
}

impl std::fmt::Display for PackageIncompatibleReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn string_of_option(opt: &Option<FlowSmolStr>) -> &str {
            match opt {
                None => "<None>",
                Some(x) => x.as_str(),
            }
        }

        match self {
            Self::New => write!(f, "new"),
            Self::BecameInvalid => write!(f, "became invalid"),
            Self::BecameValid => write!(f, "became valid"),
            Self::NameChanged(old, new_) => write!(
                f,
                "name changed from `{}` to `{}`",
                string_of_option(old),
                string_of_option(new_)
            ),
            Self::MainChanged(old, new_) => write!(
                f,
                "main changed from `{}` to `{}`",
                string_of_option(old),
                string_of_option(new_)
            ),
            // | Types_changed (old, new_) ->
            //   Printf.sprintf "types changed from `%s` to `%s`" (string_of_option old) (string_of_option new_)
            Self::TypesChanged(old, new_) => write!(
                f,
                "types changed from `{}` to `{}`",
                string_of_option(old),
                string_of_option(new_)
            ),
            Self::HasteCommonjsChanged(new_) => {
                write!(f, "haste_commonjs changed from `{}` to `{}`", !new_, new_)
            }
            Self::ExportsChanged => write!(f, "exports changed"),
            Self::Unknown => write!(f, "Unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageIncompatibleReturn {
    Compatible,
    Incompatible(PackageIncompatibleReason),
}

pub fn package_incompatible(
    _filename: &FileKey,
    old_package: Option<Result<PackageJson, ()>>,
    new_package: Result<PackageJson, ()>,
) -> PackageIncompatibleReturn {
    match (old_package, new_package) {
        (None, Ok(_)) => PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::New),
        (None, Err(_)) => PackageIncompatibleReturn::Compatible,
        (Some(Err(())), Err(_)) => PackageIncompatibleReturn::Compatible,
        (Some(Err(())), Ok(_)) => {
            PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::BecameValid)
        }
        (Some(Ok(_)), Err(_)) => {
            PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::BecameInvalid)
        }
        (Some(Ok(old_package)), Ok(new_package)) => {
            let old_main = old_package.main();
            let new_main = new_package.main();
            let old_types = old_package.types();
            let new_types = new_package.types();
            let old_name = old_package.name();
            let new_name = new_package.name();
            let old_haste_commonjs = old_package.haste_commonjs();
            let new_haste_commonjs = new_package.haste_commonjs();
            let old_exports = old_package.exports();
            let new_exports = new_package.exports();

            if old_name == new_name
                && old_main == new_main
                && old_types == new_types
                && old_haste_commonjs == new_haste_commonjs
                && old_exports == new_exports
            {
                return PackageIncompatibleReturn::Compatible;
            }

            if old_name != new_name {
                PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::NameChanged(
                    old_name, new_name,
                ))
            } else if old_main != new_main {
                PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::MainChanged(
                    old_main, new_main,
                ))
            } else if old_types != new_types {
                PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::TypesChanged(
                    old_types, new_types,
                ))
            } else if old_haste_commonjs != new_haste_commonjs {
                PackageIncompatibleReturn::Incompatible(
                    PackageIncompatibleReason::HasteCommonjsChanged(new_haste_commonjs),
                )
            } else if old_exports != new_exports {
                PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::ExportsChanged)
            } else {
                PackageIncompatibleReturn::Incompatible(PackageIncompatibleReason::Unknown)
            }
        }
    }
}

pub struct PackageInfo(Option<PackageJson>);

impl PackageInfo {
    pub fn new(package_json: Option<PackageJson>) -> Self {
        Self(package_json)
    }

    pub fn none() -> Self {
        Self(None)
    }
}

#[derive(Default)]
pub struct PhantomAcc(BTreeMap<Modulename, Option<Dependency>>);

impl PhantomAcc {
    pub fn keys(&self) -> impl Iterator<Item = &Modulename> {
        self.0.keys()
    }
}

lazy_static! {
    static ref CURRENT_DIR_NAME: Regex = Regex::new(r"^\.").unwrap();
    static ref PARENT_DIR_NAME: Regex = Regex::new(r"^\.\.").unwrap();
    static ref ABSOLUTE_PATH_REGEXP: Regex = Regex::new(r"^(/|[A-Za-z]:[/\\])").unwrap();
}

fn is_relative_or_absolute(r: &str) -> bool {
    CURRENT_DIR_NAME.is_match(r) || PARENT_DIR_NAME.is_match(r) || ABSOLUTE_PATH_REGEXP.is_match(r)
}

fn resolve_symlinks(path: &str) -> String {
    use std::path::Path;

    let p = Path::new(path);
    flow_common::files::cached_canonicalize(p)
        .ok()
        .and_then(|p| p.to_str().map(|s| s.to_string()))
        .unwrap_or_else(|| path.to_string())
}

fn record_phantom_dependency(
    mname: Modulename,
    dependency: Option<Dependency>,
    phantom_acc: Option<&mut PhantomAcc>,
) {
    match phantom_acc {
        None => (),
        Some(phantom_acc) => {
            phantom_acc.0.insert(mname, dependency);
        }
    }
}

trait ModuleSystemSig {
    fn exported_module(
        options: &Options,
        file: &FileKey,
        package_info: &PackageInfo,
    ) -> Option<HasteModuleInfo>;

    fn imported_module(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        importing_file: &FileKey,
        phantom_acc: Option<&mut PhantomAcc>,
        import_specifier: &FlowImportSpecifier,
    ) -> Result<Dependency, Option<FlowImportSpecifier>>;

    fn choose_provider(
        options: &Options,
        m: &str,
        errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
        files: Vec<FileKey>,
        fallback: impl FnOnce() -> Option<FileKey>,
    ) -> Option<FileKey>;
}

mod node {
    use std::collections::BTreeSet;
    use std::path::Path;
    use std::sync::Arc;

    use flow_common::files;
    use flow_common::files::FileOptions;
    use flow_common::flow_import_specifier::FlowImportSpecifier;
    use flow_common::flow_projects::ProjectsOptions;
    use flow_common::options::Options;
    use flow_common_modulename::Modulename;
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;
    use flow_heap::parsing_heaps::SharedMem;
    use flow_parser::file_key::FileKey;
    use flow_parser_utils::package_json::PackageJson;
    use vec1::Vec1;

    use super::*;

    pub struct Node;

    impl super::ModuleSystemSig for Node {
        fn exported_module(
            _options: &Options,
            _file: &FileKey,
            _package_info: &PackageInfo,
        ) -> Option<HasteModuleInfo> {
            None
        }

        fn imported_module(
            options: &Options,
            shared_mem: &SharedMem,
            node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
            importing_file: &FileKey,
            phantom_acc: Option<&mut PhantomAcc>,
            import_specifier: &FlowImportSpecifier,
        ) -> Result<Dependency, Option<FlowImportSpecifier>> {
            imported_module_impl(
                options,
                shared_mem,
                node_modules_containers,
                importing_file,
                phantom_acc,
                import_specifier,
            )
        }

        fn choose_provider(
            options: &Options,
            m: &str,
            errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
            files: Vec<FileKey>,
            _fallback: impl FnOnce() -> Option<FileKey>,
        ) -> Option<FileKey> {
            choose_provider_impl(options, m, files, errmap)
        }
    }

    fn path_if_exists(
        shared_mem: &SharedMem,
        file_options: &FileOptions,
        phantom_acc: Option<&mut PhantomAcc>,
        path: &str,
    ) -> Option<Dependency> {
        let path = super::resolve_symlinks(path);
        let file_key = files::filename_from_string(
            file_options,
            false, // Module resolution should never resolve to a libdef file
            &BTreeSet::new(),
            &path,
        );
        let mname = Modulename::eponymous_module(file_key);

        let dependency = shared_mem.get_dependency(&mname);
        match &dependency {
            Some(dep) if shared_mem.get_provider(dep).is_some() => dependency,
            _ => {
                super::record_phantom_dependency(mname, dependency, phantom_acc);
                None
            }
        }
    }

    fn try_dts_counterpart(
        shared_mem: &SharedMem,
        file_options: &FileOptions,
        phantom_acc: Option<&mut PhantomAcc>,
        ts_lib_support: bool,
        path: &str,
    ) -> Option<Dependency> {
        if ts_lib_support {
            match files::js_to_dts(path) {
                Some(dts_path) => path_if_exists(shared_mem, file_options, phantom_acc, &dts_path),
                None => None,
            }
        } else {
            None
        }
    }

    fn path_if_exists_with_file_exts(
        shared_mem: &SharedMem,
        file_options: &FileOptions,
        phantom_acc: Option<&mut PhantomAcc>,
        path: &str,
        file_exts: &[FlowSmolStr],
    ) -> Option<Dependency> {
        let mut phantom_acc = phantom_acc;
        for ext in file_exts {
            let full_path = format!("{}{}", path, ext);
            if let Some(result) = path_if_exists(
                shared_mem,
                file_options,
                phantom_acc.as_deref_mut(),
                &full_path,
            ) {
                return Some(result);
            }
        }
        None
    }

    fn parse_package(
        _options: &Options,
        shared_mem: &SharedMem,
        package_filename: &str,
    ) -> Arc<PackageJson> {
        let package_filename = super::resolve_symlinks(package_filename);
        let file_key = FileKey::json_file_of_absolute(&package_filename);

        shared_mem
            .get_package_info(&file_key)
            .unwrap_or_else(|| Arc::new(PackageJson::empty()))
    }

    fn parse_exports(
        shared_mem: &SharedMem,
        options: &Options,
        mut phantom_acc: Option<&mut PhantomAcc>,
        package_dir: &str,
        subpath: Option<&str>,
        file_exts: &[FlowSmolStr],
    ) -> Option<Dependency> {
        let subpath = subpath.unwrap_or(".");
        let file_options = &options.file_options;
        let export_conditions: Vec<FlowSmolStr> = options
            .node_package_export_conditions
            .iter()
            .map(FlowSmolStr::new)
            .collect();
        let package_json_path = Path::new(package_dir).join("package.json");
        let package = parse_package(options, shared_mem, package_json_path.to_str().unwrap());

        let source_path = package
            .exports()
            .and_then(|exports| exports.resolve_package(subpath, &export_conditions));

        source_path.and_then(|file| {
            let path = files::normalize_path(package_dir, file.as_str());
            let ts_lib_support = options.typescript_library_definition_support;
            try_dts_counterpart(
                shared_mem,
                file_options,
                phantom_acc.as_deref_mut(),
                ts_lib_support,
                &path,
            )
            .or_else(|| path_if_exists(shared_mem, file_options, phantom_acc.as_deref_mut(), &path))
            .or_else(|| {
                path_if_exists_with_file_exts(
                    shared_mem,
                    file_options,
                    phantom_acc,
                    &path,
                    file_exts,
                )
            })
        })
    }

    fn parse_main(
        options: &Options,
        shared_mem: &SharedMem,
        file_options: &FileOptions,
        ts_lib_support: bool,
        mut phantom_acc: Option<&mut PhantomAcc>,
        package_filename: &str,
        file_exts: &[FlowSmolStr],
    ) -> Option<Dependency> {
        let package = parse_package(options, shared_mem, package_filename);
        package.main().and_then(|main| {
            let dir = Path::new(package_filename).parent()?.to_str()?;
            let path = files::normalize_path(dir, main.as_str());
            let path_w_index = Path::new(&path).join("index");
            let path_w_index_str = path_w_index.to_str()?;

            try_dts_counterpart(
                shared_mem,
                file_options,
                phantom_acc.as_deref_mut(),
                ts_lib_support,
                &path,
            )
            .or_else(|| path_if_exists(shared_mem, file_options, phantom_acc.as_deref_mut(), &path))
            .or_else(|| {
                path_if_exists_with_file_exts(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    &path,
                    file_exts,
                )
            })
            .or_else(|| {
                path_if_exists_with_file_exts(
                    shared_mem,
                    file_options,
                    phantom_acc,
                    path_w_index_str,
                    file_exts,
                )
            })
        })
    }

    fn resolve_types_field(
        shared_mem: &SharedMem,
        file_options: &FileOptions,
        mut phantom_acc: Option<&mut PhantomAcc>,
        package_dir: &str,
        package: &PackageJson,
        file_exts: &[FlowSmolStr],
    ) -> Option<Dependency> {
        package.types().and_then(|file| {
            let path = files::normalize_path(package_dir, file.as_str());
            let path_w_index = Path::new(&path).join("index");
            let path_w_index_str = path_w_index.to_str()?;

            path_if_exists(shared_mem, file_options, phantom_acc.as_deref_mut(), &path)
                .or_else(|| {
                    path_if_exists_with_file_exts(
                        shared_mem,
                        file_options,
                        phantom_acc.as_deref_mut(),
                        &path,
                        file_exts,
                    )
                })
                .or_else(|| {
                    path_if_exists_with_file_exts(
                        shared_mem,
                        file_options,
                        phantom_acc,
                        path_w_index_str,
                        file_exts,
                    )
                })
        })
    }

    pub(super) fn resolve_package(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        subpath: Option<&str>,
        package_dir: &str,
    ) -> Option<Dependency> {
        let file_options = &options.file_options;
        let file_exts = &file_options.module_file_exts;
        let full_package_path = subpath
            .map(|s| files::normalize_path(package_dir, s))
            .unwrap_or_else(|| package_dir.to_string());

        let ts_lib_support = options.typescript_library_definition_support;
        let package_json_path = Path::new(package_dir).join("package.json");
        let package = std::cell::OnceCell::new();
        let get_package = || -> Arc<PackageJson> {
            package
                .get_or_init(|| {
                    parse_package(options, shared_mem, package_json_path.to_str().unwrap())
                })
                .clone()
        };

        parse_exports(
            shared_mem,
            options,
            phantom_acc.as_deref_mut(),
            package_dir,
            subpath,
            file_exts,
        )
        .or_else(|| {
            if ts_lib_support
                && get_package().exports().is_none()
                && (subpath.is_none() || subpath == Some("."))
            {
                resolve_types_field(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    package_dir,
                    &get_package(),
                    file_exts,
                )
            } else {
                None
            }
        })
        .or_else(|| {
            let package_json = Path::new(&full_package_path).join("package.json");
            parse_main(
                options,
                shared_mem,
                file_options,
                ts_lib_support,
                phantom_acc.as_deref_mut(),
                package_json.to_str().unwrap(),
                file_exts,
            )
        })
        .or_else(|| {
            let index_path = Path::new(&full_package_path).join("index");
            path_if_exists_with_file_exts(
                shared_mem,
                file_options,
                phantom_acc,
                index_path.to_str().unwrap(),
                file_exts,
            )
        })
    }

    pub(super) fn ordered_allowed_implicit_platform_specific_import(
        file_options: &FileOptions,
        projects_options: &ProjectsOptions,
        importing_file: &str,
        explicit_available_platforms: Option<Vec<FlowSmolStr>>,
    ) -> Option<Vec<FlowSmolStr>> {
        use std::collections::BTreeSet;

        use flow_common::platform_set;

        let available_platform_set = platform_set::available_platforms(
            file_options,
            projects_options,
            importing_file,
            explicit_available_platforms.as_deref(),
        )
        .map(|ps| ps.to_platform_string_set(file_options))?;

        let single_platform = if available_platform_set.len() == 1 {
            vec![available_platform_set.first().unwrap().clone()]
        } else {
            Vec::new()
        };

        let grouped_platform: Vec<FlowSmolStr> = file_options
            .multi_platform_extension_group_mapping
            .iter()
            .find_map(|(group_ext, platforms)| {
                let platforms_set: BTreeSet<FlowSmolStr> = platforms.iter().cloned().collect();
                if available_platform_set.is_subset(&platforms_set) {
                    group_ext.strip_prefix('.').map(FlowSmolStr::new)
                } else {
                    None
                }
            })
            .into_iter()
            .collect();

        let result: Vec<_> = single_platform
            .into_iter()
            .chain(grouped_platform)
            .collect();
        Some(result)
    }

    pub(super) fn resolve_relative(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        relative_to_directory: &str,
        subpath: Option<&str>,
        rel_path: &str,
    ) -> Option<Dependency> {
        let file_options = &options.file_options;
        let projects_options = &options.projects_options;
        let package_path = files::normalize_path(relative_to_directory, rel_path);
        let full_path = subpath
            .map(|s| files::normalize_path(&package_path, s))
            .unwrap_or_else(|| package_path.clone());

        let file_exts = &file_options.module_file_exts;
        let ts_lib_support = options.typescript_library_definition_support;

        match ordered_allowed_implicit_platform_specific_import(
            file_options,
            projects_options,
            importing_file.as_str(),
            None,
        ) {
            None => try_dts_counterpart(
                shared_mem,
                file_options,
                phantom_acc.as_deref_mut(),
                ts_lib_support,
                &full_path,
            )
            .or_else(|| {
                path_if_exists(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    &full_path,
                )
            })
            .or_else(|| {
                path_if_exists_with_file_exts(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    &full_path,
                    file_exts,
                )
            })
            .or_else(|| {
                resolve_package(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    subpath,
                    &package_path,
                )
            }),
            Some(ordered_platforms) => try_dts_counterpart(
                shared_mem,
                file_options,
                phantom_acc.as_deref_mut(),
                ts_lib_support,
                &full_path,
            )
            .or_else(|| {
                path_if_exists(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    &full_path,
                )
            })
            .or_else(|| {
                for platform in &ordered_platforms {
                    let platform_path = format!("{}.{}", full_path, platform);
                    if let Some(result) = path_if_exists_with_file_exts(
                        shared_mem,
                        file_options,
                        phantom_acc.as_deref_mut(),
                        &platform_path,
                        file_exts,
                    ) {
                        return Some(result);
                    }
                }
                None
            })
            .or_else(|| {
                path_if_exists_with_file_exts(
                    shared_mem,
                    file_options,
                    phantom_acc.as_deref_mut(),
                    &full_path,
                    file_exts,
                )
            })
            .or_else(|| resolve_package(options, shared_mem, phantom_acc, subpath, &package_path)),
        }
    }

    fn parse_package_name(specifier: &str) -> (String, String) {
        if specifier.is_empty() {
            return (String::new(), ".".to_string());
        }

        let initial_char = specifier.chars().next().unwrap();
        let dir_sep = std::path::MAIN_SEPARATOR;

        let first_separator_index = specifier.find(dir_sep);

        let separator_index = if initial_char == '@'
            && let Some(first_separator_index) = first_separator_index
        {
            specifier[first_separator_index + 1..]
                .find(dir_sep)
                .map(|i| first_separator_index + 1 + i)
        } else {
            first_separator_index
        };

        let package_name = if let Some(separator_index) = separator_index {
            specifier[..separator_index].to_string()
        } else {
            specifier.to_string()
        };

        let package_subpath = if let Some(separator_index) = separator_index {
            format!(".{}", &specifier[separator_index..])
        } else {
            ".".to_string()
        };

        (package_name, package_subpath)
    }

    pub(super) fn node_module(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        possible_node_module_container_dir: &str,
        import_specifier: &str,
    ) -> Option<Dependency> {
        let file_options = &options.file_options;
        let (package_name, package_subpath) = parse_package_name(import_specifier);

        if let Some(existing_node_modules_dirs) =
            node_modules_containers.get(possible_node_module_container_dir)
        {
            for dirname in &file_options.node_resolver_dirnames {
                if existing_node_modules_dirs.contains(&FlowSmolStr::new(dirname)) {
                    if let Some(result) = resolve_relative(
                        options,
                        shared_mem,
                        phantom_acc.as_deref_mut(),
                        importing_file,
                        possible_node_module_container_dir,
                        Some(&package_subpath),
                        &format!("{}{}{}", dirname, std::path::MAIN_SEPARATOR, package_name),
                    ) {
                        return Some(result);
                    }
                }
            }
        }

        let parent_dir = Path::new(possible_node_module_container_dir).parent()?;
        let parent_str = parent_dir.to_str()?;
        if parent_str != possible_node_module_container_dir {
            node_module(
                options,
                shared_mem,
                node_modules_containers,
                phantom_acc,
                importing_file,
                parent_str,
                import_specifier,
            )
        } else {
            None
        }
    }

    pub(super) fn flow_typed_module(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        import_specifier: &str,
    ) -> Option<Dependency> {
        for relative_to_directory in &options.file_options.module_declaration_dirnames {
            if let Some(result) = resolve_relative(
                options,
                shared_mem,
                phantom_acc.as_deref_mut(),
                importing_file,
                relative_to_directory,
                Some("."),
                import_specifier,
            ) {
                return Some(result);
            }
        }
        None
    }

    pub(super) fn resolve_root_relative(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        import_specifier: &str,
    ) -> Option<Dependency> {
        if !options.node_resolver_allow_root_relative {
            return None;
        }

        let dirnames = &options.node_resolver_root_relative_dirnames;
        let root = options.root.to_string_lossy();

        for (applicable_dirname_opt, dirname) in dirnames.iter() {
            let relative_to_directory = if dirname.is_empty() {
                root.to_string()
            } else {
                files::normalize_path(&root, dirname)
            };

            let applicable = applicable_dirname_opt
                .as_ref()
                .map(|prefix| files::is_file_key_prefix(prefix, importing_file.suffix()))
                .unwrap_or(true);

            if applicable {
                if let Some(result) = resolve_relative(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    &relative_to_directory,
                    None,
                    import_specifier,
                ) {
                    return Some(result);
                }
            }
        }

        None
    }

    fn resolve_import(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        importing_file: &FileKey,
        mut phantom_acc: Option<&mut PhantomAcc>,
        import_specifier: &str,
    ) -> Option<Dependency> {
        let file = importing_file.to_absolute();
        let importing_file_dir = Path::new(&file).parent()?.to_str()?;

        if super::is_relative_or_absolute(import_specifier) {
            resolve_relative(
                options,
                shared_mem,
                phantom_acc.as_deref_mut(),
                importing_file,
                importing_file_dir,
                None,
                import_specifier,
            )
        } else {
            resolve_root_relative(
                options,
                shared_mem,
                phantom_acc.as_deref_mut(),
                importing_file,
                import_specifier,
            )
            .or_else(|| {
                flow_typed_module(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    import_specifier,
                )
            })
            .or_else(|| {
                node_module(
                    options,
                    shared_mem,
                    node_modules_containers,
                    phantom_acc,
                    importing_file,
                    importing_file_dir,
                    import_specifier,
                )
            })
        }
    }

    fn imported_module_impl(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        importing_file: &FileKey,
        mut phantom_acc: Option<&mut PhantomAcc>,
        import_specifier: &FlowImportSpecifier,
    ) -> Result<Dependency, Option<FlowImportSpecifier>> {
        match import_specifier {
            FlowImportSpecifier::Userland(userland) => {
                let candidates = super::module_name_candidates(options, userland.as_str());

                for candidate in candidates.as_slice() {
                    if let Some(m) = resolve_import(
                        options,
                        shared_mem,
                        node_modules_containers,
                        importing_file,
                        phantom_acc.as_deref_mut(),
                        candidate,
                    ) {
                        return Ok(m);
                    }
                }

                Err(None)
            }
            FlowImportSpecifier::HasteImportWithSpecifiedNamespace { .. } => Err(None),
        }
    }

    fn choose_provider_impl(
        options: &Options,
        m: &str,
        files: Vec<FileKey>,
        errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    ) -> Option<FileKey> {
        let m = FlowSmolStr::new(m);
        let fallback = || None;
        super::choose_provider_and_warn_about_duplicates(options, &m, errmap, files, fallback)
    }
}

mod haste {
    use std::path::Path;

    use flow_common::files;
    use flow_common::flow_import_specifier::FlowImportSpecifier;
    use flow_common::options::Options;
    use flow_common_modulename::HasteModuleInfo;
    use flow_common_modulename::Modulename;
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;
    use flow_heap::parsing_heaps::SharedMem;
    use flow_parser::file_key::FileKey;
    use flow_parser::file_key::FileKeyInner;
    use lazy_static::lazy_static;
    use regex::Regex;
    use vec1::Vec1;

    use super::*;

    pub struct Haste;

    impl super::ModuleSystemSig for Haste {
        fn exported_module(
            options: &Options,
            file: &FileKey,
            package_info: &PackageInfo,
        ) -> Option<HasteModuleInfo> {
            exported_module_impl(options, file, package_info)
        }

        fn imported_module(
            options: &Options,
            shared_mem: &SharedMem,
            node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
            importing_file: &FileKey,
            phantom_acc: Option<&mut PhantomAcc>,
            import_specifier: &FlowImportSpecifier,
        ) -> Result<Dependency, Option<FlowImportSpecifier>> {
            imported_module_impl(
                options,
                shared_mem,
                node_modules_containers,
                importing_file,
                phantom_acc,
                import_specifier,
            )
        }

        fn choose_provider(
            options: &Options,
            m: &str,
            errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
            files: Vec<FileKey>,
            _fallback: impl FnOnce() -> Option<FileKey>,
        ) -> Option<FileKey> {
            choose_provider_impl(options, m, files, errmap)
        }
    }

    fn short_module_name_of(file: &FileKey) -> String {
        let path = file.as_str();
        let basename = Path::new(path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(path);

        Path::new(basename)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(basename)
            .to_string()
    }

    fn is_mock(file: &FileKey) -> bool {
        lazy_static! {
            static ref MOCK_PATH: Regex = Regex::new(r".*/__mocks__/.*").unwrap();
        }

        let path = file.as_str();
        path.replace('\\', "/").contains("/__mocks__/")
    }

    fn is_within_node_modules(options: &Options, path: &str) -> bool {
        let root = &options.root;
        let file_options = &options.file_options;
        files::is_within_node_modules(root.as_path(), file_options, path)
    }

    fn exported_module_impl(
        options: &Options,
        file: &FileKey,
        package_info: &PackageInfo,
    ) -> Option<HasteModuleInfo> {
        let namespace_of_path = |fk: &FileKey| -> Bitset {
            FlowProjects::from_path(&options.projects_options, fk.suffix())
                .map(|p| p.to_bitset())
                .unwrap_or_else(|| {
                    panic!("Path {} doesn't match any Haste namespace.", fk.suffix())
                })
        };

        let abs_path = file.to_absolute();
        match file.inner() {
            FileKeyInner::SourceFile(_) => {
                if is_mock(file) {
                    Some(HasteModuleInfo::mk(
                        short_module_name_of(file).into(),
                        namespace_of_path(file),
                    ))
                } else {
                    files::haste_name_opt(&options.file_options, file)
                        .map(|name| HasteModuleInfo::mk(name.into(), namespace_of_path(file)))
                }
            }
            FileKeyInner::JsonFile(_) => {
                if let PackageInfo(Some(pkg)) = package_info {
                    if pkg.haste_commonjs() || !is_within_node_modules(options, &abs_path) {
                        pkg.name()
                            .map(|name| HasteModuleInfo::mk(name.dupe(), namespace_of_path(file)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn package_dir_opt(shared_mem: &SharedMem, file: &FileKey) -> Option<String> {
        if shared_mem.is_package_file(file) {
            use std::path::Path;
            let abs = file.to_absolute();
            Path::new(&abs)
                .parent()
                .and_then(|p| p.to_str())
                .map(|s| s.to_string())
        } else {
            None
        }
    }

    fn resolve_haste_module(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        importing_file_dir: &str,
        namespace_opt: Option<Bitset>,
        import_specifier: &str,
    ) -> Option<Dependency> {
        let (name, subpath) = parse_haste_name(import_specifier);

        let mut resolve = |namespace_bitset: &Bitset| -> Option<Dependency> {
            let haste_info = HasteModuleInfo::mk(name.clone().into(), namespace_bitset.clone());
            let mname = Modulename::Haste(haste_info.clone());
            let dependency = shared_mem.get_dependency(&mname);

            if let Some(provider_file) = dependency
                .as_ref()
                .and_then(|dep| shared_mem.get_provider(dep))
            {
                match (
                    package_dir_opt(shared_mem, &provider_file),
                    subpath.as_slice(),
                ) {
                    (Some(package_dir), []) => super::node::resolve_package(
                        options,
                        shared_mem,
                        phantom_acc.as_deref_mut(),
                        None,
                        &package_dir,
                    ),
                    (Some(package_dir), _) => {
                        record_phantom_dependency(
                            mname.clone(),
                            dependency.clone(),
                            phantom_acc.as_deref_mut(),
                        );

                        let subpath_strs: Vec<&str> = subpath.iter().map(|s| s.as_str()).collect();
                        let path = files::construct_path(&package_dir, &subpath_strs);
                        super::node::resolve_relative(
                            options,
                            shared_mem,
                            phantom_acc.as_deref_mut(),
                            importing_file,
                            importing_file_dir,
                            None,
                            &path,
                        )
                    }
                    (None, []) => dependency,
                    (None, _) => {
                        record_phantom_dependency(mname, dependency, phantom_acc.as_deref_mut());
                        None
                    }
                }
            } else {
                record_phantom_dependency(mname, dependency, phantom_acc.as_deref_mut());
                None
            }
        };

        let haste_namespace_bitset_candidates = if let Some(namespace) = namespace_opt {
            vec![namespace]
        } else {
            let opts = &options.projects_options;
            if let Some(p) = FlowProjects::from_path(opts, importing_file.as_str()) {
                opts.reachable_projects_bitsets_from_projects_bitset(import_specifier, p)
                    .into_iter()
                    .map(|p| p.to_bitset())
                    .collect()
            } else {
                vec![]
            }
        };

        for bitset in &haste_namespace_bitset_candidates {
            if let Some(result) = resolve(bitset) {
                return Some(result);
            }
        }

        None
    }

    fn parse_haste_name(import_specifier: &str) -> (String, Vec<String>) {
        let parts: Vec<&str> = import_specifier.split('/').collect();

        if parts.is_empty() {
            return (import_specifier.to_string(), vec![]);
        }

        if parts[0].starts_with('@') && parts.len() > 1 {
            let name = format!("{}/{}", parts[0], parts[1]);
            let subpath = parts[2..].iter().map(|s| s.to_string()).collect();
            (name, subpath)
        } else {
            let name = parts[0].to_string();
            let subpath = parts[1..].iter().map(|s| s.to_string()).collect();
            (name, subpath)
        }
    }

    fn resolve_haste_module_disallow_platform_specific(
        options: &Options,
        shared_mem: &SharedMem,
        mut phantom_acc: Option<&mut PhantomAcc>,
        importing_file: &FileKey,
        namespace_opt: Option<Bitset>,
        import_specifier: &str,
    ) -> Option<Dependency> {
        let importing_file_abs = importing_file.to_absolute();
        let importing_file_dir = Path::new(&importing_file_abs)
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or("");

        let dependency = resolve_haste_module(
            options,
            shared_mem,
            phantom_acc.as_deref_mut(),
            importing_file,
            importing_file_dir,
            namespace_opt,
            import_specifier,
        );

        let file_options = &options.file_options;
        if file_options.multi_platform {
            if let Some(dep) = &dependency {
                if let Dependency::HasteModule(Modulename::Haste(haste_module_info)) = dep {
                    let module_name = haste_module_info.module_name();
                    let multi_platform_extensions = &file_options.multi_platform_extensions;

                    let is_platform_specific = multi_platform_extensions
                        .iter()
                        .any(|ext| module_name.ends_with(ext.as_str()));

                    if is_platform_specific {
                        if let Dependency::HasteModule(mname) = dep {
                            record_phantom_dependency(
                                mname.clone(),
                                dependency.clone(),
                                phantom_acc,
                            );
                        }
                        return None;
                    }
                }
            }
        }

        dependency
    }

    fn resolve_import(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        importing_file: &FileKey,
        mut phantom_acc: Option<&mut PhantomAcc>,
        import_specifier: &str,
    ) -> Option<Dependency> {
        let importing_file_abs = importing_file.to_absolute();
        let importing_file_dir = Path::new(&importing_file_abs)
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or("");

        if super::is_relative_or_absolute(import_specifier) {
            return super::node::resolve_relative(
                options,
                shared_mem,
                phantom_acc.as_deref_mut(),
                importing_file,
                importing_file_dir,
                None,
                import_specifier,
            );
        }

        let file_options = &options.file_options;
        let projects_options = &options.projects_options;

        if !file_options.multi_platform {
            resolve_haste_module(
                options,
                shared_mem,
                phantom_acc.as_deref_mut(),
                importing_file,
                importing_file_dir,
                None,
                import_specifier,
            )
            .or_else(|| {
                super::node::resolve_root_relative(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    import_specifier,
                )
            })
            .or_else(|| {
                super::node::flow_typed_module(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    import_specifier,
                )
            })
            .or_else(|| {
                super::node::node_module(
                    options,
                    shared_mem,
                    node_modules_containers,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    importing_file_dir,
                    import_specifier,
                )
            })
        } else {
            let ordered_platforms = super::node::ordered_allowed_implicit_platform_specific_import(
                file_options,
                projects_options,
                importing_file.as_str(),
                None,
            );

            match ordered_platforms {
                None => resolve_haste_module_disallow_platform_specific(
                    options,
                    shared_mem,
                    phantom_acc.as_deref_mut(),
                    importing_file,
                    None,
                    import_specifier,
                )
                .or_else(|| {
                    super::node::resolve_root_relative(
                        options,
                        shared_mem,
                        phantom_acc.as_deref_mut(),
                        importing_file,
                        import_specifier,
                    )
                })
                .or_else(|| {
                    super::node::flow_typed_module(
                        options,
                        shared_mem,
                        phantom_acc.as_deref_mut(),
                        importing_file,
                        import_specifier,
                    )
                })
                .or_else(|| {
                    super::node::node_module(
                        options,
                        shared_mem,
                        node_modules_containers,
                        phantom_acc.as_deref_mut(),
                        importing_file,
                        importing_file_dir,
                        import_specifier,
                    )
                }),
                Some(ordered_platforms) => {
                    for platform in &ordered_platforms {
                        let platform_specifier = format!("{}.{}", import_specifier, platform);
                        if let Some(result) = resolve_haste_module(
                            options,
                            shared_mem,
                            phantom_acc.as_deref_mut(),
                            importing_file,
                            importing_file_dir,
                            None,
                            &platform_specifier,
                        ) {
                            return Some(result);
                        }
                    }

                    resolve_haste_module_disallow_platform_specific(
                        options,
                        shared_mem,
                        phantom_acc.as_deref_mut(),
                        importing_file,
                        None,
                        import_specifier,
                    )
                    .or_else(|| {
                        super::node::resolve_root_relative(
                            options,
                            shared_mem,
                            phantom_acc.as_deref_mut(),
                            importing_file,
                            import_specifier,
                        )
                    })
                    .or_else(|| {
                        super::node::flow_typed_module(
                            options,
                            shared_mem,
                            phantom_acc.as_deref_mut(),
                            importing_file,
                            import_specifier,
                        )
                    })
                    .or_else(|| {
                        super::node::node_module(
                            options,
                            shared_mem,
                            node_modules_containers,
                            phantom_acc,
                            importing_file,
                            importing_file_dir,
                            import_specifier,
                        )
                    })
                }
            }
        }
    }

    fn imported_module_impl(
        options: &Options,
        shared_mem: &SharedMem,
        node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
        importing_file: &FileKey,
        mut phantom_acc: Option<&mut PhantomAcc>,
        import_specifier: &FlowImportSpecifier,
    ) -> Result<Dependency, Option<FlowImportSpecifier>> {
        match import_specifier {
            FlowImportSpecifier::Userland(userland) => {
                let candidates = super::module_name_candidates(options, userland.as_str());
                let first_candidate = candidates.first();

                if let Some(m) = resolve_import(
                    options,
                    shared_mem,
                    node_modules_containers,
                    importing_file,
                    phantom_acc.as_deref_mut(),
                    first_candidate,
                ) {
                    return Ok(m);
                }

                let mapped_name = if candidates.len() > 1 {
                    Some(FlowImportSpecifier::userland(FlowSmolStr::new(
                        first_candidate,
                    )))
                } else {
                    None
                };

                Err(mapped_name)
            }
            FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                namespace,
                name,
                allow_implicit_platform_specific_import,
            } => {
                let candidates = super::module_name_candidates(options, name);
                let import_specifier_str = candidates.first();
                let importing_file_abs = importing_file.to_absolute();
                let importing_file_dir = Path::new(&importing_file_abs)
                    .parent()
                    .and_then(|p| p.to_str())
                    .unwrap_or("");
                let namespace_opt = Some(namespace.clone());

                let result = if *allow_implicit_platform_specific_import {
                    let projects = FlowProjects::from_bitset_unchecked(namespace.clone());
                    let explicit_platforms = options
                        .projects_options
                        .multi_platform_ambient_supports_platform_for_project(projects);

                    let ordered_platforms =
                        super::node::ordered_allowed_implicit_platform_specific_import(
                            &options.file_options,
                            &options.projects_options,
                            importing_file.as_str(),
                            explicit_platforms,
                        );

                    match ordered_platforms {
                        Some(platforms) => {
                            for platform in &platforms {
                                let platform_specifier =
                                    format!("{}.{}", import_specifier_str, platform);
                                if let Some(dep) = resolve_haste_module(
                                    options,
                                    shared_mem,
                                    phantom_acc.as_deref_mut(),
                                    importing_file,
                                    importing_file_dir,
                                    namespace_opt.clone(),
                                    &platform_specifier,
                                ) {
                                    return Ok(dep);
                                }
                            }

                            resolve_haste_module_disallow_platform_specific(
                                options,
                                shared_mem,
                                phantom_acc.as_deref_mut(),
                                importing_file,
                                namespace_opt.clone(),
                                import_specifier_str,
                            )
                        }
                        None => resolve_haste_module_disallow_platform_specific(
                            options,
                            shared_mem,
                            phantom_acc.as_deref_mut(),
                            importing_file,
                            namespace_opt.clone(),
                            import_specifier_str,
                        ),
                    }
                } else {
                    resolve_haste_module_disallow_platform_specific(
                        options,
                        shared_mem,
                        phantom_acc,
                        importing_file,
                        namespace_opt,
                        import_specifier_str,
                    )
                };

                match result {
                    Some(dep) => Ok(dep),
                    None => Err(Some(import_specifier.clone())),
                }
            }
        }
    }

    fn choose_provider_impl(
        options: &Options,
        m: &str,
        mut files: Vec<FileKey>,
        errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    ) -> Option<FileKey> {
        if files.is_empty() {
            return None;
        }

        if files.len() == 1 {
            return Some(files.remove(0));
        }

        let (mocks, non_mocks): (Vec<_>, Vec<_>) = files.into_iter().partition(is_mock);

        let fallback = || mocks.first().map(|k| k.dupe());

        choose_provider_and_warn_about_duplicates(
            options,
            &FlowSmolStr::new(m),
            errmap,
            non_mocks,
            fallback,
        )
    }
}

pub type ErrorMap = BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>;

pub fn exported_module(
    options: &Options,
    file: &FileKey,
    package_info: &PackageInfo,
) -> Option<HasteModuleInfo> {
    match options.module_system {
        ModuleSystem::Node => None,
        ModuleSystem::Haste => haste::Haste::exported_module(options, file, package_info),
    }
}

pub fn imported_module(
    options: &Options,
    shared_mem: &SharedMem,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
    importing_file: &FileKey,
    phantom_acc: Option<&mut PhantomAcc>,
    import_specifier: &FlowImportSpecifier,
) -> Result<Dependency, Option<FlowImportSpecifier>> {
    match options.module_system {
        ModuleSystem::Node => node::Node::imported_module(
            options,
            shared_mem,
            node_modules_containers,
            importing_file,
            phantom_acc,
            import_specifier,
        ),
        ModuleSystem::Haste => haste::Haste::imported_module(
            options,
            shared_mem,
            node_modules_containers,
            importing_file,
            phantom_acc,
            import_specifier,
        ),
    }
}

pub fn choose_provider(
    options: &Options,
    m: &str,
    files: Vec<FileKey>,
    errmap: &mut ErrorMap,
) -> Option<FileKey> {
    match options.module_system {
        ModuleSystem::Node => node::Node::choose_provider(options, m, errmap, files, || None),
        ModuleSystem::Haste => haste::Haste::choose_provider(options, m, errmap, files, || None),
    }
}

pub fn add_parsed_resolved_requires(
    options: &Options,
    shared_mem: &SharedMem,
    node_modules_containers: &RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>,
    file: &FileKey,
) -> Result<(), String> {
    use flow_heap::entity::Dependency;
    use flow_heap::entity::ResolvedModule;
    use flow_heap::entity::ResolvedRequires;

    let node_modules_containers = node_modules_containers.read().unwrap();
    let requires = shared_mem.get_requires_unsafe(file);

    let phantom_acc_map = std::collections::BTreeMap::new();
    let mut phantom_acc = PhantomAcc(phantom_acc_map);

    let resolved_modules: Vec<ResolvedModule> = requires
        .iter()
        .map(|import_specifier| {
            match imported_module(
                options,
                shared_mem,
                &node_modules_containers,
                file,
                Some(&mut phantom_acc),
                import_specifier,
            ) {
                Ok(dependency) => match dependency {
                    Dependency::HasteModule(m) => ResolvedModule::HasteModule(m),
                    Dependency::File(f) => ResolvedModule::File(f),
                },
                Err(None) => ResolvedModule::Null,
                Err(Some(spec)) => ResolvedModule::String(spec),
            }
        })
        .collect();

    let phantom_dependencies: Vec<Dependency> = phantom_acc
        .0
        .into_iter()
        .map(|(mname, dep_opt)| match dep_opt {
            Some(dep) => dep,
            None => Dependency::from_modulename(mname),
        })
        .collect();

    let resolved_requires = ResolvedRequires::new(resolved_modules, phantom_dependencies);

    shared_mem.set_resolved_requires(file, resolved_requires);

    Ok(())
}

pub fn commit_modules(
    pool: &flow_utils_concurrency::thread_pool::ThreadPool,
    options: &Options,
    shared_mem: &SharedMem,
    dirty_modules: flow_common_modulename::ModulenameSet,
) -> (
    flow_common_modulename::ModulenameSet,
    BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
) {
    use flow_common_modulename::HasteModuleInfo;
    use flow_common_modulename::ModulenameSet;

    let debug = options.debug;
    if debug {
        flow_hh_logger::info!("Committing modules");
    }

    fn commit_haste(
        options: &Options,
        shared_mem: &SharedMem,
        debug: bool,
        unchanged: &mut ModulenameSet,
        errmap: &mut BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
        mname: &Modulename,
        haste_info: &HasteModuleInfo,
    ) {
        let name = haste_info.module_name();
        let haste_module = shared_mem.get_haste_module_unsafe(haste_info);

        let all_providers = haste_module.get_all_providers();

        let old_provider = haste_module.get_provider();

        let new_provider = choose_provider(options, name, all_providers, errmap);

        match (&old_provider, &new_provider) {
            (_, None) => {
                if debug {
                    eprintln!("no remaining providers: {}", name);
                }
                haste_module.set_provider(None);
            }
            (None, Some(p)) => {
                if debug {
                    eprintln!("initial provider {} -> {}", name, p.as_str());
                }
                haste_module.set_provider(Some(p.dupe()));
            }
            (Some(old_p), Some(new_p)) => {
                if old_p == new_p {
                    if debug {
                        eprintln!("unchanged provider: {} -> {}", name, new_p.as_str());
                    }
                    let old_typed = shared_mem.get_typed_parse_committed(new_p);
                    let new_typed = shared_mem.get_typed_parse(new_p);

                    match (old_typed, new_typed) {
                        (Some(_), None) | (None, Some(_)) => {}
                        _ => {
                            unchanged.insert(mname.clone());
                        }
                    }
                } else {
                    if debug {
                        eprintln!(
                            "new provider: {} -> {} replaces {}",
                            name,
                            new_p.as_str(),
                            old_p.as_str()
                        );
                    }
                    haste_module.set_provider(Some(new_p.dupe()));
                }
            }
        }
    }

    fn commit_file(
        shared_mem: &SharedMem,
        unchanged: &mut ModulenameSet,
        mname: &Modulename,
        file_key: &FileKey,
    ) {
        let get_parses = |key: &FileKey| -> (Option<Parse>, Option<Parse>) {
            (
                shared_mem.get_parse_committed(key),
                shared_mem.get_parse(key),
            )
        };

        let decl_key = file_key.with_suffix(files::FLOW_EXT);
        let (old_decl_parse, new_decl_parse) = get_parses(&decl_key);
        let (old_impl_parse, new_impl_parse) = get_parses(file_key);

        match (
            old_decl_parse,
            new_decl_parse,
            old_impl_parse,
            new_impl_parse,
        ) {
            (None, None, None, None) => {
                unchanged.insert(mname.clone());
            }
            (None, Some(_), _, _)
            | (Some(_), None, _, _)
            | (None, None, Some(_), None)
            | (None, None, None, Some(_)) => {}
            (Some(old_parse), Some(new_parse), _, _)
            | (None, None, Some(old_parse), Some(new_parse)) => {
                let old_typed = matches!(old_parse, Parse::Typed(_));
                let new_typed = matches!(new_parse, Parse::Typed(_));

                match (old_typed, new_typed) {
                    (true, false) | (false, true) => {}
                    _ => {
                        unchanged.insert(mname.clone());
                    }
                }
            }
        }
    }

    let work_items: Vec<&Modulename> = dirty_modules.iter().collect();

    let (unchanged, errmap) = flow_utils_concurrency::map_reduce::fold(
        pool,
        work_items,
        |acc: &mut (
            ModulenameSet,
            BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
        ),
         &mname| {
            let (unchanged, errmap) = acc;
            match mname {
                Modulename::Haste(haste_info) => {
                    commit_haste(
                        options, shared_mem, debug, unchanged, errmap, mname, haste_info,
                    );
                }
                Modulename::Filename(file_key) => {
                    commit_file(shared_mem, unchanged, mname, file_key);
                }
            }
        },
        |acc: &mut (
            ModulenameSet,
            BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
        ),
         other: (
            ModulenameSet,
            BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
        )| {
            let (unchanged, errmap) = acc;
            let (other_unchanged, other_errmap) = other;
            unchanged.extend(other_unchanged);
            errmap.extend(other_errmap);
        },
    );

    let changed_modules: ModulenameSet = dirty_modules
        .into_iter()
        .filter(|m| !unchanged.contains(m))
        .collect();

    if debug {
        flow_hh_logger::info!("Committing modules Done");
    }

    (changed_modules, errmap)
}
