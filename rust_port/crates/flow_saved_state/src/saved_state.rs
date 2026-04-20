/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::options::Options;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::ResolvedModule;
use flow_heap::parsing_heaps::SharedMem;
use flow_heap_serialization::serialize_aloc_table;
use flow_heap_serialization::serialize_docblock;
use flow_heap_serialization::serialize_file_sig_with_errors;
use flow_heap_serialization::serialize_type_sig;
use flow_imports_exports::exports::Exports;
use flow_imports_exports::imports::Imports;
use flow_parser::file_key::FileKey;
use flow_parser_utils::package_json::PackageJson;
use flow_server_env::dependency_info::DependencyInfo;
use flow_server_env::dependency_info::PartialDependencyGraph;
use flow_server_env::server_env::Env;
use flow_services_export::export_index::ExportIndex;
use flow_typing_errors::flow_error::ErrorSet;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::compression::saved_state_compression;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct NormalizedFileData {
    pub requires: Vec<FlowImportSpecifier>,
    pub resolved_modules: Vec<ResolvedModule>,
    pub phantom_dependencies: Vec<Modulename>,
    pub exports: Exports,
    pub hash: u64,
    pub imports: Imports,
}

pub type DenormalizedFileData = NormalizedFileData;

// For each parsed file, this is what we will save
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct ParsedFileData {
    pub haste_module_info: Option<HasteModuleInfo>,
    pub normalized_file_data: NormalizedFileData,
    pub ast: Option<Vec<u8>>,
    pub docblock: Option<Vec<u8>>,
    pub aloc_table: Option<Vec<u8>>,
    pub type_sig: Option<Vec<u8>>,
    pub file_sig_with_tolerable_errors: Option<Vec<u8>>,
}

// We also need to store the info for unparsed files
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct UnparsedFileData {
    pub unparsed_haste_module_info: Option<HasteModuleInfo>,
    pub unparsed_hash: u64,
}

// info for package.json files
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct PackageFileData {
    pub package_haste_module_info: Option<HasteModuleInfo>,
    pub package_hash: u64,
    pub package_info: Result<PackageJson, ()>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct SavedStateDependencyGraph {
    pub files: Vec<FileKey>,
    pub impl_deps: Vec<Vec<usize>>,
    pub sig_deps: Vec<Vec<usize>>,
}

// This is the complete saved state data representation
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct SavedStateData {
    // The version header should guarantee that a saved state is used by the same version of Flow.
    // However, config might have changed in a way that invalidates the saved state. In the future,
    // we probably could allow some config options, whitespace, etc. But for now, let's
    // invalidate the saved state if the config has changed at all
    pub flowconfig_hash: FlowSmolStr,
    pub parsed_heaps: Vec<(FileKey, ParsedFileData)>,
    pub unparsed_heaps: Vec<(FileKey, UnparsedFileData)>,
    // package.json info
    pub package_heaps: Vec<(FileKey, PackageFileData)>,
    pub non_flowlib_libs: BTreeSet<FlowSmolStr>,
    // Why store local errors and not merge_errors/suppressions/etc? Well, I have a few reasons:
    //
    // 1. Much smaller data structure. The whole env.errors data structure can be hundreds of MBs
    //    when marshal'd, even when there are 0 errors reported to the user)
    // 2. Saved state is designed to help skip parsing. One of the outputs of parsing are local errors
    // 3. Local errors should be the same after a lazy init and after a full init. This isn't true
    //    for the other members of env.errors which are filled in during typechecking
    pub local_errors: BTreeMap<FileKey, ErrorSet>,
    pub node_modules_containers: BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
    pub dependency_graph: SavedStateDependencyGraph,
}

// Direct serialization saved state data: the shared memory heap is dumped
// directly to disk via SharedMem.save_heap. This type holds only lightweight
// env-level metadata (file sets, dependency graph, etc.) - the per-file data
// lives in the heap dump itself. On load, SharedMem.load_heap bulk-loads the
// heap, so no per-file restoration is needed.
#[derive(Clone)]
pub struct SavedStateEnvData {
    pub flowconfig_hash: FlowSmolStr,
    pub parsed_files: BTreeSet<FileKey>,
    pub unparsed_files: BTreeSet<FileKey>,
    pub package_json_files: BTreeSet<FileKey>,
    pub non_flowlib_libs: BTreeSet<FlowSmolStr>,
    pub local_errors: BTreeMap<FileKey, ErrorSet>,
    pub node_modules_containers: BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
    pub dependency_info: DependencyInfo,
    pub duplicate_providers: BTreeMap<FlowSmolStr, (FileKey, Vec<FileKey>)>,
    pub export_index: Option<ExportIndex>,
}

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub enum LoadedSavedState {
    Legacy_saved_state(SavedStateData),
    Direct_saved_state(SavedStateEnvData),
}

#[derive(serde::Serialize, serde::Deserialize, Clone)]
struct SerializedSavedStateEnvData {
    flowconfig_hash: FlowSmolStr,
    parsed_files: BTreeSet<FileKey>,
    unparsed_files: BTreeSet<FileKey>,
    package_json_files: BTreeSet<FileKey>,
    non_flowlib_libs: BTreeSet<FlowSmolStr>,
    local_errors: BTreeMap<FileKey, ErrorSet>,
    node_modules_containers: BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
    dependency_graph: SavedStateDependencyGraph,
    duplicate_providers: BTreeMap<FlowSmolStr, (FileKey, Vec<FileKey>)>,
    #[serde(skip, default)]
    export_index: Option<ExportIndex>,
    parsed_heaps: Vec<(FileKey, ParsedFileData)>,
    unparsed_heaps: Vec<(FileKey, UnparsedFileData)>,
    package_heaps: Vec<(FileKey, PackageFileData)>,
}

#[allow(non_camel_case_types)]
#[derive(serde::Serialize, serde::Deserialize, Clone)]
enum SerializedLoadedSavedState {
    Legacy_saved_state(SavedStateData),
    Direct_saved_state(SerializedSavedStateEnvData),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub enum InvalidReason {
    Bad_header,
    Build_mismatch { expected: String, actual: String },
    Changed_files,
    Failed_to_marshal(String),
    Failed_to_decompress(String),
    Failed_to_load_heap(String),
    File_does_not_exist,
    Flowconfig_mismatch,
}

#[derive(Debug, Clone)]
pub struct InvalidSavedState(pub InvalidReason);

impl fmt::Display for InvalidReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&invalid_reason_to_string(self))
    }
}

impl std::error::Error for InvalidSavedState {}

impl fmt::Display for InvalidSavedState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn invalid_reason_to_string(reason: &InvalidReason) -> String {
    match reason {
        InvalidReason::Bad_header => "Invalid saved state header".to_string(),
        InvalidReason::Build_mismatch { .. } => {
            "Build ID of saved state does not match this binary".to_string()
        }
        InvalidReason::Changed_files => "A file change invalidated the saved state".to_string(),
        InvalidReason::Failed_to_marshal(_) => {
            "Failed to unmarshal data from saved state".to_string()
        }
        InvalidReason::Failed_to_decompress(_) => {
            "Failed to decompress saved state data".to_string()
        }
        InvalidReason::Failed_to_load_heap(msg) => format!("Failed to load heap: {msg}"),
        InvalidReason::File_does_not_exist => "Saved state file does not exist".to_string(),
        InvalidReason::Flowconfig_mismatch => {
            ".flowconfig has changed since saved state was generated".to_string()
        }
    }
}

pub fn backtrace_of_invalid_reason(reason: &InvalidReason) -> Option<String> {
    match reason {
        InvalidReason::Failed_to_marshal(msg)
        | InvalidReason::Failed_to_decompress(msg)
        | InvalidReason::Failed_to_load_heap(msg) => Some(msg.clone()),
        InvalidReason::Build_mismatch { expected, actual } => {
            Some(format!("Expected {expected:?}, got {actual}"))
        }
        InvalidReason::Bad_header
        | InvalidReason::Changed_files
        | InvalidReason::File_does_not_exist
        | InvalidReason::Flowconfig_mismatch => None,
    }
}

pub fn non_flowlib_libs(saved_state: &LoadedSavedState) -> &BTreeSet<FlowSmolStr> {
    match saved_state {
        LoadedSavedState::Legacy_saved_state(data) => &data.non_flowlib_libs,
        LoadedSavedState::Direct_saved_state(data) => &data.non_flowlib_libs,
    }
}

// With relative paths in File_key.t, denormalization is identity. The data
// was already sorted on the save side, and no path transformation changes
// the sort order.
pub fn denormalize_file_data(_options: &Options, data: NormalizedFileData) -> DenormalizedFileData {
    data
}

pub fn restore_dependency_info(
    pool: &ThreadPool,
    graph: SavedStateDependencyGraph,
) -> DependencyInfo {
    let SavedStateDependencyGraph {
        files,
        impl_deps,
        sig_deps,
    } = graph;
    let mut map = BTreeMap::new();
    for (i, file) in files.iter().enumerate() {
        let sig_files: BTreeSet<FileKey> = sig_deps
            .get(i)
            .into_iter()
            .flat_map(|deps| deps.iter())
            .filter_map(|idx| files.get(*idx))
            .cloned()
            .collect();
        let impl_files: BTreeSet<FileKey> = impl_deps
            .get(i)
            .into_iter()
            .flat_map(|deps| deps.iter())
            .filter_map(|idx| files.get(*idx))
            .cloned()
            .collect();
        map.insert(file.clone(), (sig_files, impl_files));
    }
    DependencyInfo::of_map(pool, PartialDependencyGraph::from_map(map))
}

// It's simplest if the build ID is always the same length. Let's use 16, since that happens to
// be the size of the build ID hash.
const SAVED_STATE_VERSION_LENGTH: usize = 16;

fn saved_state_version() -> String {
    let version = if cfg!(debug_assertions) {
        flow_common_build_id::get_build_id()
    } else {
        let unpadded = flow_common::flow_version::VERSION;
        assert!(unpadded.len() <= SAVED_STATE_VERSION_LENGTH);
        // We have to pad out the build ID to bring it up to the right length
        let padding = "n".repeat(SAVED_STATE_VERSION_LENGTH - unpadded.len());
        format!("{unpadded}{padding}")
    };
    assert_eq!(version.len(), SAVED_STATE_VERSION_LENGTH);
    version
}

// We write the Flow version at the beginning of each saved state file.
fn write_version(file: &mut impl Write) -> Result<(), InvalidReason> {
    file.write_all(saved_state_version().as_bytes())
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))
}

fn verify_version(options: &Options, file: &mut impl Read) -> Result<(), InvalidReason> {
    let mut buf = vec![0u8; SAVED_STATE_VERSION_LENGTH];
    file.read_exact(&mut buf)
        .map_err(|_| InvalidReason::Bad_header)?;
    if options.saved_state_skip_version_check {
        // This is really unsafe! Saved state is marshal'd OCaml data and it's
        // easy to introduce serialization differences that would lead to
        // segfaults. This is only for debugging.
        //
        // We still have to read the version because it consumes from the fd.
        return Ok(());
    }
    let actual = String::from_utf8_lossy(&buf).to_string();
    let expected = saved_state_version();
    if actual == expected {
        Ok(())
    } else {
        Err(InvalidReason::Build_mismatch { expected, actual })
    }
}

fn verify_flowconfig_hash(
    options: &Options,
    flowconfig_hash: &flow_data_structure_wrapper::smol_str::FlowSmolStr,
) -> Result<(), InvalidReason> {
    if !options.saved_state_skip_version_check && options.flowconfig_hash != *flowconfig_hash {
        Err(InvalidReason::Flowconfig_mismatch)
    } else {
        Ok(())
    }
}

fn collect_non_flowlib_libs(env: &Env, options: &Options) -> BTreeSet<FlowSmolStr> {
    let flowlib_root = options
        .file_options
        .default_lib_dir
        .as_ref()
        .map(|dir| match dir {
            flow_common::files::LibDir::Prelude(path)
            | flow_common::files::LibDir::Flowlib(path) => path.clone(),
        });
    env.all_unordered_libs
        .iter()
        .filter(|lib| {
            flowlib_root
                .as_ref()
                .is_none_or(|root| !std::path::Path::new(lib.as_str()).starts_with(root))
        })
        .map(|lib| FlowSmolStr::new(files::relative_path(options.root.as_path(), lib.as_str())))
        .collect()
}

fn collect_node_modules_containers(
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
    options: &Options,
) -> BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>> {
    node_modules_containers
        .iter()
        .map(|(key, value)| {
            (
                FlowSmolStr::new(files::relative_path(options.root.as_path(), key.as_str())),
                value.clone(),
            )
        })
        .collect()
}

fn collect_dependency_graph(dependency_info: &DependencyInfo) -> SavedStateDependencyGraph {
    let impl_map = dependency_info.implementation_dependency_graph().to_map();
    let sig_map = dependency_info.sig_dependency_graph().to_map();
    let files: Vec<_> = impl_map.keys().cloned().collect();
    let file_to_index: BTreeMap<_, _> = files
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, file)| (file, i))
        .collect();
    let impl_deps = files
        .iter()
        .map(|file| {
            impl_map
                .get(file)
                .into_iter()
                .flat_map(|deps| deps.iter())
                .filter_map(|dep| file_to_index.get(dep).copied())
                .collect::<Vec<_>>()
        })
        .collect();
    let sig_deps = files
        .iter()
        .map(|file| {
            sig_map
                .get(file)
                .into_iter()
                .flat_map(|deps| deps.iter())
                .filter_map(|dep| file_to_index.get(dep).copied())
                .collect::<Vec<_>>()
        })
        .collect();
    SavedStateDependencyGraph {
        files,
        impl_deps,
        sig_deps,
    }
}

// Collect all the data for a single parsed file
fn collect_parsed_data(shared_mem: &SharedMem, file: &FileKey) -> ParsedFileData {
    let requires = shared_mem
        .get_requires_unsafe(file)
        .iter()
        .cloned()
        .collect();
    let resolved_requires = shared_mem.get_resolved_requires_unsafe(file);
    let resolved_modules = resolved_requires.get_resolved_modules().to_vec();
    let phantom_dependencies = resolved_requires
        .get_phantom_dependencies()
        .iter()
        .map(|dep| dep.to_modulename())
        .collect();
    ParsedFileData {
        haste_module_info: shared_mem.get_haste_module_info(file),
        normalized_file_data: NormalizedFileData {
            requires,
            resolved_modules,
            phantom_dependencies,
            exports: (*shared_mem.get_exports_unsafe(file)).clone(),
            hash: shared_mem.get_file_hash_unsafe(file),
            imports: (*shared_mem.get_imports_unsafe(file)).clone(),
        },
        ast: None,
        docblock: shared_mem
            .get_docblock(file)
            .map(|docblock| serialize_docblock(docblock.as_ref())),
        aloc_table: shared_mem
            .get_aloc_table(file)
            .map(|table| serialize_aloc_table(table.as_ref())),
        type_sig: shared_mem
            .get_type_sig(file)
            .map(|type_sig| serialize_type_sig(type_sig.as_ref())),
        file_sig_with_tolerable_errors: shared_mem.get_tolerable_file_sig(file).map(
            |(file_sig, tolerable_errors)| {
                serialize_file_sig_with_errors(file_sig.as_ref(), tolerable_errors.as_ref())
            },
        ),
    }
}

// Collect all the data for all the files
fn collect_saved_state_data(
    shared_mem: &Arc<SharedMem>,
    env: &Env,
    options: &Options,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> SavedStateData {
    let parsed_heaps = env
        .files
        .iter()
        .filter(|file| !file.is_lib_file())
        .map(|file| (file.dupe(), collect_parsed_data(shared_mem, file)))
        .collect();
    let unparsed_heaps = env
        .unparsed
        .iter()
        .filter(|file| !file.is_lib_file())
        .map(|file| {
            (
                file.dupe(),
                UnparsedFileData {
                    unparsed_haste_module_info: shared_mem.get_haste_module_info(file),
                    unparsed_hash: shared_mem.get_file_hash_unsafe(file),
                },
            )
        })
        .collect();
    let package_heaps = env
        .package_json_files
        .iter()
        .map(|file| {
            (
                file.dupe(),
                PackageFileData {
                    package_haste_module_info: shared_mem.get_haste_module_info(file),
                    package_hash: shared_mem.get_file_hash_unsafe(file),
                    package_info: shared_mem
                        .get_package_info(file)
                        .map(|pkg| Ok((*pkg).clone()))
                        .unwrap_or(Err(())),
                },
            )
        })
        .collect();
    SavedStateData {
        flowconfig_hash: options.flowconfig_hash.dupe(),
        parsed_heaps,
        unparsed_heaps,
        package_heaps,
        // The builtin flowlibs are excluded from the saved state. The server which loads the saved
        // state will extract and typecheck its own builtin flowlibs
        non_flowlib_libs: collect_non_flowlib_libs(env, options),
        local_errors: env.errors.local_errors.clone(),
        node_modules_containers: collect_node_modules_containers(node_modules_containers, options),
        dependency_graph: collect_dependency_graph(&env.dependency_info),
    }
}

fn collect_saved_state_env_data(
    env: &Env,
    options: &Options,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> SavedStateEnvData {
    let duplicate_providers = env
        .errors
        .duplicate_providers
        .iter()
        .map(|(k, (leader, others))| {
            (
                k.dupe(),
                (
                    leader.dupe(),
                    others.iter().map(|f| f.dupe()).collect::<Vec<_>>(),
                ),
            )
        })
        .collect();
    SavedStateEnvData {
        flowconfig_hash: options.flowconfig_hash.dupe(),
        parsed_files: env
            .files
            .iter()
            .filter(|f| !f.is_lib_file())
            .cloned()
            .collect(),
        unparsed_files: env
            .unparsed
            .iter()
            .filter(|f| !f.is_lib_file())
            .cloned()
            .collect(),
        package_json_files: env.package_json_files.iter().cloned().collect(),
        non_flowlib_libs: collect_non_flowlib_libs(env, options),
        local_errors: env.errors.local_errors.clone(),
        node_modules_containers: collect_node_modules_containers(node_modules_containers, options),
        dependency_info: env.dependency_info.clone(),
        duplicate_providers,
        export_index: None,
    }
}

fn collect_direct_data(
    shared_mem: &Arc<SharedMem>,
    env: &Env,
    options: &Options,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> SerializedSavedStateEnvData {
    let legacy = collect_saved_state_data(shared_mem, env, options, node_modules_containers);
    let saved_state_env_data = collect_saved_state_env_data(env, options, node_modules_containers);
    SerializedSavedStateEnvData {
        flowconfig_hash: legacy.flowconfig_hash,
        parsed_files: saved_state_env_data.parsed_files,
        unparsed_files: saved_state_env_data.unparsed_files,
        package_json_files: saved_state_env_data.package_json_files,
        non_flowlib_libs: saved_state_env_data.non_flowlib_libs,
        local_errors: saved_state_env_data.local_errors,
        node_modules_containers: saved_state_env_data.node_modules_containers,
        dependency_graph: legacy.dependency_graph,
        duplicate_providers: saved_state_env_data.duplicate_providers,
        export_index: saved_state_env_data.export_index,
        parsed_heaps: legacy.parsed_heaps,
        unparsed_heaps: legacy.unparsed_heaps,
        package_heaps: legacy.package_heaps,
    }
}

fn write_loaded_state(
    path: &Path,
    loaded: &SerializedLoadedSavedState,
) -> Result<(), InvalidReason> {
    let compressed = saved_state_compression::marshal_and_compress(loaded)?;
    let mut file =
        File::create(path).map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))?;
    write_version(&mut file)?;
    let bytes = bincode::serde::encode_to_vec(&compressed, bincode::config::legacy())
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))?;
    file.write_all(&bytes)
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))
}

// Saving the saved state generally consists of 3 things:
//
// 1. Collecting the various bits of data
// 2. Normalizing the data so it can be used by other Flow servers. This generally means turning
//    absolute paths into relative paths
// 3. Writing the data to the saved state file
//
// We care a little bit about the perf of generating saved state, so that any script which
// generates saved states has an easier time keeping up. But the perf of saving isn't as important
// as the perf of loading
pub fn save(
    path: &Path,
    shared_mem: &Arc<SharedMem>,
    env: &Env,
    options: &Options,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> Result<(), InvalidReason> {
    let loaded = if options.saved_state_direct_serialization {
        SerializedLoadedSavedState::Direct_saved_state(collect_direct_data(
            shared_mem,
            env,
            options,
            node_modules_containers,
        ))
    } else {
        SerializedLoadedSavedState::Legacy_saved_state(collect_saved_state_data(
            shared_mem,
            env,
            options,
            node_modules_containers,
        ))
    };
    write_loaded_state(path, &loaded)
}

fn denormalize_paths(
    options: &Options,
    non_flowlib_libs: &mut BTreeSet<FlowSmolStr>,
    node_modules_containers: &mut BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) {
    // Raw string paths still need the root prepended
    let root = options.root.as_path();
    *non_flowlib_libs = non_flowlib_libs
        .iter()
        .map(|path| FlowSmolStr::new(files::absolute_path(root, path.as_str())))
        .collect();
    *node_modules_containers = node_modules_containers
        .iter()
        .map(|(key, value)| {
            (
                FlowSmolStr::new(files::absolute_path(root, key.as_str())),
                value.clone(),
            )
        })
        .collect();
}

fn restore_resolved_requires(
    shared_mem: &Arc<SharedMem>,
    file: &FileKey,
    normalized_file_data: &NormalizedFileData,
) {
    let phantom_dependencies = normalized_file_data
        .phantom_dependencies
        .iter()
        .cloned()
        .map(flow_heap::entity::Dependency::from_modulename)
        .collect();
    let resolved_requires = flow_heap::entity::ResolvedRequires::new(
        normalized_file_data.resolved_modules.clone(),
        phantom_dependencies,
    );
    shared_mem.set_resolved_requires(file, resolved_requires);
}

fn restore_parsed(shared_mem: &Arc<SharedMem>, file: &FileKey, parsed_file_data: &ParsedFileData) {
    let normalized_file_data = &parsed_file_data.normalized_file_data;
    shared_mem.add_parsed(
        file.dupe(),
        normalized_file_data.hash,
        parsed_file_data.haste_module_info.clone(),
        None,
        parsed_file_data
            .docblock
            .as_ref()
            .map(|bytes| flow_heap_serialization::deserialize_docblock(file, bytes)),
        parsed_file_data
            .aloc_table
            .as_ref()
            .map(|bytes| flow_heap_serialization::deserialize_aloc_table(bytes)),
        parsed_file_data
            .type_sig
            .as_ref()
            .map(|bytes| flow_heap_serialization::deserialize_type_sig(file, bytes)),
        parsed_file_data
            .file_sig_with_tolerable_errors
            .as_ref()
            .map(|bytes| flow_heap_serialization::deserialize_file_sig_with_errors(file, bytes)),
        Arc::new(normalized_file_data.exports.clone()),
        Arc::from(normalized_file_data.requires.clone()),
        Arc::new(normalized_file_data.imports.clone()),
    );
    restore_resolved_requires(shared_mem, file, normalized_file_data);
}

fn restore_unparsed(
    shared_mem: &Arc<SharedMem>,
    file: &FileKey,
    unparsed_file_data: &UnparsedFileData,
) {
    shared_mem.add_unparsed(
        file.dupe(),
        unparsed_file_data.unparsed_hash,
        unparsed_file_data.unparsed_haste_module_info.clone(),
    );
}

fn restore_package(shared_mem: &Arc<SharedMem>, file: &FileKey, package_data: &PackageFileData) {
    match &package_data.package_info {
        Ok(package_json) => {
            shared_mem.add_package(
                file.dupe(),
                package_data.package_hash,
                package_data.package_haste_module_info.clone(),
                Arc::new(package_json.clone()),
            );
        }
        Err(()) => {
            shared_mem.add_unparsed(
                file.dupe(),
                package_data.package_hash,
                package_data.package_haste_module_info.clone(),
            );
        }
    }
}

// Loading the saved state generally consists of 2 things:
//
// 1. Loading the saved state from a file
// 2. Denormalizing the data. This generally means turning relative paths into absolute paths
//
// This is on the critical path for starting up a server with saved state. We really care about
// the perf
pub fn load(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    path: &Path,
    options: &Options,
) -> Result<LoadedSavedState, InvalidReason> {
    flow_server_env::monitor_rpc::status_update(
        flow_server_env::server_status::Event::ReadSavedState,
    );
    let mut file = File::open(path).map_err(|_| InvalidReason::File_does_not_exist)?;
    verify_version(options, &mut file)?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))?;
    let compressed: saved_state_compression::Compressed =
        bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
            .map(|(v, _)| v)
            .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))?;
    let mut loaded: SerializedLoadedSavedState =
        saved_state_compression::decompress_and_unmarshal(&compressed)?;
    match &mut loaded {
        SerializedLoadedSavedState::Legacy_saved_state(data) => {
            // Signal progress so the status state machine transitions to Loading_saved_state,
            // which is required before the Restoring_heaps_start event can fire.
            flow_server_env::monitor_rpc::status_update(
                flow_server_env::server_status::Event::LoadSavedStateProgress(
                    flow_server_env::server_status::Progress {
                        total: None,
                        finished: 0,
                    },
                ),
            );
            verify_flowconfig_hash(options, &data.flowconfig_hash)?;
            denormalize_paths(
                options,
                &mut data.non_flowlib_libs,
                &mut data.node_modules_containers,
            );
            Ok(LoadedSavedState::Legacy_saved_state(data.clone()))
        }
        SerializedLoadedSavedState::Direct_saved_state(data) => {
            flow_server_env::monitor_rpc::status_update(
                flow_server_env::server_status::Event::LoadSavedStateProgress(
                    flow_server_env::server_status::Progress {
                        total: None,
                        finished: 0,
                    },
                ),
            );
            verify_flowconfig_hash(options, &data.flowconfig_hash)?;
            denormalize_paths(
                options,
                &mut data.non_flowlib_libs,
                &mut data.node_modules_containers,
            );
            for (file, parsed_file_data) in &data.parsed_heaps {
                restore_parsed(shared_mem, file, parsed_file_data);
            }
            for (file, unparsed_file_data) in &data.unparsed_heaps {
                restore_unparsed(shared_mem, file, unparsed_file_data);
            }
            for (file, package_data) in &data.package_heaps {
                restore_package(shared_mem, file, package_data);
            }
            Ok(LoadedSavedState::Direct_saved_state(SavedStateEnvData {
                flowconfig_hash: data.flowconfig_hash.dupe(),
                parsed_files: data.parsed_files.clone(),
                unparsed_files: data.unparsed_files.clone(),
                package_json_files: data.package_json_files.clone(),
                non_flowlib_libs: data.non_flowlib_libs.clone(),
                local_errors: data.local_errors.clone(),
                node_modules_containers: data.node_modules_containers.clone(),
                dependency_info: restore_dependency_info(pool, data.dependency_graph.clone()),
                duplicate_providers: data.duplicate_providers.clone(),
                export_index: data.export_index.clone(),
            }))
        }
    }
}
