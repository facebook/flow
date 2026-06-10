/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::btree_map::Entry;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Bound;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocTable;
use flow_aloc::LazyALocTable;
use flow_aloc::PackedALocTable;
use flow_common::docblock::Docblock;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_heap_serialization::ReaderCache;
use flow_imports_exports::exports::Exports;
use flow_imports_exports::imports::Imports;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::package_json::PackageJson;
use flow_type_sig::packed_type_sig::Module as TypeSigModule;
use flow_type_sig::signature_error::TolerableError;
use parking_lot::Mutex;

use crate::entity::Dependency;
use crate::entity::EntityTransaction;
use crate::entity::ResolvedRequires;
use crate::haste_module::HasteModule;
use crate::parse::FileEntry;
use crate::parse::PackageParse;
use crate::parse::Parse;
use crate::parse::TypedParse;

pub struct SharedMem {
    file_heap: GcMap<FileKey, FileEntry>,
    pub haste_module_heap: GcMap<HasteModuleInfo, HasteModule>,
    entity_transaction: EntityTransaction,
    reader_cache: ReaderCache,
    configured_heap_size: Option<u64>,
    configured_hash_table_pow: Option<u32>,
    on_compact: RwLock<Option<Arc<dyn Fn() -> Box<dyn FnOnce() + Send> + Send + Sync>>>,
    gc_state: Mutex<GcState>,
}

pub struct HashStats {
    pub nonempty_slots: i32,
    pub used_slots: i32,
    pub slots: i32,
}

const GC_MAP_SHARDS: usize = 256;

#[derive(Debug)]
pub struct GcMap<K, V> {
    shards: [parking_lot::RwLock<BTreeMap<K, V>>; GC_MAP_SHARDS],
}

impl<K, V> Default for GcMap<K, V> {
    fn default() -> Self {
        Self {
            shards: std::array::from_fn(|_| parking_lot::RwLock::new(BTreeMap::new())),
        }
    }
}

impl<K: Ord + Hash, V: Dupe> GcMap<K, V> {
    fn new() -> Self {
        Self::default()
    }

    fn shard_index(key: &K) -> usize {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish() as usize & (GC_MAP_SHARDS - 1)
    }

    fn shard(&self, key: &K) -> &parking_lot::RwLock<BTreeMap<K, V>> {
        &self.shards[Self::shard_index(key)]
    }

    fn len(&self) -> usize {
        self.shards.iter().map(|shard| shard.read().len()).sum()
    }

    fn get(&self, key: &K) -> Option<V> {
        self.shard(key).read().get(key).map(|value| value.dupe())
    }

    fn insert(&self, key: K, value: V) -> Option<V> {
        let mut map = self.shard(&key).write();
        match map.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(value);
                None
            }
            Entry::Occupied(_) => Some(value),
        }
    }

    fn ensure(&self, key: &K, value: impl FnOnce() -> V) -> (V, bool)
    where
        K: Dupe,
    {
        if let Some(value) = self.shard(key).read().get(key).map(|value| value.dupe()) {
            return (value, false);
        }

        let mut map = self.shard(key).write();
        match map.entry(key.dupe()) {
            Entry::Vacant(entry) => (entry.insert(value()).dupe(), true),
            Entry::Occupied(entry) => (entry.get().dupe(), false),
        }
    }

    fn remove_if(&self, key: &K, predicate: impl FnOnce(&V) -> bool) -> Option<V> {
        let mut map = self.shard(key).write();
        if map.get(key).is_some_and(predicate) {
            map.remove(key)
        } else {
            None
        }
    }

    fn keys_after(&self, shard_index: usize, after: Option<&K>, limit: usize) -> (Vec<K>, bool)
    where
        K: Dupe,
    {
        let map = self.shards[shard_index].read();
        let mut keys = Vec::new();
        let done = match after {
            None => {
                let mut iter = map.keys();
                for key in iter.by_ref().take(limit) {
                    keys.push(key.dupe());
                }
                iter.next().is_none()
            }
            Some(after) => {
                let mut iter = map
                    .range((Bound::Excluded(after), Bound::Unbounded))
                    .map(|(key, _)| key);
                for key in iter.by_ref().take(limit) {
                    keys.push(key.dupe());
                }
                iter.next().is_none()
            }
        };
        (keys, done)
    }

    fn values(&self) -> Vec<V> {
        self.shards
            .iter()
            .flat_map(|shard| {
                shard
                    .read()
                    .values()
                    .map(|value| value.dupe())
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn iter_unordered(&self) -> Vec<(K, V)>
    where
        K: Dupe,
    {
        self.shards
            .iter()
            .flat_map(|shard| {
                shard
                    .read()
                    .iter()
                    .map(|(key, value)| (key.dupe(), value.dupe()))
                    .collect::<Vec<_>>()
            })
            .collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GcPhase {
    Idle,
    Mark,
    Sweep,
}

#[derive(Debug)]
struct GcState {
    phase: GcPhase,
    files: Vec<FileKey>,
    haste_modules: Vec<HasteModuleInfo>,
    free_files: Vec<FileKey>,
    free_haste_modules: Vec<HasteModuleInfo>,
    mark_file_shard: usize,
    mark_file_cursor: Option<FileKey>,
    mark_haste_shard: usize,
    mark_haste_cursor: Option<HasteModuleInfo>,
    sweep_file_index: usize,
    sweep_haste_index: usize,
    new_alloc_size: usize,
    free_size: usize,
}

impl Default for GcState {
    fn default() -> Self {
        Self {
            phase: GcPhase::Idle,
            files: Vec::new(),
            haste_modules: Vec::new(),
            free_files: Vec::new(),
            free_haste_modules: Vec::new(),
            mark_file_shard: 0,
            mark_file_cursor: None,
            mark_haste_shard: 0,
            mark_haste_cursor: None,
            sweep_file_index: 0,
            sweep_haste_index: 0,
            new_alloc_size: 0,
            free_size: 0,
        }
    }
}

impl SharedMem {
    pub fn new() -> Self {
        Self::new_with_config(None, None)
    }

    pub fn new_with_config(
        configured_heap_size: Option<u64>,
        configured_hash_table_pow: Option<u32>,
    ) -> Self {
        Self {
            file_heap: GcMap::new(),
            haste_module_heap: GcMap::new(),
            entity_transaction: EntityTransaction::new(),
            reader_cache: ReaderCache::new(),
            configured_heap_size,
            configured_hash_table_pow,
            on_compact: RwLock::new(None),
            gc_state: Mutex::new(GcState::default()),
        }
    }

    pub fn hash_stats(&self) -> HashStats {
        let file_count = self.file_heap.len() as i32;
        let haste_count = self.haste_module_heap.len() as i32;
        let used_slots = file_count + haste_count;
        let slots = self
            .configured_hash_table_pow
            .and_then(|pow| 1_i32.checked_shl(pow))
            .unwrap_or(used_slots);
        HashStats {
            nonempty_slots: used_slots,
            used_slots,
            slots,
        }
    }

    pub fn heap_size(&self) -> i32 {
        (self.file_heap.len() + self.haste_module_heap.len()) as i32
    }

    pub fn configured_heap_size(&self) -> Option<u64> {
        self.configured_heap_size
    }

    pub fn set_on_compact(
        &self,
        on_compact: Arc<dyn Fn() -> Box<dyn FnOnce() + Send> + Send + Sync>,
    ) {
        let mut callback = self.on_compact.write().unwrap();
        *callback = Some(on_compact);
    }

    pub fn clear_reader_cache(&self) {
        self.reader_cache.clear();
    }
    pub fn get_haste_info(&self, file: &FileKey) -> Option<HasteModuleInfo> {
        self.file_heap
            .get(file)
            .and_then(|entry| entry.haste_info.read_latest_clone())
    }

    pub fn get_haste_info_committed(&self, file: &FileKey) -> Option<HasteModuleInfo> {
        self.file_heap
            .get(file)
            .and_then(|entry| entry.get_haste_info_committed())
    }

    pub fn get_haste_module_info(&self, file: &FileKey) -> Option<HasteModuleInfo> {
        self.get_haste_info(file)
    }

    pub fn get_haste_module(&self, info: &HasteModuleInfo) -> Option<HasteModule> {
        self.haste_module_heap.get(info).map(|m| m.dupe())
    }

    pub fn get_haste_module_unsafe(&self, info: &HasteModuleInfo) -> HasteModule {
        self.get_haste_module(info)
            .unwrap_or_else(|| panic!("Haste module not found: {:?}", info))
    }

    pub fn get_dependency(&self, modulename: &Modulename) -> Option<Dependency> {
        match modulename {
            Modulename::Haste(haste_module_info) => self
                .get_haste_module(haste_module_info)
                .map(|_| Dependency::HasteModule(modulename.clone())),
            Modulename::Filename(file_key) => self
                .file_heap
                .get(file_key)
                .map(|_| Dependency::File(file_key.clone())),
        }
    }

    pub fn get_dependency_unsafe(&self, modulename: &Modulename) -> Dependency {
        match modulename {
            Modulename::Haste(haste_module_info) => {
                if self.get_haste_module(haste_module_info).is_some() {
                    Dependency::HasteModule(modulename.clone())
                } else {
                    panic!("Haste module not found: {:?}", haste_module_info)
                }
            }
            Modulename::Filename(file_key) => {
                if self.file_heap.get(file_key).is_some() {
                    Dependency::File(file_key.clone())
                } else {
                    panic!("File not found: {}", file_key.as_str())
                }
            }
        }
    }

    pub fn get_provider(&self, dependency: &Dependency) -> Option<FileKey> {
        match dependency {
            Dependency::HasteModule(Modulename::Haste(haste_info)) => self
                .get_haste_module(haste_info)
                .and_then(|module| module.get_provider()),
            Dependency::HasteModule(Modulename::Filename(_)) => None,
            Dependency::File(file_key) => {
                if let Some(file_entry) = self.file_heap.get(file_key) {
                    if let Some(alternate) = file_entry.get_alternate_file() {
                        if self.get_parse(&alternate).is_some() {
                            return Some(alternate);
                        }
                    }
                    if self.get_parse(file_key).is_some() {
                        Some(file_key.dupe())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn get_provider_committed(&self, dependency: &Dependency) -> Option<FileKey> {
        match dependency {
            Dependency::HasteModule(Modulename::Haste(haste_info)) => self
                .get_haste_module(haste_info)
                .and_then(|module| module.get_provider_committed()),
            Dependency::HasteModule(Modulename::Filename(_)) => None,
            Dependency::File(file_key) => {
                if let Some(file_entry) = self.file_heap.get(file_key) {
                    if let Some(alternate) = file_entry.get_alternate_file() {
                        if self.get_parse_committed(&alternate).is_some() {
                            return Some(alternate);
                        }
                    }
                    if self.get_parse_committed(file_key).is_some() {
                        Some(file_key.dupe())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn get_parse(&self, file: &FileKey) -> Option<Parse> {
        self.file_heap
            .get(file)
            .and_then(|entry| entry.parse_latest())
    }

    pub fn get_parse_committed(&self, file: &FileKey) -> Option<Parse> {
        self.file_heap
            .get(file)
            .and_then(|entry| entry.parse_committed())
    }

    pub fn get_typed_parse(&self, file: &FileKey) -> Option<TypedParse> {
        self.get_parse(file).and_then(|p| match p {
            Parse::Typed(t) => Some(t),
            _ => None,
        })
    }

    pub fn get_typed_parse_committed(&self, file: &FileKey) -> Option<TypedParse> {
        self.get_parse_committed(file).and_then(|p| match p {
            Parse::Typed(t) => Some(t),
            _ => None,
        })
    }

    pub fn get_package_parse(&self, file: &FileKey) -> Option<PackageParse> {
        self.get_parse(file).and_then(|p| match p {
            Parse::Package(pkg) => Some(pkg),
            _ => None,
        })
    }

    pub fn is_typed_file(&self, file: &FileKey) -> bool {
        self.get_parse(file).is_some_and(|p| p.is_typed())
    }

    pub fn is_package_file(&self, file: &FileKey) -> bool {
        self.get_parse(file).is_some_and(|p| p.is_package())
    }

    pub fn get_leader(&self, file: &FileKey) -> Option<FileKey> {
        self.get_typed_parse(file)
            .and_then(|typed| typed.leader.read_latest())
    }

    pub fn has_ast(&self, file: &FileKey) -> bool {
        self.get_typed_parse(file).is_some_and(|t| t.ast.is_some())
    }

    pub fn get_parse_unsafe(&self, file: &FileKey) -> Parse {
        self.get_parse(file)
            .unwrap_or_else(|| panic!("Parse not found for file: {}", file.as_str()))
    }

    pub fn get_typed_parse_unsafe(&self, file: &FileKey) -> TypedParse {
        self.get_typed_parse(file)
            .unwrap_or_else(|| panic!("Typed parse not found for file: {}", file.as_str()))
    }

    pub fn get_package_parse_unsafe(&self, file: &FileKey) -> PackageParse {
        self.get_package_parse(file)
            .unwrap_or_else(|| panic!("Package parse not found for file: {}", file.as_str()))
    }

    pub fn get_package_info(&self, file: &FileKey) -> Option<Arc<PackageJson>> {
        self.get_package_parse(file)
            .map(|pkg| pkg.package_info.dupe())
    }

    pub fn get_package_info_unsafe(&self, file: &FileKey) -> Arc<PackageJson> {
        let pkg = self.get_package_parse_unsafe(file);
        pkg.package_info.dupe()
    }

    pub fn get_file_hash_unsafe(&self, file: &FileKey) -> u64 {
        self.get_parse_unsafe(file).get_file_hash()
    }

    pub fn get_file_hash(&self, file: &FileKey) -> Option<u64> {
        self.get_parse(file).map(|p| p.get_file_hash())
    }

    pub fn get_ast_unsafe(&self, file: &FileKey) -> Arc<Program<Loc, Loc>> {
        if let Some(cached) = self.reader_cache.get_ast(file) {
            return cached;
        }
        let typed = self.get_typed_parse_unsafe(file);
        typed.ast_unsafe(file)
    }

    pub fn get_ast(&self, file: &FileKey) -> Option<Arc<Program<Loc, Loc>>> {
        if let Some(cached) = self.reader_cache.get_ast(file) {
            return Some(cached);
        }
        self.get_typed_parse(file)
            .and_then(|typed| match &typed.ast {
                Some(bytes) => {
                    let ast = flow_heap_serialization::deserialize_ast(file, bytes);
                    self.reader_cache.add_ast(file.dupe(), ast.dupe());
                    Some(ast)
                }
                None => None,
            })
    }

    pub fn get_docblock(&self, file: &FileKey) -> Option<Arc<Docblock>> {
        self.get_typed_parse(file).and_then(|typed| {
            typed
                .docblock
                .as_ref()
                .map(|bytes| flow_heap_serialization::deserialize_docblock(file, bytes))
        })
    }

    pub fn get_docblock_unsafe(&self, file: &FileKey) -> Arc<Docblock> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.docblock_unsafe(file)
    }

    pub fn get_aloc_table_unsafe(&self, file: &FileKey) -> Arc<PackedALocTable> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.aloc_table_unsafe(file)
    }

    pub fn get_aloc_table(&self, file: &FileKey) -> Option<Arc<PackedALocTable>> {
        self.get_typed_parse(file).and_then(|t| {
            t.aloc_table
                .as_ref()
                .map(|bytes| flow_heap_serialization::deserialize_aloc_table(bytes))
        })
    }

    pub fn loc_of_aloc(&self, aloc: &ALoc) -> Loc {
        let source = match aloc.source() {
            Some(s) => s.dupe(),
            None => return aloc.to_loc_exn().dupe(),
        };
        match self.get_aloc_table(&source) {
            Some(packed) => {
                let lazy_table: LazyALocTable = Rc::new(LazyCell::new(Box::new(move || {
                    Rc::new(ALocTable::unpack(source, &packed))
                })
                    as Box<dyn FnOnce() -> Rc<ALocTable>>));
                aloc.to_loc(&lazy_table)
            }
            None => aloc.to_loc_exn().dupe(),
        }
    }

    pub fn get_type_sig(&self, file: &FileKey) -> Option<Arc<TypeSigModule<Loc>>> {
        self.get_typed_parse(file).and_then(|typed| {
            typed
                .type_sig
                .as_ref()
                .map(|bytes| flow_heap_serialization::deserialize_type_sig(file, bytes))
        })
    }

    pub fn get_exports(&self, file: &FileKey) -> Option<Arc<Exports>> {
        self.get_typed_parse(file)
            .map(|typed| flow_heap_serialization::deserialize_exports(&typed.exports))
    }

    pub fn get_imports(&self, file: &FileKey) -> Option<Arc<Imports>> {
        self.get_typed_parse(file)
            .map(|typed| flow_heap_serialization::deserialize_imports(&typed.imports))
    }

    pub fn get_tolerable_file_sig(
        &self,
        file: &FileKey,
    ) -> Option<(Arc<FileSig>, Arc<[TolerableError<Loc>]>)> {
        self.get_typed_parse(file).and_then(|typed| {
            typed
                .file_sig
                .as_ref()
                .map(|bytes| flow_heap_serialization::deserialize_file_sig_with_errors(file, bytes))
        })
    }

    pub fn get_file_sig(&self, file: &FileKey) -> Option<Arc<FileSig>> {
        self.get_tolerable_file_sig(file).map(|(sig, _)| sig)
    }

    pub fn get_type_sig_unsafe(&self, file: &FileKey) -> Arc<TypeSigModule<Loc>> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.type_sig_unsafe(file)
    }

    pub fn get_exports_unsafe(&self, file: &FileKey) -> Arc<Exports> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.exports_unsafe()
    }

    pub fn get_imports_unsafe(&self, file: &FileKey) -> Arc<Imports> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.imports_unsafe()
    }

    pub fn get_tolerable_file_sig_unsafe(
        &self,
        file: &FileKey,
    ) -> (Arc<FileSig>, Arc<[TolerableError<Loc>]>) {
        let typed = self.get_typed_parse_unsafe(file);
        typed.tolerable_file_sig_unsafe(file)
    }

    pub fn get_file_sig_unsafe(&self, file: &FileKey) -> Arc<FileSig> {
        self.get_tolerable_file_sig_unsafe(file).0
    }

    pub fn get_requires_unsafe(&self, file: &FileKey) -> Arc<[FlowImportSpecifier]> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.requires.dupe()
    }

    pub fn get_requires(&self, file: &FileKey) -> Option<Arc<[FlowImportSpecifier]>> {
        self.get_typed_parse(file)
            .map(|typed| typed.requires.dupe())
    }

    pub fn get_resolved_requires_unsafe(&self, file: &FileKey) -> ResolvedRequires {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .resolved_requires
            .read_latest_clone()
            .expect("ResolvedRequires should be set")
    }

    pub fn get_resolved_modules_unsafe(
        &self,
        file: &FileKey,
    ) -> BTreeMap<FlowImportSpecifier, Result<Dependency, Option<FlowImportSpecifier>>> {
        let typed = self.get_typed_parse_unsafe(file);
        let requires = &typed.requires;
        let resolved_requires = typed.resolved_requires_unsafe();
        let resolved_modules = resolved_requires.get_resolved_modules();
        requires
            .iter()
            .zip(resolved_modules.iter())
            .map(|(req, module)| (req.dupe(), module.to_result()))
            .collect()
    }

    pub fn get_leader_unsafe(&self, file: &FileKey) -> FileKey {
        let typed = self.get_typed_parse_unsafe(file);
        typed.leader.get()
    }

    pub fn iter_dependents<F>(&self, f: &mut F, modulename: &Modulename)
    where
        F: FnMut(&FileKey),
    {
        let dependents = match modulename {
            Modulename::Haste(haste_info) => self
                .get_haste_module(haste_info)
                .map(|module| module.get_dependents()),
            Modulename::Filename(file_key) => self
                .file_heap
                .get(file_key)
                .and_then(|entry| entry.get_dependents()),
        };

        if let Some(deps) = dependents {
            for file in deps {
                f(&file);
            }
        }
    }

    pub fn get_file_hash_committed(&self, file: &FileKey) -> Option<u64> {
        self.get_parse_committed(file).map(|p| p.get_file_hash())
    }

    pub fn get_exports_committed(&self, file: &FileKey) -> Option<Arc<Exports>> {
        self.get_typed_parse_committed(file)
            .map(|typed| flow_heap_serialization::deserialize_exports(&typed.exports))
    }

    pub fn get_imports_committed(&self, file: &FileKey) -> Option<Arc<Imports>> {
        self.get_typed_parse_committed(file)
            .map(|typed| flow_heap_serialization::deserialize_imports(&typed.imports))
    }

    pub fn get_resolved_requires_committed_unsafe(&self, file: &FileKey) -> ResolvedRequires {
        let typed = self.get_typed_parse_committed(file).unwrap_or_else(|| {
            panic!(
                "Committed typed parse not found for file: {}",
                file.as_str()
            )
        });
        typed
            .resolved_requires
            .read_committed_clone()
            .unwrap_or_else(|| {
                panic!(
                    "Committed resolved requires not found for file: {}",
                    file.as_str()
                )
            })
    }

    pub fn get_resolved_modules_committed_unsafe(
        &self,
        file: &FileKey,
    ) -> BTreeMap<FlowImportSpecifier, Result<Dependency, Option<FlowImportSpecifier>>> {
        let typed = self.get_typed_parse_committed(file).unwrap_or_else(|| {
            panic!(
                "Committed typed parse not found for file: {}",
                file.as_str()
            )
        });
        let resolved_requires = typed
            .resolved_requires
            .read_committed_clone()
            .unwrap_or_else(|| {
                panic!(
                    "Committed resolved requires not found for file: {}",
                    file.as_str()
                )
            });
        let requires = &typed.requires;
        let resolved_modules = resolved_requires.get_resolved_modules();
        requires
            .iter()
            .zip(resolved_modules.iter())
            .map(|(req, module)| (req.dupe(), module.to_result()))
            .collect()
    }

    // We choose the head file as the leader, and the tail as followers.
    // It is always OK to choose the head as leader, as explained below.
    // Note that cycles cannot happen between untyped files.
    // Why? Because files in cycles must have their dependencies recorded,
    // yet dependencies are never recorded for untyped files.
    // It follows that when the head is untyped, there are no other files.
    // We don't have to worry that some other file may be typed when the head is untyped.
    // It also follows when the head is typed, the tail must be typed too.
    pub fn typed_component(
        &self,
        leader_key: &FileKey,
        rest: &[FileKey],
    ) -> Option<Vec<(FileKey, TypedParse)>> {
        let leader_parse = self.get_typed_parse(leader_key)?;
        let mut component = Vec::with_capacity(1 + rest.len());
        component.push((leader_key.dupe(), leader_parse));
        for key in rest {
            let parse = self.get_typed_parse_unsafe(key);
            component.push((key.dupe(), parse));
        }
        Some(component)
    }

    pub fn file_has_changed(&self, file: &FileKey) -> bool {
        self.file_heap
            .get(file)
            .is_some_and(|entry| entry.parse.has_changed())
    }

    pub fn get_alternate_file(&self, file: &FileKey) -> Option<FileKey> {
        self.file_heap
            .get(file)
            .and_then(|entry| entry.get_alternate_file())
    }

    pub fn set_alternate_file(&self, file: &FileKey, alternate: FileKey) {
        if let Some(entry) = self.file_heap.get(file) {
            entry.set_alternate_file(Some(alternate));
        }
    }

    pub fn get_or_create_haste_module(&self, info: HasteModuleInfo) -> HasteModule {
        let (module, inserted) = self.haste_module_heap.ensure(&info, || {
            HasteModule::new(self.entity_transaction.dupe(), info.clone())
        });
        if inserted {
            self.note_alloc();
        }
        module.dupe()
    }

    fn calc_dirty_modules(
        &self,
        file_key: &FileKey,
        file_entry: &FileEntry,
    ) -> BTreeSet<Modulename> {
        let haste_ent = file_entry.haste_info_entity();
        let new_info = haste_ent.read_latest_clone();

        let (old_haste_info, new_haste_info, changed_haste_info) = if haste_ent.has_changed() {
            let old_info = haste_ent.read_committed_clone();
            (old_info, new_info, None)
        } else {
            (None, None, new_info)
        };

        let mut dirty_modules = BTreeSet::new();

        if let Some(info) = old_haste_info {
            let module = self.get_or_create_haste_module(info.clone());
            module.remove_provider(file_key);
            dirty_modules.insert(Modulename::Haste(info));
        }

        if let Some(info) = new_haste_info.clone() {
            let module = self.get_or_create_haste_module(info.clone());
            module.add_provider(file_key.dupe());
            dirty_modules.insert(Modulename::Haste(info));
        }

        if let Some(info) = changed_haste_info {
            dirty_modules.insert(Modulename::Haste(info));
        }

        dirty_modules.insert(Modulename::Filename(files::chop_flow_ext(file_key)));

        dirty_modules
    }

    fn handle_flow_ext(&self, file: &FileKey) -> BTreeSet<Modulename> {
        if !files::has_declaration_ext(file) {
            return BTreeSet::new();
        }
        let impl_key = files::chop_declaration_ext(file);
        if self.file_heap.get(&impl_key).is_none() {
            self.file_heap.insert(
                impl_key.dupe(),
                FileEntry::new_phantom(self.entity_transaction.dupe()),
            );
        }
        self.set_alternate_file(&impl_key, file.dupe());
        BTreeSet::new()
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_parsed(
        &self,
        file: FileKey,
        file_hash: u64,
        haste_module_info: Option<HasteModuleInfo>,
        ast: Option<Arc<Program<Loc, Loc>>>,
        docblock: Option<Arc<Docblock>>,
        aloc_table: Option<Arc<PackedALocTable>>,
        type_sig: Option<Arc<TypeSigModule<Loc>>>,
        file_sig: Option<(Arc<FileSig>, Arc<[TolerableError<Loc>]>)>,
        exports: Arc<Exports>,
        requires: Arc<[FlowImportSpecifier]>,
        imports: Arc<Imports>,
    ) -> BTreeSet<Modulename> {
        let has_dependents = !file.as_str().ends_with(".flow");

        let mut dirty_modules = if let Some(existing_entry) = self.file_heap.get(&file) {
            let existing_typed = existing_entry.parse_latest().and_then(|p| match p {
                Parse::Typed(t) => Some(t),
                _ => None,
            });

            let (resolved_requires, leader, sig_hash) = match existing_typed {
                Some(ref existing) => (
                    existing.resolved_requires.clone(),
                    existing.leader.clone(),
                    existing.sig_hash.clone(),
                ),
                None => (
                    Arc::new(crate::entity::Entity::new(
                        self.entity_transaction.dupe(),
                        crate::entity::ResolvedRequires::new(vec![], vec![]),
                    )),
                    Arc::new(crate::entity::Entity::empty(self.entity_transaction.dupe())),
                    Arc::new(crate::entity::Entity::empty(self.entity_transaction.dupe())),
                ),
            };

            let typed_parse = TypedParse::new(
                file_hash,
                ast,
                docblock,
                aloc_table,
                type_sig,
                file_sig,
                exports,
                requires,
                resolved_requires,
                imports,
                leader,
                sig_hash,
            );

            existing_entry.parse().set(Parse::Typed(typed_parse));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, &existing_entry)
        } else {
            let typed_parse = TypedParse::new(
                file_hash,
                ast,
                docblock,
                aloc_table,
                type_sig,
                file_sig,
                exports,
                requires,
                Arc::new(crate::entity::Entity::new(
                    self.entity_transaction.dupe(),
                    crate::entity::ResolvedRequires::new(vec![], vec![]),
                )),
                imports,
                Arc::new(crate::entity::Entity::empty(self.entity_transaction.dupe())),
                Arc::new(crate::entity::Entity::empty(self.entity_transaction.dupe())),
            );

            let file_entry = FileEntry::new(
                self.entity_transaction.dupe(),
                Parse::Typed(typed_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => {
                    self.note_alloc();
                    self.calc_dirty_modules(&file, &file_entry)
                }
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "GcMap::insert returned Some(_) but get(&file) is None; \
                         insert and get are guarded by the same map lock",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Typed(typed_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, &in_map_entry)
                        }
                        // Two threads raced to add this file and the other thread
                        // won. We don't need to mark any files as dirty; the other
                        // thread will have done that for us. *)
                        Some(_) => BTreeSet::new(),
                    }
                }
            }
        };

        dirty_modules.extend(self.handle_flow_ext(&file));
        dirty_modules
    }

    pub fn add_unparsed(
        &self,
        file: FileKey,
        file_hash: u64,
        haste_module_info: Option<HasteModuleInfo>,
    ) -> BTreeSet<Modulename> {
        use crate::parse::UntypedParse;
        let has_dependents = !file.as_str().ends_with(".flow");

        let mut dirty_modules = if let Some(existing_entry) = self.file_heap.get(&file) {
            if let Some(Parse::Typed(old_typed)) = existing_entry.parse_latest() {
                if let Some(old_rr) = old_typed.resolved_requires.read_latest_clone() {
                    let old_deps = old_rr.all_dependencies();
                    for dep in &old_deps {
                        self.remove_dependent_from(&file, dep);
                    }
                }
            }
            existing_entry
                .parse()
                .set(Parse::Untyped(UntypedParse::new(file_hash)));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, &existing_entry)
        } else {
            let untyped_parse = UntypedParse::new(file_hash);
            let file_entry = FileEntry::new(
                self.entity_transaction.dupe(),
                Parse::Untyped(untyped_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => {
                    self.note_alloc();
                    self.calc_dirty_modules(&file, &file_entry)
                }
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "GcMap::insert returned Some(_) but get(&file) is None; \
                         insert and get are guarded by the same map lock",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Untyped(untyped_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, &in_map_entry)
                        }
                        Some(_) => BTreeSet::new(),
                    }
                }
            }
        };

        dirty_modules.extend(self.handle_flow_ext(&file));
        dirty_modules
    }

    // If this file used to exist, but no longer does, then it was deleted.
    // Record the deletion by clearing parse information.
    // Deletion might also require re-picking module providers, so we return dirty modules.
    pub fn clear_file(
        &self,
        file_key: FileKey,
        haste_module_info: Option<HasteModuleInfo>,
    ) -> BTreeSet<Modulename> {
        if let Some(existing_entry) = self.file_heap.get(&file_key) {
            if let Some(Parse::Typed(old_typed)) = existing_entry.parse_latest() {
                if let Some(old_rr) = old_typed.resolved_requires.read_latest_clone() {
                    let old_deps = old_rr.all_dependencies();
                    for dep in &old_deps {
                        self.remove_dependent_from(&file_key, dep);
                    }
                }
            }
            existing_entry.parse().advance(None);
            let mut dirty_modules = BTreeSet::new();
            dirty_modules.insert(Modulename::Filename(files::chop_flow_ext(&file_key)));
            if let Some(haste_info) = existing_entry.haste_info_entity().read_latest_clone() {
                let module = self.get_or_create_haste_module(haste_info.clone());
                module.remove_provider(&file_key);
                existing_entry.haste_info_entity().advance(None);
                dirty_modules.insert(Modulename::Haste(haste_info));
            }
            dirty_modules
        } else {
            match haste_module_info {
                None => BTreeSet::new(),
                Some(haste_module_info) => {
                    let _m = self.get_or_create_haste_module(haste_module_info.clone());
                    let mut dirty_modules = BTreeSet::new();
                    dirty_modules.insert(Modulename::Haste(haste_module_info));
                    dirty_modules
                }
            }
        }
    }

    pub fn add_package(
        &self,
        file: FileKey,
        file_hash: u64,
        haste_module_info: Option<HasteModuleInfo>,
        package_info: Arc<PackageJson>,
    ) -> BTreeSet<Modulename> {
        use crate::parse::PackageParse;
        let has_dependents = true;

        if let Some(existing_entry) = self.file_heap.get(&file) {
            existing_entry
                .parse()
                .set(Parse::Package(PackageParse::new(file_hash, package_info)));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, &existing_entry)
        } else {
            let package_parse = PackageParse::new(file_hash, package_info);
            let file_entry = FileEntry::new(
                self.entity_transaction.dupe(),
                Parse::Package(package_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => {
                    self.note_alloc();
                    self.calc_dirty_modules(&file, &file_entry)
                }
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "GcMap::insert returned Some(_) but get(&file) is None; \
                         insert and get are guarded by the same map lock",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Package(package_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, &in_map_entry)
                        }
                        Some(_) => BTreeSet::new(),
                    }
                }
            }
        }
    }

    // Given a file, it's old resolved requires, and new resolved requires,
    // compute the changes necessary to update the reverse dependency graph.
    pub fn set_resolved_requires(
        &self,
        file: &FileKey,
        resolved_requires: crate::entity::ResolvedRequires,
    ) {
        if let Some(entry) = self.file_heap.get(file) {
            if let Some(Parse::Typed(typed)) = entry.parse_latest() {
                let old_deps = typed
                    .resolved_requires
                    .read_latest_clone()
                    .map(|rr| rr.all_dependencies())
                    .unwrap_or_default();

                let new_deps = resolved_requires.all_dependencies();

                typed.resolved_requires.set(resolved_requires);

                let mut new_alloc_size = 0;
                for dep in &old_deps {
                    if !new_deps.contains(dep) {
                        self.remove_dependent_from(file, dep);
                    }
                }

                for dep in &new_deps {
                    if !old_deps.contains(dep) {
                        new_alloc_size += self.add_dependent_to(file, dep);
                    }
                }
                self.note_alloc_many(new_alloc_size);
            }
        }
    }

    fn remove_dependent_from(&self, file: &FileKey, dep: &Dependency) {
        match dep {
            Dependency::HasteModule(Modulename::Haste(haste_info)) => {
                if let Some(module) = self.get_haste_module(haste_info) {
                    module.remove_dependent(file);
                }
            }
            Dependency::HasteModule(Modulename::Filename(dep_file))
            | Dependency::File(dep_file) => {
                if let Some(dep_entry) = self.file_heap.get(dep_file) {
                    dep_entry.remove_dependent(file);
                }
            }
        }
    }

    fn add_dependent_to(&self, file: &FileKey, dep: &Dependency) -> usize {
        match dep {
            Dependency::HasteModule(Modulename::Haste(haste_info)) => {
                let module = self.get_or_create_haste_module(haste_info.dupe());
                module.add_dependent(file.dupe());
                0
            }
            Dependency::HasteModule(Modulename::Filename(dep_file))
            | Dependency::File(dep_file) => {
                let (dep_entry, inserted) = self.file_heap.ensure(dep_file, || {
                    FileEntry::new_phantom(self.entity_transaction.dupe())
                });
                dep_entry.add_dependent(file.dupe());
                usize::from(inserted)
            }
        }
    }

    fn rollback_resolved_requires(
        &self,
        file: &FileKey,
        ent: &Arc<crate::entity::Entity<ResolvedRequires>>,
    ) {
        let old_resolved_requires = ent.read_committed_clone();
        let new_resolved_requires = ent.read_latest_clone();
        let old_dependencies = old_resolved_requires
            .as_ref()
            .map(ResolvedRequires::all_dependencies)
            .unwrap_or_default();
        let new_dependencies = new_resolved_requires
            .as_ref()
            .map(ResolvedRequires::all_dependencies)
            .unwrap_or_default();
        for dep in &new_dependencies {
            if !old_dependencies.contains(dep) {
                self.remove_dependent_from(file, dep);
            }
        }
        let mut new_alloc_size = 0;
        for dep in &old_dependencies {
            if !new_dependencies.contains(dep) {
                new_alloc_size += self.add_dependent_to(file, dep);
            }
        }
        self.note_alloc_many(new_alloc_size);
        ent.rollback();
    }

    fn rollback_leader(&self, parse: &TypedParse) {
        parse.leader.rollback();
        parse.sig_hash.rollback();
    }

    fn rollback_file(&self, file_key: &FileKey, file: &FileEntry) {
        let old_typed_parse = file.parse_committed().and_then(|parse| match parse {
            Parse::Typed(typed) => Some(typed),
            _ => None,
        });
        let new_typed_parse = file.parse_latest().and_then(|parse| match parse {
            Parse::Typed(typed) => Some(typed),
            _ => None,
        });
        let haste_ent = file.haste_info_entity();
        let (old_haste_module, new_haste_module) = if haste_ent.has_changed() {
            let old_info = haste_ent.read_committed_clone();
            let new_info = haste_ent.read_latest_clone();
            (
                old_info
                    .as_ref()
                    .and_then(|info| self.get_haste_module(info)),
                new_info
                    .as_ref()
                    .and_then(|info| self.get_haste_module(info)),
            )
        } else {
            (None, None)
        };

        match (old_typed_parse, new_typed_parse) {
            (None, None) => {}
            (Some(old_parse), None) => {
                let old_resolved_requires = old_parse.resolved_requires.read_latest_clone();
                let old_dependencies = old_resolved_requires
                    .as_ref()
                    .map(ResolvedRequires::all_dependencies)
                    .unwrap_or_default();
                let mut new_alloc_size = 0;
                for dep in &old_dependencies {
                    new_alloc_size += self.add_dependent_to(file_key, dep);
                }
                self.note_alloc_many(new_alloc_size);
            }
            (None, Some(new_parse)) => {
                let new_resolved_requires = new_parse.resolved_requires.read_latest_clone();
                let new_dependencies = new_resolved_requires
                    .as_ref()
                    .map(ResolvedRequires::all_dependencies)
                    .unwrap_or_default();
                for dep in &new_dependencies {
                    self.remove_dependent_from(file_key, dep);
                }
            }
            (Some(_), Some(new_parse)) => {
                self.rollback_resolved_requires(file_key, &new_parse.resolved_requires);
                self.rollback_leader(&new_parse);
            }
        }
        if let Some(module) = &old_haste_module {
            module.rollback_provider();
        }
        if let Some(module) = &new_haste_module {
            module.rollback_provider();
            module.remove_provider(file_key);
        }
        file.parse().rollback();
        haste_ent.rollback();
        if let Some(module) = old_haste_module {
            module.add_provider(file_key.dupe());
        }
    }

    fn note_alloc(&self) {
        self.note_alloc_many(1);
    }

    fn note_alloc_many(&self, count: usize) {
        if count == 0 {
            return;
        }

        let mut gc_state = self.gc_state.lock();
        gc_state.new_alloc_size = gc_state.new_alloc_size.saturating_add(count);
    }

    // GC will attempt to keep the overhead of garbage to no more than 20%. Before
    // we actually mark and sweep, however, we don't know how much garbage there is,
    // so we estimate.
    //
    // To estimate the amount of garbage, we consider all "new" allocations --
    // allocations since the previous mark+sweep -- to be garbage. We add that
    // number to the known free space. If that is at least 20% of the total space,
    // we will kick of a new mark and sweep pass.
    fn should_collect(&self, gc_state: &GcState) -> bool {
        let estimated_garbage = gc_state.free_size.saturating_add(gc_state.new_alloc_size);
        estimated_garbage.saturating_mul(5) >= self.heap_size().max(0) as usize
    }

    // After a full mark and sweep, we want to compact the heap if the amount of
    // free space is 20% of the scanned heap.
    fn should_compact(&self, gc_state: &GcState) -> bool {
        let scanned_size =
            (self.heap_size().max(0) as usize).saturating_sub(gc_state.new_alloc_size);
        gc_state.free_size.saturating_mul(5) >= scanned_size
    }

    fn start_cycle(&self, gc_state: &mut GcState) {
        gc_state.files.clear();
        gc_state.haste_modules.clear();
        gc_state.free_files.clear();
        gc_state.free_haste_modules.clear();
        gc_state.mark_file_shard = 0;
        gc_state.mark_file_cursor = None;
        gc_state.mark_haste_shard = 0;
        gc_state.mark_haste_cursor = None;
        gc_state.sweep_file_index = 0;
        gc_state.sweep_haste_index = 0;
        gc_state.new_alloc_size = 0;
        gc_state.free_size = 0;
        gc_state.phase = GcPhase::Mark;
    }

    fn mark_slice(&self, gc_state: &mut GcState, work: usize) -> usize {
        // work := mark_slice !work
        let mut work = work;
        while work > 0 && gc_state.mark_file_shard < GC_MAP_SHARDS {
            let (keys, done) = self.file_heap.keys_after(
                gc_state.mark_file_shard,
                gc_state.mark_file_cursor.as_ref(),
                work,
            );
            if keys.is_empty() {
                gc_state.mark_file_shard += 1;
                gc_state.mark_file_cursor = None;
                work -= 1;
            } else {
                let used = keys.len();
                gc_state.mark_file_cursor = if done {
                    None
                } else {
                    keys.last().map(Dupe::dupe)
                };
                gc_state.files.extend(keys);
                if done {
                    gc_state.mark_file_shard += 1;
                }
                work -= used;
            }
        }
        while work > 0 && gc_state.mark_haste_shard < GC_MAP_SHARDS {
            let (haste_modules, done) = self.haste_module_heap.keys_after(
                gc_state.mark_haste_shard,
                gc_state.mark_haste_cursor.as_ref(),
                work,
            );
            if haste_modules.is_empty() {
                gc_state.mark_haste_shard += 1;
                gc_state.mark_haste_cursor = None;
                work -= 1;
            } else {
                let used = haste_modules.len();
                gc_state.mark_haste_cursor = if done {
                    None
                } else {
                    haste_modules.last().map(Dupe::dupe)
                };
                gc_state.haste_modules.extend(haste_modules);
                if done {
                    gc_state.mark_haste_shard += 1;
                }
                work -= used;
            }
        }
        if gc_state.mark_file_shard == GC_MAP_SHARDS && gc_state.mark_haste_shard == GC_MAP_SHARDS {
            gc_state.phase = GcPhase::Sweep;
        }
        work
    }

    fn file_entry_is_free(file_entry: &FileEntry) -> bool {
        file_entry.parse_latest().is_none()
            && file_entry.parse_committed().is_none()
            && file_entry.haste_info_entity().read_latest_clone().is_none()
            && file_entry
                .haste_info_entity()
                .read_committed_clone()
                .is_none()
            && !file_entry.has_dependents()
            && file_entry.get_alternate_file().is_none()
    }

    fn haste_module_is_free(haste_module: &HasteModule) -> bool {
        haste_module.get_provider().is_none()
            && haste_module.get_provider_committed().is_none()
            && !haste_module.has_dependents()
            && !haste_module.has_providers()
    }

    fn sweep_slice(&self, gc_state: &mut GcState, work: usize) -> usize {
        let mut work = work;
        while work > 0 && gc_state.sweep_file_index < gc_state.files.len() {
            let file = gc_state.files[gc_state.sweep_file_index].dupe();
            if let Some(file_entry) = self.file_heap.get(&file)
                && Self::file_entry_is_free(&file_entry)
            {
                gc_state.free_size = gc_state.free_size.saturating_add(1);
                gc_state.free_files.push(file.dupe());
            }
            gc_state.sweep_file_index += 1;
            work -= 1;
        }
        while work > 0 && gc_state.sweep_haste_index < gc_state.haste_modules.len() {
            let haste_info = gc_state.haste_modules[gc_state.sweep_haste_index].dupe();
            if let Some(haste_module) = self.haste_module_heap.get(&haste_info)
                && Self::haste_module_is_free(&haste_module)
            {
                gc_state.free_size = gc_state.free_size.saturating_add(1);
                gc_state.free_haste_modules.push(haste_info.dupe());
            }
            gc_state.sweep_haste_index += 1;
            work -= 1;
        }
        if gc_state.sweep_file_index == gc_state.files.len()
            && gc_state.sweep_haste_index == gc_state.haste_modules.len()
        {
            // Done sweeping, transition to idle.
            gc_state.phase = GcPhase::Idle;
            gc_state.files.clear();
            gc_state.haste_modules.clear();
            gc_state.sweep_file_index = 0;
            gc_state.sweep_haste_index = 0;
        }
        work
    }

    fn compact_helper(&self, free_files: Vec<FileKey>, free_haste_modules: Vec<HasteModuleInfo>) {
        let finish_compaction = self
            .on_compact
            .read()
            .unwrap()
            .as_ref()
            .map(|on_compact| on_compact());
        for file in free_files {
            self.file_heap.remove_if(&file, Self::file_entry_is_free);
        }
        for haste_info in free_haste_modules {
            self.haste_module_heap
                .remove_if(&haste_info, Self::haste_module_is_free);
        }
        self.clear_reader_cache();
        {
            let mut gc_state = self.gc_state.lock();
            gc_state.free_size = 0;
            gc_state.new_alloc_size = 0;
            gc_state.free_files.clear();
            gc_state.free_haste_modules.clear();
        }
        if let Some(finish_compaction) = finish_compaction {
            finish_compaction();
        }
    }

    // Perform an incremental "slice" of GC work. The caller can control the amount
    // of work performed by passing in a smaller or larger "work" budget. This
    // function returns `true` when the GC phase was completed, and `false` if there
    // is still more work to do.
    pub fn collect_slice(&self, work: usize) -> bool {
        self.collect_slice_with_force(false, work)
    }

    fn collect_slice_with_force(&self, force: bool, work: usize) -> bool {
        let mut work = work;
        while work > 0 {
            let mut gc_state = self.gc_state.lock();
            match gc_state.phase {
                GcPhase::Idle => {
                    if force || self.should_collect(&gc_state) {
                        self.start_cycle(&mut gc_state);
                    } else {
                        work = 0;
                    }
                }
                GcPhase::Mark => work = self.mark_slice(&mut gc_state, work),
                GcPhase::Sweep => {
                    Self::sweep_slice(self, &mut gc_state, work);
                    work = 0;
                }
            }
        }
        let (is_idle, should_compact, compact_files, compact_haste_modules) = {
            let mut gc_state = self.gc_state.lock();
            let is_idle = gc_state.phase == GcPhase::Idle;
            let should_compact = is_idle && self.should_compact(&gc_state);
            let compact_files = if should_compact {
                std::mem::take(&mut gc_state.free_files)
            } else {
                Vec::new()
            };
            let compact_haste_modules = if should_compact {
                std::mem::take(&mut gc_state.free_haste_modules)
            } else {
                Vec::new()
            };
            (
                is_idle,
                should_compact,
                compact_files,
                compact_haste_modules,
            )
        };
        // The GC will be in idle phase under two conditions: (1) we started in idle
        // and did not start a new collect cycle, or (2) we just finished a sweep. In
        // condition (1) should_compact should return false, so we will only possibly
        // compact in condition (2), assuming 20% of the scanned heap is free.
        if should_compact {
            self.compact_helper(compact_files, compact_haste_modules);
        }
        is_idle
    }

    // Perform a full GC pass, or complete an in-progress GC pass. This call
    // bypasses the `should_collect` heuristic and will instead always trigger a new
    // mark and sweep pass if the GC is currently idle.
    pub fn collect_full(&self) {
        while !self.collect_slice_with_force(true, usize::MAX) {}
    }

    fn finish_cycle(&self) {
        loop {
            let mut gc_state = self.gc_state.lock();
            if gc_state.phase != GcPhase::Mark {
                break;
            }
            self.mark_slice(&mut gc_state, usize::MAX);
        }
        loop {
            let mut gc_state = self.gc_state.lock();
            if gc_state.phase != GcPhase::Sweep {
                break;
            }
            Self::sweep_slice(self, &mut gc_state, usize::MAX);
        }
    }

    // Perform a full compaction of shared memory, such that no heap space is
    // wasted. We finish the current cycle, if one is in progress, then perform a
    // full mark and sweep pass before collecting. This ensures that any "floating
    // garbage" from a previous GC pass is also collected.
    pub fn compact(&self) {
        self.finish_cycle();
        {
            let mut gc_state = self.gc_state.lock();
            self.start_cycle(&mut gc_state);
        }
        self.finish_cycle();
        let (compact_files, compact_haste_modules) = {
            let mut gc_state = self.gc_state.lock();
            (
                std::mem::take(&mut gc_state.free_files),
                std::mem::take(&mut gc_state.free_haste_modules),
            )
        };
        self.compact_helper(compact_files, compact_haste_modules);
    }

    pub fn commit_entities(&self) {
        self.entity_transaction.commit();
    }

    pub fn rollback_entities(&self) {
        for (file_key, entry) in self.file_heap.iter_unordered() {
            self.rollback_file(&file_key, &entry);
        }
        for module in self.haste_module_heap.values() {
            module.rollback_provider();
        }
    }
}

#[cfg(test)]
mod tests {
    use dupe::Dupe;
    use flow_parser::file_key::FileKeyInner;

    use super::*;

    fn source_file(name: &str) -> FileKey {
        FileKey::new(FileKeyInner::SourceFile(name.to_string()))
    }

    #[test]
    fn compact_removes_committed_deleted_file() {
        let shared_mem = SharedMem::new();
        let file = source_file("a.js");

        shared_mem.add_unparsed(file.dupe(), 1, None);
        shared_mem.commit_entities();
        assert_eq!(shared_mem.heap_size(), 1);

        shared_mem.clear_file(file.dupe(), None);
        shared_mem.commit_entities();
        shared_mem.compact();

        assert_eq!(shared_mem.heap_size(), 0);
    }

    #[test]
    fn compact_revalidates_free_file_before_removing() {
        let shared_mem = SharedMem::new();
        let file = source_file("a.js");

        shared_mem.add_unparsed(file.dupe(), 1, None);
        shared_mem.commit_entities();
        shared_mem.clear_file(file.dupe(), None);
        shared_mem.commit_entities();

        {
            let mut gc_state = shared_mem.gc_state.lock();
            shared_mem.start_cycle(&mut gc_state);
            shared_mem.mark_slice(&mut gc_state, usize::MAX);
            SharedMem::sweep_slice(&shared_mem, &mut gc_state, usize::MAX);
        }

        shared_mem.add_unparsed(file.dupe(), 2, None);
        let (free_files, free_haste_modules) = {
            let mut gc_state = shared_mem.gc_state.lock();
            (
                std::mem::take(&mut gc_state.free_files),
                std::mem::take(&mut gc_state.free_haste_modules),
            )
        };
        shared_mem.compact_helper(free_files, free_haste_modules);

        assert!(matches!(
            shared_mem.get_parse(&file),
            Some(Parse::Untyped(_))
        ));
        assert_eq!(shared_mem.heap_size(), 1);
    }

    #[test]
    fn commit_entities_commits_the_current_transaction() {
        let shared_mem = SharedMem::new();
        let a = source_file("a.js");
        let b = source_file("b.js");

        shared_mem.add_unparsed(a.dupe(), 1, None);
        shared_mem.add_unparsed(b.dupe(), 1, None);
        shared_mem.commit_entities();

        shared_mem.add_unparsed(a.dupe(), 2, None);
        shared_mem.add_unparsed(b.dupe(), 2, None);
        shared_mem.commit_entities();

        assert_eq!(shared_mem.get_file_hash_committed(&a), Some(2));
        assert_eq!(shared_mem.get_file_hash_committed(&b), Some(2));

        shared_mem.rollback_entities();

        assert_eq!(shared_mem.get_file_hash(&a), Some(2));
        assert_eq!(shared_mem.get_file_hash(&b), Some(2));
    }

    #[test]
    fn collect_slice_uses_new_allocations_to_start_cycle() {
        let shared_mem = SharedMem::new();
        let file = source_file("a.js");

        shared_mem.add_unparsed(file, 1, None);

        assert!(!shared_mem.collect_slice(1));
        assert_eq!(shared_mem.gc_state.lock().phase, GcPhase::Mark);
        for _ in 0..(GC_MAP_SHARDS * 2) {
            if shared_mem.gc_state.lock().phase == GcPhase::Sweep {
                return;
            }
            assert!(!shared_mem.collect_slice(1));
        }
        panic!("GC should reach sweep phase after incremental mark slices");
    }
}

impl Default for SharedMem {
    fn default() -> Self {
        Self::new()
    }
}

pub mod merge_context_mutator {
    use super::*;

    fn update_leader(leader: Option<FileKey>, parse: &TypedParse) {
        parse.leader.advance(leader);
    }

    fn update_sig_hash(hash: Option<u64>, parse: &TypedParse) {
        parse.sig_hash.advance(hash);
    }

    fn add_sig_hash(for_find_all_refs: bool, parse: &TypedParse, sig_hash: u64) -> bool {
        let prev_sig_hash = parse.sig_hash.read_committed();

        match prev_sig_hash {
            Some(prev_hash) if prev_hash == sig_hash => false,
            _ => {
                if !for_find_all_refs {
                    parse.sig_hash.advance(Some(sig_hash));
                }
                true
            }
        }
    }

    pub fn add_merge_on_diff(
        for_find_all_refs: bool,
        component: &[(FileKey, TypedParse)],
        sig_hash: u64,
    ) -> bool {
        if component.is_empty() {
            return false;
        }
        let (leader_key, leader_parse) = &component[0];
        let rest = &component[1..];
        for (_, parse) in component.iter() {
            update_leader(Some(leader_key.dupe()), parse);
        }
        let diff = add_sig_hash(for_find_all_refs, leader_parse, sig_hash);
        if diff && !for_find_all_refs {
            for (_, parse) in rest.iter() {
                update_sig_hash(None, parse);
            }
        }

        diff
    }
}
