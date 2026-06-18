/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::btree_map::Entry;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::io;
use std::io::Read;
use std::io::Write;
use std::ops::Bound;
use std::sync::Arc;
use std::sync::RwLock;

use dupe::Dupe;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_heap_serialization::ReaderCache;
use flow_parser::file_key::FileKey;
use flow_parser_utils::package_json::PackageJson;
use flow_utils_concurrency::locked_set::LockedSet;
use parking_lot::Mutex;
use rayon::prelude::*;

use crate::entity::Dependency;
use crate::entity::EntityTransaction;
use crate::entity::ResolvedModule;
use crate::entity::ResolvedRequires;
use crate::haste_module::HasteModule;
use crate::parse::FileEntry;
use crate::parse::MergeHashes;
use crate::parse::PackageParse;
use crate::parse::Parse;
use crate::parse::TypedParse;
use crate::parse::UntypedParse;

pub struct SharedMem {
    pub(crate) file_heap: GcMap<FileKey, FileEntry>,
    pub(crate) haste_module_heap: GcMap<HasteModuleInfo, HasteModule>,
    pub(crate) entity_transaction: EntityTransaction,
    pub(crate) reader_cache: ReaderCache,
    configured_heap_size: Option<u64>,
    configured_hash_table_pow: Option<u32>,
    on_compact: RwLock<Option<Arc<dyn Fn() -> Box<dyn FnOnce() + Send> + Send + Sync>>>,
    pub(crate) gc_state: Mutex<GcState>,
}

pub struct HashStats {
    pub nonempty_slots: i32,
    pub used_slots: i32,
    pub slots: i32,
}

pub(crate) const GC_MAP_SHARDS: usize = 256;
const HEAP_MAGIC_RUST_SHARDED_INDEXED_LZ4_EXTERNAL_FILES: u64 = 0x464C4F57525A5337; // "FLOWRZS7"
const HEAP_MAGIC_RUST_SHARDED_LOCAL_INDEXED_LZ4: u64 = 0x464C4F57525A5338; // "FLOWRZS8"

#[derive(Debug)]
pub(crate) struct GcMap<K, V> {
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

    pub(crate) fn len(&self) -> usize {
        self.shards.iter().map(|shard| shard.read().len()).sum()
    }

    fn clear(&self) {
        for shard in &self.shards {
            shard.write().clear();
        }
    }

    pub(crate) fn get(&self, key: &K) -> Option<V> {
        self.shard(key).read().get(key).map(|value| value.dupe())
    }

    pub(crate) fn insert(&self, key: K, value: V) -> Option<V> {
        let mut map = self.shard(&key).write();
        match map.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(value);
                None
            }
            Entry::Occupied(_) => Some(value),
        }
    }

    pub(crate) fn ensure(&self, key: &K, value: impl FnOnce() -> V) -> (V, bool)
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

    pub(crate) fn remove_if(&self, key: &K, predicate: impl FnOnce(&V) -> bool) -> Option<V> {
        let mut map = self.shard(key).write();
        if map.get(key).is_some_and(predicate) {
            map.remove(key)
        } else {
            None
        }
    }

    pub(crate) fn keys_after(
        &self,
        shard_index: usize,
        after: Option<&K>,
        limit: usize,
    ) -> (Vec<K>, bool)
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

    pub(crate) fn values(&self) -> Vec<V> {
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

    pub(crate) fn iter_unordered(&self) -> Vec<(K, V)>
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
pub(crate) enum GcPhase {
    Idle,
    Mark,
    Sweep,
}

#[derive(Debug)]
pub(crate) struct GcState {
    pub(crate) phase: GcPhase,
    pub(crate) files: Vec<FileKey>,
    pub(crate) haste_modules: Vec<HasteModuleInfo>,
    pub(crate) free_files: Vec<FileKey>,
    pub(crate) free_haste_modules: Vec<HasteModuleInfo>,
    pub(crate) mark_file_shard: usize,
    pub(crate) mark_file_cursor: Option<FileKey>,
    pub(crate) mark_haste_shard: usize,
    pub(crate) mark_haste_cursor: Option<HasteModuleInfo>,
    pub(crate) sweep_file_index: usize,
    pub(crate) sweep_haste_index: usize,
    pub(crate) new_alloc_size: usize,
    pub(crate) free_size: usize,
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

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedHeapHeader {
    magic: u64,
    file_count: u64,
    haste_module_count: u64,
    file_shard_count: u64,
    haste_module_shard_count: u64,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedFileEntry {
    parse: Option<SerializedParse>,
    haste_info: Option<HasteModuleInfo>,
    dependents: Option<Vec<u32>>,
    alternate_file: Option<u32>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedFileHeapShard {
    files: Vec<FileKey>,
    entries: Vec<(u32, SerializedFileEntry)>,
}

#[derive(serde::Serialize, serde::Deserialize)]
enum SerializedParse {
    Typed(SerializedTypedParse),
    Untyped {
        file_hash: u64,
    },
    Package {
        file_hash: u64,
        package_info: PackageJson,
    },
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedTypedParse {
    file_hash: u64,
    ast: Option<Vec<u8>>,
    docblock: Option<Vec<u8>>,
    aloc_table: Option<Vec<u8>>,
    type_sig: Option<Vec<u8>>,
    file_sig: Option<Vec<u8>>,
    exports: Vec<u8>,
    requires: Vec<FlowImportSpecifier>,
    resolved_requires: SerializedResolvedRequires,
    imports: Vec<u8>,
    leader: Option<u32>,
    sig_hash: Option<u64>,
    merge_hashes: Option<MergeHashes>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedResolvedRequires {
    resolved_modules: Vec<SerializedResolvedModule>,
    phantom_dependencies: Vec<SerializedDependency>,
}

#[derive(serde::Serialize, serde::Deserialize)]
enum SerializedResolvedModule {
    HasteModule(Modulename),
    File(u32),
    String(FlowImportSpecifier),
    Null,
}

#[derive(serde::Serialize, serde::Deserialize)]
enum SerializedDependency {
    HasteModule(Modulename),
    File(u32),
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedHasteModule {
    provider: Option<u32>,
    dependents: Vec<u32>,
    all_providers: Vec<u32>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializedHasteModuleHeapShard {
    files: Vec<FileKey>,
    entries: Vec<(HasteModuleInfo, SerializedHasteModule)>,
}

fn encode_into_writer<T: serde::Serialize>(writer: &mut impl Write, value: &T) -> io::Result<()> {
    bincode::serde::encode_into_std_write(value, writer, bincode::config::legacy())
        .map(|_| ())
        .map_err(io::Error::other)
}

fn decode_from_reader<T: serde::de::DeserializeOwned>(reader: &mut impl Read) -> io::Result<T> {
    bincode::serde::decode_from_std_read(reader, bincode::config::legacy())
        .map_err(io::Error::other)
}

fn encode_to_vec<T: serde::Serialize>(value: &T) -> io::Result<Vec<u8>> {
    bincode::serde::encode_to_vec(value, bincode::config::legacy()).map_err(io::Error::other)
}

fn decode_from_slice<T: serde::de::DeserializeOwned>(bytes: &[u8]) -> io::Result<T> {
    bincode::serde::decode_from_slice(bytes, bincode::config::legacy())
        .map(|(value, _)| value)
        .map_err(io::Error::other)
}

fn write_u64(writer: &mut impl Write, value: u64) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn read_u64(reader: &mut impl Read) -> io::Result<u64> {
    let mut bytes = [0; 8];
    reader.read_exact(&mut bytes)?;
    Ok(u64::from_le_bytes(bytes))
}

fn write_compressed_block(writer: &mut impl Write, bytes: &[u8]) -> io::Result<()> {
    let compressed = lz4_flex::compress_prepend_size(bytes);
    write_u64(writer, compressed.len() as u64)?;
    writer.write_all(&compressed)
}

fn read_compressed_block(reader: &mut impl Read) -> io::Result<Vec<u8>> {
    let compressed_len = read_u64(reader)? as usize;
    let mut compressed = vec![0; compressed_len];
    reader.read_exact(&mut compressed)?;
    Ok(compressed)
}

fn decompress_block(compressed: &[u8]) -> io::Result<Vec<u8>> {
    lz4_flex::decompress_size_prepended(compressed).map_err(io::Error::other)
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

    pub(crate) fn note_alloc(&self) {
        self.note_alloc_many(1);
    }

    pub(crate) fn note_alloc_many(&self, count: usize) {
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

    pub(crate) fn start_cycle(&self, gc_state: &mut GcState) {
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

    pub(crate) fn mark_slice(&self, gc_state: &mut GcState, work: usize) -> usize {
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

    pub(crate) fn sweep_slice(&self, gc_state: &mut GcState, work: usize) -> usize {
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

    pub(crate) fn compact_helper(
        &self,
        free_files: Vec<FileKey>,
        free_haste_modules: Vec<HasteModuleInfo>,
    ) {
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

    fn file_index(file_to_index: &BTreeMap<FileKey, u32>, file: &FileKey) -> u32 {
        *file_to_index
            .get(file)
            .expect("file should have been collected in heap file table")
    }

    fn file_from_index(files: &[FileKey], index: u32) -> io::Result<FileKey> {
        files
            .get(index as usize)
            .map(Dupe::dupe)
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "invalid file index"))
    }

    fn collect_modulename_file_keys(module: &Modulename, file_keys: &mut BTreeSet<FileKey>) {
        if let Modulename::Filename(file) = module {
            file_keys.insert(file.dupe());
        }
    }

    fn collect_dependency_file_keys(dependency: &Dependency, file_keys: &mut BTreeSet<FileKey>) {
        match dependency {
            Dependency::HasteModule(module) => {
                Self::collect_modulename_file_keys(module, file_keys)
            }
            Dependency::File(file) => {
                file_keys.insert(file.dupe());
            }
        }
    }

    fn collect_resolved_module_file_keys(
        module: &crate::entity::ResolvedModule,
        file_keys: &mut BTreeSet<FileKey>,
    ) {
        match module {
            crate::entity::ResolvedModule::HasteModule(module) => {
                Self::collect_modulename_file_keys(module, file_keys)
            }
            crate::entity::ResolvedModule::File(file) => {
                file_keys.insert(file.dupe());
            }
            crate::entity::ResolvedModule::String(_) | crate::entity::ResolvedModule::Null => {}
        }
    }

    fn collect_resolved_requires_file_keys(
        resolved_requires: Option<ResolvedRequires>,
        file_keys: &mut BTreeSet<FileKey>,
    ) {
        if let Some(resolved_requires) = resolved_requires {
            for module in resolved_requires.get_resolved_modules() {
                Self::collect_resolved_module_file_keys(module, file_keys);
            }
            for dependency in resolved_requires.get_phantom_dependencies() {
                Self::collect_dependency_file_keys(dependency, file_keys);
            }
        }
    }

    fn collect_parse_file_keys(parse: &Parse, file_keys: &mut BTreeSet<FileKey>) {
        match parse {
            Parse::Typed(typed) => {
                Self::collect_resolved_requires_file_keys(
                    typed.resolved_requires.read_latest_clone(),
                    file_keys,
                );
                if let Some(leader) = typed.leader.read_latest_clone() {
                    file_keys.insert(leader);
                }
            }
            Parse::Untyped(_) | Parse::Package(_) => {}
        }
    }

    fn collect_file_entry_file_keys(file_entry: &FileEntry, file_keys: &mut BTreeSet<FileKey>) {
        if let Some(parse) = file_entry.parse_latest() {
            Self::collect_parse_file_keys(&parse, file_keys);
        }
        if let Some(dependents) = file_entry.get_dependents() {
            file_keys.extend(dependents);
        }
        if let Some(alternate_file) = file_entry.get_alternate_file() {
            file_keys.insert(alternate_file);
        }
    }

    fn collect_haste_module_file_keys(
        haste_module: &HasteModule,
        file_keys: &mut BTreeSet<FileKey>,
    ) {
        if let Some(provider) = haste_module.get_provider() {
            file_keys.insert(provider);
        }
        file_keys.extend(haste_module.get_dependents());
        file_keys.extend(haste_module.get_all_providers());
    }

    pub fn collect_heap_file_table(&self) -> Vec<FileKey> {
        let mut file_keys = BTreeSet::new();
        for shard in &self.file_heap.shards {
            let map = shard.read();
            for (file, entry) in map.iter() {
                file_keys.insert(file.dupe());
                Self::collect_file_entry_file_keys(entry, &mut file_keys);
            }
        }
        for shard in &self.haste_module_heap.shards {
            let map = shard.read();
            for module in map.values() {
                Self::collect_haste_module_file_keys(module, &mut file_keys);
            }
        }
        file_keys.into_iter().collect()
    }

    fn file_to_index(files: &[FileKey]) -> BTreeMap<FileKey, u32> {
        files
            .iter()
            .enumerate()
            .map(|(index, file)| {
                (
                    file.dupe(),
                    u32::try_from(index).expect("heap file table index should fit in u32"),
                )
            })
            .collect()
    }

    pub fn save_heap(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let finish_compaction = self
            .on_compact
            .read()
            .unwrap()
            .as_ref()
            .map(|on_compact| on_compact());
        let header = SerializedHeapHeader {
            magic: HEAP_MAGIC_RUST_SHARDED_LOCAL_INDEXED_LZ4,
            file_count: self.file_heap.len() as u64,
            haste_module_count: self.haste_module_heap.len() as u64,
            file_shard_count: GC_MAP_SHARDS as u64,
            haste_module_shard_count: GC_MAP_SHARDS as u64,
        };
        encode_into_writer(writer, &header)?;
        for shard in &self.file_heap.shards {
            let map = shard.read();
            let mut files = BTreeSet::new();
            for (file, entry) in map.iter() {
                files.insert(file.dupe());
                Self::collect_file_entry_file_keys(entry, &mut files);
            }
            let files = files.into_iter().collect::<Vec<_>>();
            let file_to_index = Self::file_to_index(&files);
            let entries = map
                .iter()
                .map(|(file, entry)| {
                    (
                        Self::file_index(&file_to_index, file),
                        Self::serialized_file_entry(entry, &file_to_index),
                    )
                })
                .collect();
            let shard = SerializedFileHeapShard { files, entries };
            let bytes = encode_to_vec(&shard)?;
            write_compressed_block(writer, &bytes)?;
        }
        for shard in &self.haste_module_heap.shards {
            let map = shard.read();
            let mut files = BTreeSet::new();
            for module in map.values() {
                Self::collect_haste_module_file_keys(module, &mut files);
            }
            let files = files.into_iter().collect::<Vec<_>>();
            let file_to_index = Self::file_to_index(&files);
            let entries = map
                .iter()
                .map(|(info, module)| {
                    (
                        info.dupe(),
                        Self::serialized_haste_module(module, &file_to_index),
                    )
                })
                .collect();
            let shard = SerializedHasteModuleHeapShard { files, entries };
            let bytes = encode_to_vec(&shard)?;
            write_compressed_block(writer, &bytes)?;
        }
        if let Some(finish_compaction) = finish_compaction {
            finish_compaction();
        }
        Ok(())
    }

    pub fn save_heap_with_file_table(
        &self,
        writer: &mut impl Write,
        files: &[FileKey],
    ) -> std::io::Result<()> {
        let finish_compaction = self
            .on_compact
            .read()
            .unwrap()
            .as_ref()
            .map(|on_compact| on_compact());
        let header = SerializedHeapHeader {
            magic: HEAP_MAGIC_RUST_SHARDED_INDEXED_LZ4_EXTERNAL_FILES,
            file_count: self.file_heap.len() as u64,
            haste_module_count: self.haste_module_heap.len() as u64,
            file_shard_count: GC_MAP_SHARDS as u64,
            haste_module_shard_count: GC_MAP_SHARDS as u64,
        };
        let file_to_index = Self::file_to_index(files);
        encode_into_writer(writer, &header)?;
        for shard in &self.file_heap.shards {
            let map = shard.read();
            let entries = map
                .iter()
                .map(|(file, entry)| {
                    (
                        Self::file_index(&file_to_index, file),
                        Self::serialized_file_entry(entry, &file_to_index),
                    )
                })
                .collect();
            let shard = SerializedFileHeapShard {
                files: Vec::new(),
                entries,
            };
            let bytes = encode_to_vec(&shard)?;
            write_compressed_block(writer, &bytes)?;
        }
        for shard in &self.haste_module_heap.shards {
            let map = shard.read();
            let entries = map
                .iter()
                .map(|(info, module)| {
                    (
                        info.dupe(),
                        Self::serialized_haste_module(module, &file_to_index),
                    )
                })
                .collect();
            let shard = SerializedHasteModuleHeapShard {
                files: Vec::new(),
                entries,
            };
            let bytes = encode_to_vec(&shard)?;
            write_compressed_block(writer, &bytes)?;
        }
        if let Some(finish_compaction) = finish_compaction {
            finish_compaction();
        }
        Ok(())
    }

    pub fn load_heap(&self, reader: &mut impl Read) -> std::io::Result<()> {
        let header: SerializedHeapHeader = decode_from_reader(reader)?;
        Self::validate_serialized_heap_header(&header, HEAP_MAGIC_RUST_SHARDED_LOCAL_INDEXED_LZ4)?;
        self.reset_heap();
        self.load_heap_shards(reader, None)
    }

    pub fn load_heap_with_file_table(
        &self,
        reader: &mut impl Read,
        files: Arc<Vec<FileKey>>,
    ) -> std::io::Result<()> {
        let header: SerializedHeapHeader = decode_from_reader(reader)?;
        if header.magic != HEAP_MAGIC_RUST_SHARDED_LOCAL_INDEXED_LZ4
            && header.magic != HEAP_MAGIC_RUST_SHARDED_INDEXED_LZ4_EXTERNAL_FILES
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "hh_load_heap: invalid magic number",
            ));
        }
        if header.file_shard_count != GC_MAP_SHARDS as u64
            || header.haste_module_shard_count != GC_MAP_SHARDS as u64
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "hh_load_heap: invalid shard count",
            ));
        }
        self.reset_heap();
        let files = if header.magic == HEAP_MAGIC_RUST_SHARDED_INDEXED_LZ4_EXTERNAL_FILES {
            Some(files)
        } else {
            None
        };
        self.load_heap_shards(reader, files)
    }

    fn load_heap_shards(
        &self,
        reader: &mut impl Read,
        external_files: Option<Arc<Vec<FileKey>>>,
    ) -> std::io::Result<()> {
        let mut file_shards = Vec::with_capacity(GC_MAP_SHARDS);
        for shard_index in 0..GC_MAP_SHARDS {
            file_shards.push((shard_index, read_compressed_block(reader)?));
        }
        let mut haste_module_shards = Vec::with_capacity(GC_MAP_SHARDS);
        for shard_index in 0..GC_MAP_SHARDS {
            haste_module_shards.push((shard_index, read_compressed_block(reader)?));
        }
        let external_files = external_files.as_deref().map(|files| files.as_slice());
        let (file_result, haste_result) = rayon::join(
            || {
                file_shards
                    .into_par_iter()
                    .try_for_each(|(shard_index, bytes)| {
                        self.load_file_heap_shard(shard_index, bytes, external_files)
                            .map(|_| ())
                    })
            },
            || {
                haste_module_shards
                    .into_par_iter()
                    .try_for_each(|(shard_index, bytes)| {
                        self.load_haste_heap_shard(shard_index, bytes, external_files)
                            .map(|_| ())
                    })
            },
        );
        file_result?;
        haste_result?;
        self.commit_entities();
        Ok(())
    }

    pub fn reset_heap(&self) {
        self.file_heap.clear();
        self.haste_module_heap.clear();
        self.clear_reader_cache();
        *self.gc_state.lock() = GcState::default();
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

    fn serialized_resolved_module(
        module: &ResolvedModule,
        file_to_index: &BTreeMap<FileKey, u32>,
    ) -> SerializedResolvedModule {
        match module {
            ResolvedModule::HasteModule(module) => {
                SerializedResolvedModule::HasteModule(module.clone())
            }
            ResolvedModule::File(file) => {
                SerializedResolvedModule::File(Self::file_index(file_to_index, file))
            }
            ResolvedModule::String(specifier) => SerializedResolvedModule::String(specifier.dupe()),
            ResolvedModule::Null => SerializedResolvedModule::Null,
        }
    }

    fn serialized_dependency(
        dependency: &Dependency,
        file_to_index: &BTreeMap<FileKey, u32>,
    ) -> SerializedDependency {
        match dependency {
            Dependency::HasteModule(module) => SerializedDependency::HasteModule(module.clone()),
            Dependency::File(file) => {
                SerializedDependency::File(Self::file_index(file_to_index, file))
            }
        }
    }

    fn serialized_resolved_requires(
        resolved_requires: Option<ResolvedRequires>,
        file_to_index: &BTreeMap<FileKey, u32>,
    ) -> SerializedResolvedRequires {
        match resolved_requires {
            Some(resolved_requires) => SerializedResolvedRequires {
                resolved_modules: resolved_requires
                    .get_resolved_modules()
                    .iter()
                    .map(|module| Self::serialized_resolved_module(module, file_to_index))
                    .collect(),
                phantom_dependencies: resolved_requires
                    .get_phantom_dependencies()
                    .iter()
                    .map(|dependency| Self::serialized_dependency(dependency, file_to_index))
                    .collect(),
            },
            None => SerializedResolvedRequires {
                resolved_modules: Vec::new(),
                phantom_dependencies: Vec::new(),
            },
        }
    }

    fn serialized_parse(parse: &Parse, file_to_index: &BTreeMap<FileKey, u32>) -> SerializedParse {
        match parse {
            Parse::Typed(typed) => SerializedParse::Typed(SerializedTypedParse {
                file_hash: typed.file_hash,
                ast: None,
                docblock: None,
                aloc_table: None,
                type_sig: None,
                file_sig: None,
                exports: typed.exports.as_ref().to_vec(),
                requires: typed.requires.as_ref().to_vec(),
                resolved_requires: Self::serialized_resolved_requires(
                    typed.resolved_requires.read_latest_clone(),
                    file_to_index,
                ),
                imports: typed.imports.as_ref().to_vec(),
                leader: None,
                sig_hash: None,
                merge_hashes: typed.merge_hashes.read().clone(),
            }),
            Parse::Untyped(untyped) => SerializedParse::Untyped {
                file_hash: untyped.file_hash,
            },
            Parse::Package(package) => SerializedParse::Package {
                file_hash: package.file_hash,
                package_info: package.package_info.as_ref().clone(),
            },
        }
    }

    fn serialized_file_entry(
        file_entry: &FileEntry,
        file_to_index: &BTreeMap<FileKey, u32>,
    ) -> SerializedFileEntry {
        let parse = file_entry.parse_latest();
        let parse = parse
            .as_ref()
            .map(|parse| Self::serialized_parse(parse, file_to_index));
        SerializedFileEntry {
            parse,
            haste_info: file_entry.haste_info_entity().read_latest_clone(),
            dependents: file_entry.get_dependents().map(|files| {
                files
                    .into_iter()
                    .map(|file| Self::file_index(file_to_index, &file))
                    .collect()
            }),
            alternate_file: file_entry
                .get_alternate_file()
                .map(|file| Self::file_index(file_to_index, &file)),
        }
    }

    fn serialized_haste_module(
        haste_module: &HasteModule,
        file_to_index: &BTreeMap<FileKey, u32>,
    ) -> SerializedHasteModule {
        SerializedHasteModule {
            provider: haste_module
                .get_provider()
                .map(|file| Self::file_index(file_to_index, &file)),
            dependents: haste_module
                .get_dependents()
                .into_iter()
                .map(|file| Self::file_index(file_to_index, &file))
                .collect(),
            all_providers: haste_module
                .get_all_providers()
                .into_iter()
                .map(|file| Self::file_index(file_to_index, &file))
                .collect(),
        }
    }

    fn resolved_module_from_serialized(
        serialized: SerializedResolvedModule,
        files: &[FileKey],
    ) -> io::Result<ResolvedModule> {
        match serialized {
            SerializedResolvedModule::HasteModule(module) => {
                Ok(ResolvedModule::HasteModule(module))
            }
            SerializedResolvedModule::File(file) => {
                Ok(ResolvedModule::File(Self::file_from_index(files, file)?))
            }
            SerializedResolvedModule::String(specifier) => Ok(ResolvedModule::String(specifier)),
            SerializedResolvedModule::Null => Ok(ResolvedModule::Null),
        }
    }

    fn dependency_from_serialized(
        serialized: SerializedDependency,
        files: &[FileKey],
    ) -> io::Result<Dependency> {
        match serialized {
            SerializedDependency::HasteModule(module) => Ok(Dependency::HasteModule(module)),
            SerializedDependency::File(file) => {
                Ok(Dependency::File(Self::file_from_index(files, file)?))
            }
        }
    }

    fn resolved_requires_from_serialized(
        serialized: SerializedResolvedRequires,
        files: &[FileKey],
    ) -> io::Result<ResolvedRequires> {
        Ok(ResolvedRequires::new(
            serialized
                .resolved_modules
                .into_iter()
                .map(|module| Self::resolved_module_from_serialized(module, files))
                .collect::<io::Result<Vec<_>>>()?,
            serialized
                .phantom_dependencies
                .into_iter()
                .map(|dependency| Self::dependency_from_serialized(dependency, files))
                .collect::<io::Result<Vec<_>>>()?,
        ))
    }

    fn optional_entity<T>(&self, value: Option<T>) -> Arc<crate::entity::Entity<T>> {
        match value {
            Some(value) => Arc::new(crate::entity::Entity::new_committed(
                self.entity_transaction.dupe(),
                value,
            )),
            None => Arc::new(crate::entity::Entity::empty_committed(
                self.entity_transaction.dupe(),
            )),
        }
    }

    fn parse_from_serialized(
        &self,
        serialized: SerializedParse,
        files: &[FileKey],
    ) -> io::Result<Parse> {
        match serialized {
            SerializedParse::Typed(typed) => Ok(Parse::Typed(TypedParse {
                file_hash: typed.file_hash,
                ast: typed.ast.map(|bytes| Arc::from(bytes.into_boxed_slice())),
                docblock: typed
                    .docblock
                    .map(|bytes| Arc::from(bytes.into_boxed_slice())),
                aloc_table: typed
                    .aloc_table
                    .map(|bytes| Arc::from(bytes.into_boxed_slice())),
                type_sig: typed
                    .type_sig
                    .map(|bytes| Arc::from(bytes.into_boxed_slice())),
                file_sig: typed
                    .file_sig
                    .map(|bytes| Arc::from(bytes.into_boxed_slice())),
                exports: Arc::from(typed.exports.into_boxed_slice()),
                requires: Arc::from(typed.requires.into_boxed_slice()),
                resolved_requires: Arc::new(crate::entity::Entity::new_committed(
                    self.entity_transaction.dupe(),
                    Self::resolved_requires_from_serialized(typed.resolved_requires, files)?,
                )),
                imports: Arc::from(typed.imports.into_boxed_slice()),
                leader: self.optional_entity(
                    typed
                        .leader
                        .map(|file| Self::file_from_index(files, file))
                        .transpose()?,
                ),
                sig_hash: self.optional_entity(typed.sig_hash),
                merge_hashes: Arc::new(parking_lot::RwLock::new(typed.merge_hashes)),
            })),
            SerializedParse::Untyped { file_hash } => {
                Ok(Parse::Untyped(UntypedParse::new(file_hash)))
            }
            SerializedParse::Package {
                file_hash,
                package_info,
            } => Ok(Parse::Package(PackageParse::new(
                file_hash,
                Arc::new(package_info),
            ))),
        }
    }

    fn locked_set_from_serialized(files: Vec<FileKey>) -> LockedSet<FileKey> {
        let set = LockedSet::new();
        for file in files {
            set.insert(file);
        }
        set
    }

    fn file_entry_from_serialized(
        &self,
        serialized: SerializedFileEntry,
        files: &[FileKey],
    ) -> io::Result<FileEntry> {
        Ok(FileEntry {
            parse: match serialized.parse {
                Some(parse) => Arc::new(crate::entity::Entity::new_committed(
                    self.entity_transaction.dupe(),
                    self.parse_from_serialized(parse, files)?,
                )),
                None => Arc::new(crate::entity::Entity::empty_committed(
                    self.entity_transaction.dupe(),
                )),
            },
            haste_info: self.optional_entity(serialized.haste_info),
            dependents: serialized
                .dependents
                .map(|dependents| {
                    dependents
                        .into_iter()
                        .map(|file| Self::file_from_index(files, file))
                        .collect::<io::Result<Vec<_>>>()
                })
                .transpose()?
                .map(Self::locked_set_from_serialized)
                .map(Arc::new),
            alternate_file: Arc::new(parking_lot::RwLock::new(
                serialized
                    .alternate_file
                    .map(|file| Self::file_from_index(files, file))
                    .transpose()?,
            )),
        })
    }

    fn haste_module_from_serialized(
        &self,
        info: HasteModuleInfo,
        serialized: SerializedHasteModule,
        files: &[FileKey],
    ) -> io::Result<HasteModule> {
        Ok(HasteModule::new_committed(
            self.entity_transaction.dupe(),
            info,
            serialized
                .provider
                .map(|file| Self::file_from_index(files, file))
                .transpose()?,
            serialized
                .dependents
                .into_iter()
                .map(|file| Self::file_from_index(files, file))
                .collect::<io::Result<Vec<_>>>()?,
            serialized
                .all_providers
                .into_iter()
                .map(|file| Self::file_from_index(files, file))
                .collect::<io::Result<Vec<_>>>()?,
        ))
    }

    fn validate_serialized_heap_header(
        header: &SerializedHeapHeader,
        expected_magic: u64,
    ) -> io::Result<()> {
        if header.magic != expected_magic {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "hh_load_heap: invalid magic number",
            ));
        }
        if header.file_shard_count != GC_MAP_SHARDS as u64
            || header.haste_module_shard_count != GC_MAP_SHARDS as u64
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "hh_load_heap: invalid shard count",
            ));
        }
        Ok(())
    }

    fn load_file_heap_shard(
        &self,
        shard_index: usize,
        compressed: Vec<u8>,
        external_files: Option<&[FileKey]>,
    ) -> io::Result<u64> {
        let bytes = decompress_block(&compressed)?;
        let shard: SerializedFileHeapShard = decode_from_slice(&bytes)?;
        let SerializedFileHeapShard {
            files: local_files,
            entries,
        } = shard;
        let files = external_files.unwrap_or(&local_files);
        let count = entries.len() as u64;
        let map = entries
            .into_iter()
            .map(|(file, entry)| {
                Ok((
                    Self::file_from_index(files, file)?,
                    self.file_entry_from_serialized(entry, files)?,
                ))
            })
            .collect::<io::Result<BTreeMap<_, _>>>()?;
        assert_eq!(
            map.len() as u64,
            count,
            "file heap shard size should match saved shard header"
        );
        *self.file_heap.shards[shard_index].write() = map;
        Ok(count)
    }

    fn load_haste_heap_shard(
        &self,
        shard_index: usize,
        compressed: Vec<u8>,
        external_files: Option<&[FileKey]>,
    ) -> io::Result<u64> {
        let bytes = decompress_block(&compressed)?;
        let shard: SerializedHasteModuleHeapShard = decode_from_slice(&bytes)?;
        let SerializedHasteModuleHeapShard {
            files: local_files,
            entries,
        } = shard;
        let files = external_files.unwrap_or(&local_files);
        let count = entries.len() as u64;
        let map = entries
            .into_iter()
            .map(|(info, module)| {
                Ok((
                    info.dupe(),
                    self.haste_module_from_serialized(info, module, files)?,
                ))
            })
            .collect::<io::Result<BTreeMap<_, _>>>()?;
        assert_eq!(
            map.len() as u64,
            count,
            "haste heap shard size should match saved shard header"
        );
        *self.haste_module_heap.shards[shard_index].write() = map;
        Ok(count)
    }

    pub fn commit_entities(&self) {
        self.entity_transaction.commit();
    }
}

impl Default for SharedMem {
    fn default() -> Self {
        Self::new()
    }
}
