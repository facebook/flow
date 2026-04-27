/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
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
use flow_utils_concurrency::locked_map::LockedMap;

use crate::entity::Dependency;
use crate::entity::ResolvedRequires;
use crate::haste_module::HasteModule;
use crate::parse::FileEntry;
use crate::parse::PackageParse;
use crate::parse::Parse;
use crate::parse::TypedParse;

pub struct SharedMem {
    file_heap: LockedMap<FileKey, FileEntry>,
    pub haste_module_heap: LockedMap<HasteModuleInfo, HasteModule>,
    reader_cache: ReaderCache,
    configured_heap_size: Option<u64>,
    configured_hash_table_pow: Option<u32>,
    on_compact: RwLock<Option<Arc<dyn Fn() -> Box<dyn FnOnce() + Send> + Send + Sync>>>,
}

pub struct HashStats {
    pub nonempty_slots: i32,
    pub used_slots: i32,
    pub slots: i32,
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
            file_heap: LockedMap::new(),
            haste_module_heap: LockedMap::new(),
            reader_cache: ReaderCache::new(),
            configured_heap_size,
            configured_hash_table_pow,
            on_compact: RwLock::new(None),
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

    pub fn file_has_changed(&self, _file: &FileKey) -> bool {
        false
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
        self.haste_module_heap
            .ensure(&info, || HasteModule::new(info.clone()))
            .dupe()
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
            self.file_heap
                .insert(impl_key.dupe(), FileEntry::new_phantom());
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
                        crate::entity::ResolvedRequires::new(vec![], vec![]),
                    )),
                    Arc::new(crate::entity::Entity::empty()),
                    Arc::new(crate::entity::Entity::empty()),
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
            self.calc_dirty_modules(&file, existing_entry)
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
                    crate::entity::ResolvedRequires::new(vec![], vec![]),
                )),
                imports,
                Arc::new(crate::entity::Entity::empty()),
                Arc::new(crate::entity::Entity::empty()),
            );

            let file_entry = FileEntry::new(
                Parse::Typed(typed_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => self.calc_dirty_modules(&file, &file_entry),
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "LockedMap::insert returned Some(_) but get(&file) is None; \
                         file_heap has no per-key removal so this should be unreachable",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Typed(typed_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, in_map_entry)
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
            self.calc_dirty_modules(&file, existing_entry)
        } else {
            let untyped_parse = UntypedParse::new(file_hash);
            let file_entry = FileEntry::new(
                Parse::Untyped(untyped_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => self.calc_dirty_modules(&file, &file_entry),
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "LockedMap::insert returned Some(_) but get(&file) is None; \
                         file_heap has no per-key removal so this should be unreachable",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Untyped(untyped_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, in_map_entry)
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
            self.calc_dirty_modules(&file, existing_entry)
        } else {
            let package_parse = PackageParse::new(file_hash, package_info);
            let file_entry = FileEntry::new(
                Parse::Package(package_parse.dupe()),
                haste_module_info.clone(),
                has_dependents,
            );
            match self.file_heap.insert(file.dupe(), file_entry.dupe()) {
                None => self.calc_dirty_modules(&file, &file_entry),
                Some(_rejected) => {
                    let in_map_entry = self.file_heap.get(&file).expect(
                        "LockedMap::insert returned Some(_) but get(&file) is None; \
                         file_heap has no per-key removal so this should be unreachable",
                    );
                    match in_map_entry.parse_latest() {
                        None => {
                            in_map_entry.parse().set(Parse::Package(package_parse));
                            if let Some(info) = haste_module_info {
                                in_map_entry.haste_info_entity().set(info);
                            }
                            self.calc_dirty_modules(&file, in_map_entry)
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

                for dep in &old_deps {
                    if !new_deps.contains(dep) {
                        self.remove_dependent_from(file, dep);
                    }
                }

                for dep in &new_deps {
                    if !old_deps.contains(dep) {
                        self.add_dependent_to(file, dep);
                    }
                }
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

    fn add_dependent_to(&self, file: &FileKey, dep: &Dependency) {
        match dep {
            Dependency::HasteModule(Modulename::Haste(haste_info)) => {
                let module = self.get_or_create_haste_module(haste_info.dupe());
                module.add_dependent(file.dupe());
            }
            Dependency::HasteModule(Modulename::Filename(dep_file))
            | Dependency::File(dep_file) => {
                let dep_entry = self.file_heap.ensure(dep_file, FileEntry::new_phantom);
                dep_entry.add_dependent(file.dupe());
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
        for dep in &old_dependencies {
            if !new_dependencies.contains(dep) {
                self.add_dependent_to(file, dep);
            }
        }
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
                for dep in &old_dependencies {
                    self.add_dependent_to(file_key, dep);
                }
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

    pub fn compact_parse(&self, file: &FileKey) {
        let _ = file;
    }

    pub fn collect_slice(&self, _work: usize) -> bool {
        let finish_compaction = self
            .on_compact
            .read()
            .unwrap()
            .as_ref()
            .map(|on_compact| on_compact());
        for (file, _) in self.file_heap.iter_unordered() {
            self.compact_parse(file);
        }
        self.clear_reader_cache();
        if let Some(finish_compaction) = finish_compaction {
            finish_compaction();
        }
        true
    }

    pub fn commit_entities(&self) {
        for entry in self.file_heap.values() {
            entry.parse.commit();
            entry.haste_info.commit();
            if let Some(Parse::Typed(typed)) = entry.parse_latest() {
                typed.resolved_requires.commit();
                typed.leader.commit();
                typed.sig_hash.commit();
            }
        }
        for module in self.haste_module_heap.values() {
            module.commit_provider();
        }
    }

    pub fn rollback_entities(&self) {
        for (file_key, entry) in self.file_heap.iter_unordered() {
            self.rollback_file(file_key, entry);
        }
        for module in self.haste_module_heap.values() {
            module.rollback_provider();
        }
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
