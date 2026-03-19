/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;

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
}

impl SharedMem {
    pub fn new() -> Self {
        Self {
            file_heap: LockedMap::new(),
            haste_module_heap: LockedMap::new(),
        }
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
                    Some(file_key.dupe())
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

    pub fn get_ast_unsafe(&self, file: &FileKey) -> Arc<Program<Loc, Loc>> {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .ast
            .as_ref()
            .unwrap_or_else(|| panic!("AST not found for file: {}", file.as_str()))
            .dupe()
    }

    pub fn get_ast(&self, file: &FileKey) -> Option<Arc<Program<Loc, Loc>>> {
        self.get_typed_parse(file)
            .and_then(|typed| typed.ast.as_ref().map(|ast| ast.dupe()))
    }

    pub fn get_docblock_unsafe(&self, file: &FileKey) -> Arc<Docblock> {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .docblock
            .as_ref()
            .unwrap_or_else(|| panic!("Docblock not found for file: {}", file.as_str()))
            .dupe()
    }

    pub fn get_aloc_table_unsafe(&self, file: &FileKey) -> Arc<PackedALocTable> {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .aloc_table
            .as_ref()
            .unwrap_or_else(|| panic!("ALocTable not found for file: {}", file.as_str()))
            .dupe()
    }

    pub fn get_aloc_table(&self, file: &FileKey) -> Option<Arc<PackedALocTable>> {
        self.get_typed_parse(file)
            .and_then(|t| t.aloc_table.as_ref().map(|a| a.dupe()))
    }

    pub fn loc_of_aloc(&self, aloc: &ALoc) -> Loc {
        let source = match aloc.source() {
            Some(s) => s.dupe(),
            // Concrete ALocs (from ALoc::of_loc) may not have a source.
            // Fall back to to_loc_exn which handles concrete ALocs directly.
            None => return aloc.to_loc_exn(),
        };
        // Try to get the aloc table. For files that failed parsing (e.g., parse errors),
        // there may be no typed parse and hence no aloc table. In that case, the ALoc
        // should be concrete (created via ALoc::of_loc) and to_loc_exn handles it.
        match self.get_aloc_table(&source) {
            Some(packed) => {
                let lazy_table: LazyALocTable = Rc::new(LazyCell::new(Box::new(move || {
                    Rc::new(ALocTable::unpack(source, &packed))
                })
                    as Box<dyn FnOnce() -> Rc<ALocTable>>));
                aloc.to_loc(&lazy_table)
            }
            None => aloc.to_loc_exn(),
        }
    }

    pub fn get_type_sig_unsafe(&self, file: &FileKey) -> Arc<TypeSigModule<Loc>> {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .type_sig
            .as_ref()
            .unwrap_or_else(|| panic!("Type signature not found for file: {}", file.as_str()))
            .dupe()
    }

    pub fn get_exports_unsafe(&self, file: &FileKey) -> Arc<Exports> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.exports.dupe()
    }

    pub fn get_imports_unsafe(&self, file: &FileKey) -> Arc<Imports> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.imports.dupe()
    }

    pub fn get_tolerable_file_sig_unsafe(
        &self,
        file: &FileKey,
    ) -> (Arc<FileSig>, Arc<[TolerableError<Loc>]>) {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .file_sig
            .as_ref()
            .unwrap_or_else(|| panic!("File sig not found for file: {}", file.as_str()))
            .dupe()
    }

    pub fn get_requires_unsafe(&self, file: &FileKey) -> Arc<[FlowImportSpecifier]> {
        let typed = self.get_typed_parse_unsafe(file);
        typed.requires.dupe()
    }

    pub fn get_resolved_requires_unsafe(&self, file: &FileKey) -> ResolvedRequires {
        let typed = self.get_typed_parse_unsafe(file);
        typed
            .resolved_requires
            .read_latest_clone()
            .expect("ResolvedRequires should be set")
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

    pub fn file_has_changed(&self, _file: &FileKey) -> bool {
        // TODO: Implement proper versioning system like OCaml's entity_changed.
        // For now, return false since we're building the heap from scratch during init.
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

    // Calculate the set of dirty modules and prepare those modules to be committed.
    //
    // If this file became a provider to a haste module, we add this file to the
    // module's "all providers" list and mark the module as dirty.
    //
    // If this file no longer providers a haste module, we do not remove the file
    // now, to avoid complexity around concurrent deletion. Instead, old providers
    // are "logically" deleted, the module is marked as dirty, and we perform
    // deferred deletions during commit_modules.
    //
    // We also mark modules as dirty even if the module itself does not need to be
    // committed -- that is, we do not need to pick a new provider. A module is also
    // considered dirty if the provider file's contents have changed.
    //
    // TODO: Regarding the above, we might profitably separate these two notions of
    // dirtiness! We can skip re-picking a provider for modules which keep the same
    // provider, but we still need to re-check its dependents.
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
            // Changing `file` does not cause `new_m`'s provider to be re-picked,
            // but the module is still dirty because `file` changed. (see TODO)
            (None, None, new_info)
        };

        let mut dirty_modules = BTreeSet::new();

        if let Some(info) = old_haste_info {
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

        // Changing `file` does not cause the eponymous module's provider to be
        // re-picked, but it is still dirty because `file` changed. (see TODO)
        dirty_modules.insert(Modulename::Filename(file_key.dupe()));

        dirty_modules
    }

    fn handle_flow_ext(&self, file: &FileKey) -> BTreeSet<Modulename> {
        if !files::has_flow_ext(file) {
            return BTreeSet::new();
        }
        let impl_key = files::chop_flow_ext(file);
        let phantom_dirty = if self.file_heap.get(&impl_key).is_none() {
            self.add_unparsed(impl_key.dupe(), 0, None)
        } else {
            BTreeSet::new()
        };
        self.set_alternate_file(&impl_key, file.dupe());
        phantom_dirty
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
            Arc::new(crate::entity::Entity::new(file.dupe())),
            Arc::new(crate::entity::Entity::new(0u64)),
        );

        let has_dependents = !file.as_str().ends_with(".flow");

        // Check if file already exists - if so, update parse entity
        let mut dirty_modules = if let Some(existing_entry) = self.file_heap.get(&file) {
            existing_entry.parse().set(Parse::Typed(typed_parse));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, existing_entry)
        } else {
            // Create new entry
            let file_entry = FileEntry::new(
                Parse::Typed(typed_parse),
                haste_module_info.clone(),
                has_dependents,
            );
            self.file_heap.insert(file.dupe(), file_entry.dupe());
            self.calc_dirty_modules(&file, &file_entry)
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

        // Check if file already exists - if so, update parse entity
        let mut dirty_modules = if let Some(existing_entry) = self.file_heap.get(&file) {
            existing_entry
                .parse()
                .set(Parse::Untyped(UntypedParse::new(file_hash)));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, existing_entry)
        } else {
            let file_entry = FileEntry::new(
                Parse::Untyped(UntypedParse::new(file_hash)),
                haste_module_info,
                has_dependents,
            );
            self.file_heap.insert(file.dupe(), file_entry.dupe());
            self.calc_dirty_modules(&file, &file_entry)
        };

        dirty_modules.extend(self.handle_flow_ext(&file));
        dirty_modules
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

        // Check if file already exists - if so, update parse entity
        if let Some(existing_entry) = self.file_heap.get(&file) {
            existing_entry
                .parse()
                .set(Parse::Package(PackageParse::new(file_hash, package_info)));
            if let Some(info) = haste_module_info {
                existing_entry.haste_info_entity().set(info);
            }
            self.calc_dirty_modules(&file, existing_entry)
        } else {
            let file_entry = FileEntry::new(
                Parse::Package(PackageParse::new(file_hash, package_info)),
                haste_module_info,
                has_dependents,
            );
            self.file_heap.insert(file.dupe(), file_entry.dupe());
            self.calc_dirty_modules(&file, &file_entry)
        }
    }

    pub fn set_resolved_requires(
        &self,
        file: &FileKey,
        resolved_requires: crate::entity::ResolvedRequires,
    ) {
        if let Some(entry) = self.file_heap.get(file) {
            if let Some(Parse::Typed(typed)) = entry.parse_latest() {
                typed.resolved_requires.set(resolved_requires);
            }
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
        let old_sig_hash = parse.sig_hash.read_committed();

        match old_sig_hash {
            Some(old_hash) if old_hash == sig_hash => false,
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
