/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::PackedALocTable;
use flow_common::docblock::Docblock;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common_modulename::HasteModuleInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_imports_exports::exports::Exports;
use flow_imports_exports::imports::Imports;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::package_json::PackageJson;
use flow_type_sig::packed_type_sig::Module;
use flow_type_sig::signature_error::TolerableError;
use flow_utils_concurrency::locked_set::LockedSet;
use parking_lot::RwLock;

use crate::entity::Dependency;
use crate::entity::DependencyTarget;
use crate::entity::Entity;
use crate::entity::EntityTransaction;
use crate::entity::ResolvedRequires;

/// Compressed serialized bytes wrapper for heap-stored data.
/// Fields are stored as compressed bincode bytes to reduce memory footprint.
type CompressedBytes = Arc<[u8]>;

#[derive(Clone, Debug, Dupe)]
pub struct FileEntry(Arc<FileEntryData>);

#[derive(Debug)]
struct FileEntryData {
    dependency: Dependency,
    parse: Entity<Parse>,
    haste_info: Entity<HasteModuleInfo>,
    dependents: Option<LockedSet<FileKey>>,
    alternate_file: RwLock<Option<FileKey>>,
}

impl FileEntry {
    pub(crate) fn new(
        file_key: FileKey,
        transaction: EntityTransaction,
        parse: Parse,
        haste_info: Option<HasteModuleInfo>,
        has_dependents: bool,
    ) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse: Entity::new(transaction.dupe(), parse),
            haste_info: if let Some(info) = haste_info {
                Entity::new(transaction, info)
            } else {
                Entity::empty(transaction)
            },
            dependents: if has_dependents {
                Some(LockedSet::new())
            } else {
                None
            },
            alternate_file: RwLock::new(None),
        }))
    }

    /// Create a phantom file entry that only tracks dependents.
    /// OCaml's prepare_find_or_add_phantom_file creates these for files that
    /// don't exist yet but are referenced as dependencies. The entry has no
    /// parse data and no haste info — it exists solely to hold the dependents
    /// list so that reverse-dep edges survive until the file is actually created.
    pub(crate) fn new_phantom(file_key: FileKey, transaction: EntityTransaction) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse: Entity::empty(transaction.dupe()),
            haste_info: Entity::empty(transaction),
            dependents: Some(LockedSet::new()),
            alternate_file: RwLock::new(None),
        }))
    }

    pub(crate) fn new_committed(
        file_key: FileKey,
        transaction: EntityTransaction,
        parse: Option<Parse>,
        haste_info: Option<HasteModuleInfo>,
        dependents: Option<LockedSet<FileKey>>,
        alternate_file: Option<FileKey>,
    ) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse: match parse {
                Some(parse) => Entity::new_committed(transaction.dupe(), parse),
                None => Entity::empty_committed(transaction.dupe()),
            },
            haste_info: match haste_info {
                Some(info) => Entity::new_committed(transaction, info),
                None => Entity::empty_committed(transaction),
            },
            dependents,
            alternate_file: RwLock::new(alternate_file),
        }))
    }

    pub(crate) fn parse(&self) -> &Entity<Parse> {
        &self.0.parse
    }

    pub(crate) fn dependency(&self) -> Dependency {
        self.0.dependency.dupe()
    }

    pub(crate) fn parse_latest(&self) -> Option<Parse> {
        self.0.parse.read_latest()
    }

    pub(crate) fn parse_committed(&self) -> Option<Parse> {
        self.0.parse.read_committed()
    }

    pub(crate) fn parse_has_changed(&self) -> bool {
        self.0.parse.has_changed()
    }

    pub(crate) fn haste_info_entity(&self) -> &Entity<HasteModuleInfo> {
        &self.0.haste_info
    }

    pub(crate) fn get_haste_info_latest(&self) -> Option<HasteModuleInfo> {
        self.0.haste_info.read_latest_clone()
    }

    pub(crate) fn get_haste_info_committed(&self) -> Option<HasteModuleInfo> {
        self.0.haste_info.read_committed_clone()
    }

    #[expect(dead_code)]
    pub(crate) fn set_haste_info(&self, info: Option<HasteModuleInfo>) {
        self.0.haste_info.advance(info);
    }

    pub(crate) fn add_dependent(&self, dependent: FileKey) {
        if let Some(deps) = &self.0.dependents {
            deps.insert(dependent);
        }
    }

    pub(crate) fn remove_dependent(&self, dependent: &FileKey) {
        if let Some(deps) = &self.0.dependents {
            deps.remove(dependent);
        }
    }

    pub(crate) fn get_dependents(&self) -> Option<Vec<FileKey>> {
        self.0.dependents.as_ref().map(|deps| deps.iter().collect())
    }

    pub(crate) fn has_dependents(&self) -> bool {
        self.0
            .dependents
            .as_ref()
            .is_some_and(|deps| !deps.is_empty())
    }

    pub(crate) fn get_alternate_file(&self) -> Option<FileKey> {
        self.0.alternate_file.read().dupe()
    }

    pub(crate) fn set_alternate_file(&self, alternate: Option<FileKey>) {
        *self.0.alternate_file.write() = alternate;
    }
}

/// Per-element hashes computed by cycle_hash during merge.
/// In OCaml, these are stored in the mutable binary type_sig buffer.
/// In Rust, we store them separately since our type_sig is immutable.
/// These hashes incorporate transitive dependency information and are
/// read by acyclic_dep when a subsequent component depends on this file.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MergeHashes {
    CJS {
        type_export_hashes: Vec<(FlowSmolStr, u64)>,
        exports_hash: Option<u64>,
        ns_hash: u64,
    },
    ES {
        type_export_hashes: Vec<(FlowSmolStr, u64)>,
        export_hashes: Vec<(FlowSmolStr, u64)>,
        ns_hash: u64,
    },
}

#[derive(Debug, Clone, Dupe)]
pub struct TypedParse {
    pub(crate) file_hash: u64,
    pub(crate) ast: Option<CompressedBytes>,
    pub(crate) docblock: Option<CompressedBytes>,
    pub(crate) aloc_table: Option<CompressedBytes>,
    pub(crate) type_sig: Option<CompressedBytes>,
    pub(crate) file_sig: Option<CompressedBytes>,
    pub(crate) exports: CompressedBytes,
    pub(crate) requires: Arc<[FlowImportSpecifier]>,
    pub(crate) resolved_requires: Arc<Entity<ResolvedRequires>>,
    pub(crate) imports: CompressedBytes,
    pub(crate) leader: Arc<Entity<FileKey>>,
    pub(crate) sig_hash: Arc<Entity<u64>>,
    pub(crate) merge_hashes: Arc<RwLock<Option<MergeHashes>>>,
}

impl TypedParse {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        file_hash: u64,
        ast: Option<Arc<Program<Loc, Loc>>>,
        docblock: Option<Arc<Docblock>>,
        aloc_table: Option<Arc<PackedALocTable>>,
        type_sig: Option<Arc<Module<Loc>>>,
        file_sig: Option<(Arc<FileSig>, Arc<[TolerableError<Loc>]>)>,
        exports: Arc<Exports>,
        requires: Arc<[FlowImportSpecifier]>,
        resolved_requires: Arc<Entity<ResolvedRequires>>,
        imports: Arc<Imports>,
        leader: Arc<Entity<FileKey>>,
        sig_hash: Arc<Entity<u64>>,
    ) -> Self {
        Self {
            file_hash,
            ast: ast.map(|a| Arc::from(flow_heap_serialization::serialize_ast(&a))),
            docblock: docblock.map(|d| Arc::from(flow_heap_serialization::serialize_docblock(&d))),
            aloc_table: aloc_table
                .map(|a| Arc::from(flow_heap_serialization::serialize_aloc_table(&a))),
            type_sig: type_sig.map(|t| Arc::from(flow_heap_serialization::serialize_type_sig(&t))),
            file_sig: file_sig.map(|(f, e)| {
                Arc::from(flow_heap_serialization::serialize_file_sig_with_errors(
                    &f, &e,
                ))
            }),
            exports: Arc::from(flow_heap_serialization::serialize_exports(&exports)),
            requires,
            resolved_requires,
            imports: Arc::from(flow_heap_serialization::serialize_imports(&imports)),
            leader,
            sig_hash,
            merge_hashes: Arc::new(RwLock::new(None)),
        }
    }

    pub fn ast_unsafe(&self, file: &FileKey) -> Arc<Program<Loc, Loc>> {
        match &self.ast {
            Some(bytes) => flow_heap_serialization::deserialize_ast(file, bytes),
            None => panic!("AST not found for file: {}", file.as_str()),
        }
    }

    pub fn has_ast(&self) -> bool {
        self.ast.is_some()
    }

    pub fn tolerable_file_sig_unsafe(
        &self,
        file: &FileKey,
    ) -> (Arc<FileSig>, Arc<[TolerableError<Loc>]>) {
        match &self.file_sig {
            Some(bytes) => flow_heap_serialization::deserialize_file_sig_with_errors(file, bytes),
            None => panic!("File sig not found for file: {}", file.as_str()),
        }
    }

    pub fn aloc_table_unsafe(&self, file: &FileKey) -> Arc<PackedALocTable> {
        match &self.aloc_table {
            Some(bytes) => flow_heap_serialization::deserialize_aloc_table(bytes),
            None => panic!("ALocTable not found for file: {}", file.as_str()),
        }
    }

    pub fn type_sig_unsafe(&self, file: &FileKey) -> Arc<Module<Loc>> {
        match &self.type_sig {
            Some(bytes) => flow_heap_serialization::deserialize_type_sig(file, bytes),
            None => panic!("Type signature not found for file: {}", file.as_str()),
        }
    }

    pub fn requires(&self) -> Arc<[FlowImportSpecifier]> {
        self.requires.dupe()
    }

    pub fn resolved_requires_unsafe(&self) -> ResolvedRequires {
        self.resolved_requires
            .read_latest_clone()
            .expect("ResolvedRequires should be set")
    }

    pub fn leader_unsafe(&self) -> FileKey {
        self.leader.get()
    }

    pub fn docblock_unsafe(&self, file: &FileKey) -> Arc<Docblock> {
        match &self.docblock {
            Some(bytes) => flow_heap_serialization::deserialize_docblock(file, bytes),
            None => panic!("Docblock not found for file: {}", file.as_str()),
        }
    }

    /// Deserialize exports from compressed bytes
    pub fn exports_unsafe(&self) -> Arc<Exports> {
        flow_heap_serialization::deserialize_exports(&self.exports)
    }

    /// Deserialize imports from compressed bytes
    pub fn imports_unsafe(&self) -> Arc<Imports> {
        flow_heap_serialization::deserialize_imports(&self.imports)
    }

    /// Store per-element hashes computed by cycle_hash during merge.
    pub fn set_merge_hashes(&self, hashes: MergeHashes) {
        *self.merge_hashes.write() = Some(hashes);
    }

    /// Read per-element merge hashes. Returns None if merge hasn't run yet.
    pub fn get_merge_hashes(&self) -> Option<MergeHashes> {
        self.merge_hashes.read().clone()
    }
}

#[derive(Debug, Clone, Dupe)]
pub struct UntypedParse {
    pub(crate) file_hash: u64,
}

impl UntypedParse {
    pub(crate) fn new(file_hash: u64) -> Self {
        Self { file_hash }
    }
}

#[derive(Clone, Debug, Dupe)]
pub struct PackageParse {
    pub(crate) file_hash: u64,
    pub(crate) package_info: Arc<PackageJson>,
}

impl PackageParse {
    pub(crate) fn new(file_hash: u64, package_info: Arc<PackageJson>) -> Self {
        Self {
            file_hash,
            package_info,
        }
    }
}

#[derive(Clone, Debug, Dupe)]
pub enum Parse {
    Typed(TypedParse),
    Untyped(UntypedParse),
    Package(PackageParse),
}

impl Parse {
    pub(crate) fn is_typed(&self) -> bool {
        matches!(self, Parse::Typed(_))
    }

    #[expect(dead_code)]
    pub(crate) fn is_untyped(&self) -> bool {
        matches!(self, Parse::Untyped(_))
    }

    pub(crate) fn is_package(&self) -> bool {
        matches!(self, Parse::Package(_))
    }

    #[expect(dead_code)]
    pub(crate) fn as_typed(&self) -> Option<&TypedParse> {
        match self {
            Parse::Typed(typed) => Some(typed),
            _ => None,
        }
    }

    #[expect(dead_code)]
    pub(crate) fn as_package(&self) -> Option<&PackageParse> {
        match self {
            Parse::Package(pkg) => Some(pkg),
            _ => None,
        }
    }

    pub(crate) fn get_file_hash(&self) -> u64 {
        match self {
            Parse::Typed(typed) => typed.file_hash,
            Parse::Untyped(untyped) => untyped.file_hash,
            Parse::Package(pkg) => pkg.file_hash,
        }
    }
}
