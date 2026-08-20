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
use flow_parser::dts_file_kind::DtsFileKind;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::package_json::PackageJson;
use flow_type_sig::packed_type_sig::Module;
use flow_type_sig::signature_error::TolerableError;

use crate::resolved_requires::Dependency;
use crate::resolved_requires::DependencyTarget;
use crate::resolved_requires::ResolvedRequires;

/// Compressed serialized bytes wrapper for heap-stored data.
/// Fields are stored as compressed bincode bytes to reduce memory footprint.
type CompressedBytes = Arc<[u8]>;

#[derive(Clone, Debug, Dupe)]
pub struct FileEntry(Arc<FileEntryData>);

#[derive(Debug)]
struct FileEntryData {
    dependency: Dependency,
    parse: Option<Parse>,
    haste_info: Option<HasteModuleInfo>,
    alternate_file: Option<FileKey>,
}

impl FileEntry {
    pub(crate) fn new(
        file_key: FileKey,
        parse: Parse,
        haste_info: Option<HasteModuleInfo>,
    ) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse: Some(parse),
            haste_info,
            alternate_file: None,
        }))
    }

    pub(crate) fn new_empty(file_key: FileKey) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse: None,
            haste_info: None,
            alternate_file: None,
        }))
    }

    pub(crate) fn new_committed(
        file_key: FileKey,
        parse: Option<Parse>,
        haste_info: Option<HasteModuleInfo>,
        alternate_file: Option<FileKey>,
    ) -> Self {
        let dependency = Dependency::new(DependencyTarget::File(file_key));
        Self(Arc::new(FileEntryData {
            dependency,
            parse,
            haste_info,
            alternate_file,
        }))
    }

    pub(crate) fn dependency(&self) -> Dependency {
        self.0.dependency.dupe()
    }

    pub(crate) fn parse(&self) -> Option<Parse> {
        self.0.parse.dupe()
    }

    pub(crate) fn parse_latest(&self) -> Option<Parse> {
        self.parse()
    }

    pub(crate) fn with_parse(&self, parse: Option<Parse>) -> Self {
        Self(Arc::new(FileEntryData {
            dependency: self.0.dependency.dupe(),
            parse,
            haste_info: self.0.haste_info.dupe(),
            alternate_file: self.0.alternate_file.dupe(),
        }))
    }

    pub(crate) fn get_haste_info(&self) -> Option<HasteModuleInfo> {
        self.0.haste_info.dupe()
    }

    pub(crate) fn with_haste_info(&self, haste_info: Option<HasteModuleInfo>) -> Self {
        Self(Arc::new(FileEntryData {
            dependency: self.0.dependency.dupe(),
            parse: self.0.parse.dupe(),
            haste_info,
            alternate_file: self.0.alternate_file.dupe(),
        }))
    }

    pub(crate) fn get_alternate_file(&self) -> Option<FileKey> {
        self.0.alternate_file.dupe()
    }

    pub(crate) fn with_alternate_file(&self, alternate_file: Option<FileKey>) -> Self {
        Self(Arc::new(FileEntryData {
            dependency: self.0.dependency.dupe(),
            parse: self.0.parse.dupe(),
            haste_info: self.0.haste_info.dupe(),
            alternate_file,
        }))
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
    pub(crate) dts_file_kind: Option<DtsFileKind>,
    pub(crate) ast: Option<CompressedBytes>,
    pub(crate) docblock: Option<CompressedBytes>,
    pub(crate) aloc_table: Option<CompressedBytes>,
    pub(crate) type_sig: Option<CompressedBytes>,
    pub(crate) file_sig: Option<CompressedBytes>,
    pub(crate) exports: CompressedBytes,
    pub(crate) requires: Arc<[FlowImportSpecifier]>,
    pub(crate) resolved_requires: Option<ResolvedRequires>,
    pub(crate) imports: CompressedBytes,
    pub(crate) leader: Option<FileKey>,
    pub(crate) sig_hash: Option<u64>,
    pub(crate) merge_hashes: Option<Arc<MergeHashes>>,
}

impl TypedParse {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        file_hash: u64,
        dts_file_kind: Option<DtsFileKind>,
        ast: Option<Arc<Program<Loc, Loc>>>,
        docblock: Option<Arc<Docblock>>,
        aloc_table: Option<Arc<PackedALocTable>>,
        type_sig: Option<Arc<Module<Loc>>>,
        file_sig: Option<(Arc<FileSig>, Arc<[TolerableError<Loc>]>)>,
        exports: Arc<Exports>,
        requires: Arc<[FlowImportSpecifier]>,
        resolved_requires: Option<ResolvedRequires>,
        imports: Arc<Imports>,
        leader: Option<FileKey>,
        sig_hash: Option<u64>,
    ) -> Self {
        Self {
            file_hash,
            dts_file_kind,
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
            merge_hashes: None,
        }
    }

    pub(crate) fn with_resolved_requires(mut self, resolved_requires: ResolvedRequires) -> Self {
        self.resolved_requires = Some(resolved_requires);
        self
    }

    pub(crate) fn with_merge_hashes(mut self, hashes: MergeHashes) -> Self {
        self.merge_hashes = Some(Arc::new(hashes));
        self
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
            .dupe()
            .expect("ResolvedRequires should be set")
    }

    pub fn leader(&self) -> Option<FileKey> {
        self.leader.dupe()
    }

    pub fn leader_unsafe(&self) -> FileKey {
        self.leader.dupe().expect("Leader should be set")
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

    /// Read per-element merge hashes. Returns None if merge hasn't run yet.
    pub fn get_merge_hashes(&self) -> Option<MergeHashes> {
        self.merge_hashes
            .as_ref()
            .map(|hashes| hashes.as_ref().clone())
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

    #[expect(dead_code)]
    pub(crate) fn dts_file_kind(&self) -> Option<DtsFileKind> {
        match self {
            Parse::Typed(typed) => typed.dts_file_kind,
            Parse::Untyped(_) | Parse::Package(_) => None,
        }
    }
}
