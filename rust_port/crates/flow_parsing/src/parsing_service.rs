/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::docblock::Docblock;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::options::Options;
use flow_common::platform_set;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_imports_exports::exports;
use flow_imports_exports::exports::Exports;
use flow_imports_exports::imports;
use flow_imports_exports::imports::Imports;
use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::ParseOptions;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::parse_error::ParseError;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::FileSigOptions;
use flow_parser_utils::package_json::PackageJson;
use flow_services_module::PackageInfo;
use flow_type_sig::signature_error::TolerableError;
use flow_type_sig::type_sig::Errno;
use flow_type_sig::type_sig_options::TypeSigOptions;
use flow_type_sig::type_sig_utils;
use flow_utils_concurrency::map_reduce;
use flow_utils_concurrency::thread_pool::ThreadPool;
use map_reduce::Next as _;
use vec1::Vec1;

use crate::docblock_parser::DocblockError;

pub type Next = Box<dyn FnMut() -> Option<Vec<FileKey>> + Send>;

// result of individual parse
#[derive(Debug)]
pub enum ParseResult {
    ParseOk {
        ast: Program<Loc, Loc>,
        requires: Vec<FlowImportSpecifier>,
        file_sig: Arc<FileSig>,
        locs: flow_type_sig::compact_table::Table<Loc>,
        type_sig: flow_type_sig::packed_type_sig::Module<Loc>,
        tolerable_errors: Vec<TolerableError<Loc>>,
        exports: Exports,
        imports: Imports,
    },
    ParseRecovered {
        ast: Program<Loc, Loc>,
        requires: Vec<FlowImportSpecifier>,
        file_sig: Arc<FileSig>,
        tolerable_errors: Vec<TolerableError<Loc>>,
        parse_errors: Vec1<(Loc, ParseError)>,
    },
    ParseExn(String),
    ParseSkip(ParseSkipReason),
}

#[derive(Debug)]
pub enum ParseSkipReason {
    SkipResourceFile,
    SkipNonFlowFile,
    SkipPackageJson(Result<PackageJson, (Loc, ParseError)>),
}

#[derive(Debug, Clone)]
pub enum ParseFailure {
    UncaughtException(String),
    DocblockErrors(Vec<DocblockError>),
    ParseError((Loc, ParseError)),
}

// results of parse job, returned by parse and reparse
#[derive(Debug, Default, Clone)]
pub struct ParseResults {
    // successfully parsed files
    pub parsed: FlowOrdSet<FileKey>,
    // list of skipped files
    pub unparsed: FlowOrdSet<FileKey>,
    // list of files skipped due to an out of date hash
    pub changed: FlowOrdSet<FileKey>,
    // list of failed files
    pub failed: (Vec<FileKey>, Vec<ParseFailure>),
    // set of unchanged files
    pub unchanged: FlowOrdSet<FileKey>,
    // set of files that were not found on disk
    pub not_found: FlowOrdSet<FileKey>,
    // package.json files parsed
    pub package_json: (Vec<FileKey>, Vec<Option<(Loc, ParseError)>>),
    // set of modules that need to be committed
    pub dirty_modules: BTreeSet<Modulename>,
}

// **************************** internal ********************************
fn parse_source_file(
    options: &Options,
    content: Result<&str, ()>,
    file: &FileKey,
) -> (Program<Loc, Loc>, Vec<(Loc, ParseError)>) {
    let use_strict = if file.is_lib_file() {
        // lib files are always "use strict"
        true
    } else {
        options.modules_are_use_strict
    };
    let assert_operator = options.assert_operator.parse();
    let parse_options = Some(ParseOptions {
        use_strict,
        assert_operator,
        module_ref_prefix: options.haste_module_ref_prefix.dupe(),
        ..PERMISSIVE_PARSE_OPTIONS
    });

    flow_parser::parse_program_file::<()>(false, None, parse_options, file.dupe(), content)
}

pub fn parse_package_json_file(
    options: &Options,
    content: Result<&str, ()>,
    file: &FileKey,
) -> Result<PackageJson, (Loc, ParseError)> {
    let node_main_fields: Vec<_> = options
        .node_main_fields
        .iter()
        .map(flow_data_structure_wrapper::smol_str::FlowSmolStr::new)
        .collect();
    let parse_options = None;

    let ((_loc, obj), mut parse_errors) = flow_parser::parse_package_json_file(
        false,
        None,
        parse_options,
        Some(file.dupe()),
        content,
    );

    if !parse_errors.is_empty() {
        Err(parse_errors.swap_remove(0))
    } else {
        Ok(PackageJson::parse(&node_main_fields, &obj))
    }
}

// Allow types based on `types_mode`, using the @flow annotation in the
// file header if possible. Note, this should be consistent with
// Infer_service.apply_docblock_overrides w.r.t. the metadata.checked flag.
fn types_checked(options: &Options, file: &FileKey, docblock: &Docblock) -> bool {
    if file.is_lib_file() {
        // types are always allowed in lib files
        true
    } else if flow_common::files::has_ts_ext(file) {
        // .ts files are always typed
        true
    } else {
        use flow_common::docblock::FlowMode;
        match &docblock.flow {
            None => options.all,
            Some(FlowMode::OptOut) => false,
            Some(FlowMode::OptIn | FlowMode::OptInStrict | FlowMode::OptInStrictLocal) => true,
        }
    }
}

pub fn parse_file_sig(
    options: &Options,
    file: &FileKey,
    docblock: &Docblock,
    ast: &Program<Loc, Loc>,
) -> FileSig {
    let enable_relay_integration = options.enable_relay_integration
        && flow_common::relay_options::enabled_for_file(&options.relay_integration_excludes, file);

    let relay_integration_module_prefix = flow_common::relay_options::module_prefix_for_file(
        &options.relay_integration_module_prefix_includes,
        file,
        options.relay_integration_module_prefix.as_deref(),
    )
    .map(FlowSmolStr::new);

    let file_sig_opts = FileSigOptions {
        enable_enums: options.enums,
        enable_jest_integration: options.enable_jest_integration,
        enable_relay_integration,
        explicit_available_platforms: docblock.supports_platform.clone(),
        file_options: options.file_options.dupe(),
        haste_module_ref_prefix: options.haste_module_ref_prefix.dupe(),
        project_options: options.projects_options.dupe(),
        relay_integration_module_prefix,
    };

    FileSig::from_program(file, ast, &file_sig_opts)
}

pub fn parse_type_sig<'arena, 'ast>(
    options: &Options,
    docblock: &Docblock,
    locs_to_dirtify: Vec<Loc>,
    file: &FileKey,
    ast: &'ast Program<Loc, Loc>,
    arena: &'arena bumpalo::Bump,
) -> (
    Vec<flow_type_sig::type_sig::Errno<flow_type_sig::compact_table::Index<Loc>>>,
    flow_type_sig::compact_table::Table<Loc>,
    flow_type_sig::packed_type_sig::Module<Loc>,
) {
    let sig_opts =
        TypeSigOptions::of_options(options, docblock.prevent_munge, locs_to_dirtify, file);
    let strict = docblock.is_strict();
    let platform_availability_set = platform_set::available_platforms(
        &options.file_options,
        &options.projects_options,
        file.as_str(),
        docblock.supports_platform.as_deref(),
    );

    type_sig_utils::parse_and_pack_module(
        &sig_opts,
        arena,
        strict,
        platform_availability_set,
        Some(file.dupe()),
        ast,
    )
}

// parse contents of a file
pub fn do_parse(
    options: &Options,
    docblock: &Docblock,
    locs_to_dirtify: &[Loc],
    content: Result<&str, ()>,
    file: &FileKey,
) -> ParseResult {
    use flow_parser::file_key::FileKeyInner;

    match file.inner() {
        FileKeyInner::JsonFile(path) => {
            if path.ends_with("package.json") {
                let result = parse_package_json_file(options, content, file);
                ParseResult::ParseSkip(ParseSkipReason::SkipPackageJson(result))
            } else {
                ParseResult::ParseSkip(ParseSkipReason::SkipResourceFile)
            }
        }
        FileKeyInner::ResourceFile(_) => ParseResult::ParseSkip(ParseSkipReason::SkipResourceFile),
        FileKeyInner::LibFile(_) | FileKeyInner::SourceFile(_) => {
            // either all=true or @flow pragma exists
            if !types_checked(options, file, docblock) {
                ParseResult::ParseSkip(ParseSkipReason::SkipNonFlowFile)
            } else {
                let (ast, parse_errors) = parse_source_file(options, content, file);
                let file_sig = Arc::new(parse_file_sig(options, file, docblock, &ast));
                let requires: Vec<FlowImportSpecifier> =
                    file_sig.require_loc_map().into_keys().collect();

                if let Ok(parse_errors) = Vec1::try_from_vec(parse_errors) {
                    ParseResult::ParseRecovered {
                        ast,
                        requires,
                        file_sig,
                        tolerable_errors: vec![],
                        parse_errors,
                    }
                } else {
                    let arena = bumpalo::Bump::new();
                    let locs_to_dirtify_vec = locs_to_dirtify.to_vec();
                    let (sig_errors, locs, type_sig) =
                        parse_type_sig(options, docblock, locs_to_dirtify_vec, file, &ast, &arena);

                    let exports_result = exports::of_module(&type_sig);
                    let imports_result = imports::of_file_sig(&file_sig);
                    let tolerable_errors = sig_errors
                        .into_iter()
                        .filter_map(|err| match err {
                            Errno::SigError(sig_err) => {
                                let mapped_err =
                                    sig_err.map(&mut (), |_, index| locs.get(*index).dupe());
                                Some(TolerableError::SignatureVerificationError(mapped_err))
                            }
                            Errno::BindingValidationError(bind_err) => {
                                let mapped_err =
                                    bind_err.map(&mut (), |_, index| locs.get(*index).dupe());
                                Some(TolerableError::SignatureBindingValidationError(mapped_err))
                            }
                            Errno::CheckError => None,
                        })
                        .collect();

                    ParseResult::ParseOk {
                        ast,
                        requires,
                        file_sig,
                        locs,
                        type_sig,
                        tolerable_errors,
                        exports: exports_result,
                        imports: imports_result,
                    }
                }
            }
        }
    }
}

fn hash_content(content: &[u8]) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;

    let mut hasher = DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

fn content_hash_matches_file_hash(
    shared_mem: &SharedMem,
    file: &FileKey,
    content_hash: u64,
) -> bool {
    shared_mem
        .get_parse(file)
        .map(|_| shared_mem.get_file_hash_unsafe(file) == content_hash)
        .unwrap_or(false)
}

fn content_hash_matches_old_file_hash(
    shared_mem: &SharedMem,
    file: &FileKey,
    content_hash: u64,
) -> bool {
    shared_mem
        .get_file_hash(file)
        .map(|old_hash| old_hash == content_hash)
        .unwrap_or(false)
}

pub fn does_content_match_file_hash(shared_mem: &SharedMem, file: &FileKey, content: &str) -> bool {
    let content_hash = hash_content(content.as_bytes());
    content_hash_matches_file_hash(shared_mem, file, content_hash)
}

fn fold_failed(
    acc: &mut ParseResults,
    shared_mem: &SharedMem,
    file_key: FileKey,
    hash: u64,
    haste_module_info: Option<HasteModuleInfo>,
    error: ParseFailure,
) {
    let dirty_modules = shared_mem.add_unparsed(file_key.dupe(), hash, haste_module_info);

    acc.failed.0.push(file_key);
    acc.failed.1.push(error);
    acc.dirty_modules.extend(dirty_modules);
}

// parse file, store AST to shared heap on success.
// Add success/error info to passed accumulator.
fn reducer(
    shared_mem: &SharedMem,
    options: &Options,
    skip_changed: bool,
    skip_unchanged: bool,
    is_init: bool,
    locs_to_dirtify: &[Loc],
    acc: &mut ParseResults,
    file_key: FileKey,
) {
    if is_init
        && shared_mem.get_parse(&file_key).is_some()
        && shared_mem.get_parse_committed(&file_key).is_none()
    {
        return;
    }

    let filename_string = file_key.to_absolute();

    let bytes = match std::fs::read(&filename_string) {
        Ok(bytes) => bytes,
        Err(_) => {
            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());
            let dirty_modules = shared_mem.clear_file(file_key.dupe(), haste_module_info);
            acc.not_found.insert(file_key);
            acc.dirty_modules.extend(dirty_modules);
            return;
        }
    };

    let hash = hash_content(&bytes);

    if skip_changed && !content_hash_matches_file_hash(shared_mem, &file_key, hash) {
        acc.changed.insert(file_key);
        return;
    }

    if skip_unchanged && content_hash_matches_old_file_hash(shared_mem, &file_key, hash) {
        acc.unchanged.insert(file_key);
        return;
    }

    let content_str: Result<&str, ()> = std::str::from_utf8(&bytes).map_err(|_| ());

    // When the bytes aren't valid UTF-8 we cannot run `parse_docblock`
    // here. We still want `do_parse` to produce the `MalformedUnicode`
    // parse error (it does, via `init_env` with `Err(())`), so we fall back
    // to a default `Docblock` and continue. The parse error path is the
    // single source of truth for the "malformed unicode" diagnostic.
    let mut docblock = if let Ok(content) = content_str {
        let (docblock_errors, docblock) = crate::docblock_parser::parse_docblock(
            options.max_header_tokens as usize,
            &options.file_options,
            &file_key,
            content,
        );

        if !docblock_errors.is_empty() {
            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());
            fold_failed(
                acc,
                shared_mem,
                file_key,
                hash,
                haste_module_info,
                ParseFailure::DocblockErrors(docblock_errors),
            );
            return;
        }
        docblock
    } else {
        Docblock::default()
    };

    if files::is_untyped(&options.file_options, &file_key.to_absolute()) {
        docblock.flow = Some(flow_common::docblock::FlowMode::OptOut);
    }

    match do_parse(options, &docblock, locs_to_dirtify, content_str, &file_key) {
        ParseResult::ParseOk {
            ast,
            requires: _,
            file_sig,
            locs,
            type_sig,
            tolerable_errors,
            exports,
            imports,
        } => {
            use flow_aloc::aloc_representation_do_not_use;

            let aloc_table =
                aloc_representation_do_not_use::make_table(file_key.dupe(), locs.into_vec());
            let packed_aloc_table = aloc_table.pack();

            let requires_vec: Vec<_> = file_sig.require_loc_map().into_keys().collect();

            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());

            let dirty_modules = shared_mem.add_parsed(
                file_key.dupe(),
                hash,
                haste_module_info,
                Some(Arc::new(ast)),
                Some(Arc::new(docblock)),
                Some(Arc::new(packed_aloc_table)),
                Some(Arc::new(type_sig)),
                Some((file_sig, Arc::from(tolerable_errors))),
                Arc::new(exports),
                Arc::from(requires_vec),
                Arc::new(imports),
            );
            acc.parsed.insert(file_key);
            acc.dirty_modules.extend(dirty_modules);
        }
        ParseResult::ParseRecovered { parse_errors, .. } => {
            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());
            let error = parse_errors.first().clone();
            fold_failed(
                acc,
                shared_mem,
                file_key,
                hash,
                haste_module_info,
                ParseFailure::ParseError(error),
            );
        }
        ParseResult::ParseExn(exn) => {
            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());
            fold_failed(
                acc,
                shared_mem,
                file_key,
                hash,
                haste_module_info,
                ParseFailure::UncaughtException(exn),
            );
        }
        ParseResult::ParseSkip(ParseSkipReason::SkipPackageJson(result)) => match result {
            Ok(package_json) => {
                let package_info = PackageInfo::new(Some(package_json.clone()));
                let haste_module_info =
                    flow_services_module::exported_module(options, &file_key, &package_info);
                let dirty_modules = shared_mem.add_package(
                    file_key.dupe(),
                    hash,
                    haste_module_info,
                    Arc::new(package_json),
                );
                acc.dirty_modules.extend(dirty_modules);
            }
            Err(parse_error) => {
                acc.package_json.0.push(file_key.dupe());
                acc.package_json.1.push(Some(parse_error));
                let dirty_modules = shared_mem.add_unparsed(file_key, hash, None);
                acc.dirty_modules.extend(dirty_modules);
            }
        },
        ParseResult::ParseSkip(ParseSkipReason::SkipNonFlowFile)
        | ParseResult::ParseSkip(ParseSkipReason::SkipResourceFile) => {
            let haste_module_info =
                flow_services_module::exported_module(options, &file_key, &PackageInfo::none());
            let dirty_modules = shared_mem.add_unparsed(file_key.dupe(), hash, haste_module_info);
            acc.unparsed.insert(file_key);
            acc.dirty_modules.extend(dirty_modules);
        }
    }
}

// merge is just memberwise union/concat of results
fn merge(a: &mut ParseResults, b: ParseResults) {
    for file in b.parsed.iter() {
        a.parsed.insert(file.dupe());
    }
    for file in b.unparsed.iter() {
        a.unparsed.insert(file.dupe());
    }
    for file in b.changed.iter() {
        a.changed.insert(file.dupe());
    }
    a.failed.0.extend(b.failed.0);
    a.failed.1.extend(b.failed.1);
    for file in b.unchanged.iter() {
        a.unchanged.insert(file.dupe());
    }
    for file in b.not_found.iter() {
        a.not_found.insert(file.dupe());
    }
    a.package_json.0.extend(b.package_json.0);
    a.package_json.1.extend(b.package_json.1);
    a.dirty_modules.extend(b.dirty_modules);
}

// ***************************** public ********************************

pub fn next_of_filename_set(
    pool: &ThreadPool,
    filenames: Vec<FileKey>,
    progress_fn: Option<
        impl Fn(/*total:*/ i32, /*start:*/ i32, /*length:*/ i32) + Send + Sync + 'static,
    >,
) -> Next {
    match progress_fn {
        Some(progress_fn) => {
            let bucket_next =
                map_reduce::make_next(pool.num_workers(), Some(progress_fn), None, filenames);
            Box::new(move || match bucket_next.next() {
                map_reduce::Bucket::Job(batch) => Some(batch),
                map_reduce::Bucket::Wait | map_reduce::Bucket::Done => None,
            })
        }
        None => {
            let bucket_next = map_reduce::make_next(
                pool.num_workers(),
                None::<fn(i32, i32, i32)>,
                None,
                filenames,
            );
            Box::new(move || match bucket_next.next() {
                map_reduce::Bucket::Job(batch) => Some(batch),
                map_reduce::Bucket::Wait | map_reduce::Bucket::Done => None,
            })
        }
    }
}

fn parse(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    skip_changed: bool,
    skip_unchanged: bool,
    is_init: bool,
    locs_to_dirtify: &[Loc],
    mut next: Next,
) -> ParseResults {
    let shared_mem_for_job = shared_mem.clone();
    let options_for_job = options.clone();
    let locs_to_dirtify_vec = locs_to_dirtify.to_vec();

    let mut results = ParseResults::default();
    let t = std::time::Instant::now();

    while let Some(batch) = next() {
        let batch_results = map_reduce::fold(
            pool,
            batch,
            {
                let shared_mem = shared_mem_for_job.clone();
                let options = options_for_job.clone();
                let locs = locs_to_dirtify_vec.clone();
                move |acc, file_key| {
                    reducer(
                        &shared_mem,
                        &options,
                        skip_changed,
                        skip_unchanged,
                        is_init,
                        &locs,
                        acc,
                        file_key.clone(),
                    );
                }
            },
            merge,
        );
        merge(&mut results, batch_results);
    }

    if options.profile {
        let elapsed = t.elapsed().as_secs_f64();
        let num_parsed = results.parsed.len();
        let num_unparsed = results.unparsed.len();
        let num_changed = results.changed.len();
        let num_failed = results.failed.0.len();
        let num_unchanged = results.unchanged.len();
        let num_not_found = results.not_found.len();
        let total =
            num_parsed + num_unparsed + num_changed + num_failed + num_unchanged + num_not_found;
        flow_hh_logger::info!(
            "parsed {} files ({} ok, {} skipped, {} not found, {} bad hashes, {} failed, {} unchanged) in {}",
            total,
            num_parsed,
            num_unparsed,
            num_not_found,
            num_changed,
            num_failed,
            num_unchanged,
            elapsed
        );
    }

    results
}

pub fn parse_with_defaults(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    locs_to_dirtify: &[Loc],
    next: Next,
) -> ParseResults {
    parse(
        pool,
        shared_mem,
        options,
        false, // skip_changed
        false, // skip_unchanged
        true,  // is_init
        locs_to_dirtify,
        next,
    )
}

pub fn reparse_with_defaults(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    locs_to_dirtify: &[Loc],
    next: Next,
) -> ParseResults {
    let skip_unchanged = locs_to_dirtify.is_empty();
    parse(
        pool,
        shared_mem,
        options,
        false, // skip_changed
        skip_unchanged,
        false, // is_init (reparse = not init)
        locs_to_dirtify,
        next,
    )
}

// ensure_parsed takes a set of files, finds the files which haven't been parsed, and parses them.
// Any not-yet-parsed files whose on-disk contents don't match their already-known hash are skipped
// and returned to the caller.
pub fn ensure_parsed(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    files: FlowOrdSet<FileKey>,
    progress_fn: impl Fn(/*total:*/ i32, /*start:*/ i32, /*length:*/ i32) + Send + Sync + 'static,
) -> FlowOrdSet<FileKey> {
    let files_vec: Vec<FileKey> = files.into_iter().collect();
    let files_missing_asts: FlowOrdSet<FileKey> = {
        let shared_mem = shared_mem.dupe();
        let num_workers = pool.num_workers();
        let chunk_size = if files_vec.is_empty() {
            1
        } else {
            1.max((files_vec.len() / num_workers) + 1)
        };
        let chunks: Vec<Vec<FileKey>> = files_vec
            .chunks(chunk_size)
            .map(|chunk| chunk.to_vec())
            .collect();

        map_reduce::fold(
            pool,
            chunks,
            {
                let shared_mem = shared_mem.dupe();
                move |acc: &mut FlowOrdSet<FileKey>, chunk: &Vec<FileKey>| {
                    for file in chunk {
                        if !shared_mem.has_ast(file) {
                            acc.insert(file.dupe());
                        }
                    }
                }
            },
            |a, b| {
                a.extend(b);
            },
        )
    };
    let next: Next = {
        let bucket_next = map_reduce::make_next(
            pool.num_workers(),
            Some(progress_fn),
            None,
            files_missing_asts.into_iter().collect(),
        );
        Box::new(move || match bucket_next.next() {
            map_reduce::Bucket::Job(batch) => Some(batch),
            map_reduce::Bucket::Wait | map_reduce::Bucket::Done => None,
        })
    };

    let results = parse(pool, shared_mem, options, true, false, false, &[], next);
    // On Windows, OCaml's C runtime can't access paths >= 260 chars (MAX_PATH).
    // These files will never be readable, so retrying them is pointless and
    // causes an infinite cancel/retry loop. Filter them out — they were already
    // skipped during file discovery (see kind_of_path in files.ml).
    let not_found = if cfg!(windows) {
        results
            .not_found
            .into_inner()
            .into_iter()
            .filter(|f| f.to_absolute().len() < 248)
            .collect()
    } else {
        results.not_found.into_inner()
    };
    results.changed.into_inner().union(not_found).into()
}
