/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use dupe::Dupe;
use flow_common::packed_locs;
use flow_common::platform_set::PlatformSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

use crate::compact_table::Index;
use crate::compact_table::Table;
use crate::packed_type_sig::Builtins;
use crate::packed_type_sig::Module;
use crate::packed_type_sig::ModuleDef;
use crate::signature_error::BindingValidation;
use crate::type_sig::Errno;
use crate::type_sig_mark as mark;
use crate::type_sig_options::TypeSigOptions;
use crate::type_sig_pack as pack;
use crate::type_sig_parse as parse;

fn parse_libs<'arena: 'ast, 'ast>(
    opts: &TypeSigOptions,
    arenas: &'arena bumpalo::Bump,
    ordered_asts: &[&'ast Program<Loc, Loc>],
) -> (
    parse::scope::Scopes<'arena, 'ast>,
    parse::Tables<'arena, 'ast>,
    (
        BTreeMap<FlowSmolStr, parse::BindingNode<'arena, 'ast>>,
        BTreeMap<FlowSmolStr, parse::BindingNode<'arena, 'ast>>,
        BTreeMap<
            FlowSmolStr,
            (
                parse::LocNode<'ast>,
                Rc<RefCell<parse::Exports<'arena, 'ast>>>,
            ),
        >,
    ),
) {
    let mut scopes = parse::scope::Scopes::new();
    let global_scope = parse::scope::create_global(&mut scopes);
    let mut tbls = parse::Tables::new(arenas);
    for ast in ordered_asts {
        // For builtins, each lib file is parsed separately into an unresolved global
        // scope. The parsed lib files are then combined before resolving names. The
        // proceses of combining deals with overridden definitions. Finally, the
        // resolved builtins are merged. Declared modules can depend on each other, so
        // they are treated like a cycle.
        for stmt in ast.statements.iter() {
            parse::statement(opts, global_scope, &mut scopes, &mut tbls, stmt);
        }
    }
    parse::scope::bind_global_this(&mut scopes, &mut tbls, global_scope, LOC_NONE);
    let builtins = parse::scope::builtins_exn(&scopes, global_scope);
    (scopes, tbls, builtins)
}

fn create_pack_cx(additional_errors: Vec<BindingValidation<parse::LocNode<'_>>>) -> pack::Cx {
    pack::Cx::new(
        additional_errors
            .into_iter()
            .map(|e| {
                Errno::BindingValidationError(Box::new(
                    e.map(&mut (), |_, node| node.0.index_exn()),
                ))
            })
            .collect(),
    )
}

fn pack_builtins<'arena: 'ast, 'ast>(
    opts: &TypeSigOptions,
    mut scopes: parse::scope::Scopes<'arena, 'ast>,
    mut tbls: parse::Tables<'arena, 'ast>,
    (global_values, global_types, global_modules): (
        BTreeMap<FlowSmolStr, parse::BindingNode<'arena, 'ast>>,
        BTreeMap<FlowSmolStr, parse::BindingNode<'arena, 'ast>>,
        BTreeMap<
            FlowSmolStr,
            (
                parse::LocNode<'ast>,
                Rc<RefCell<parse::Exports<'arena, 'ast>>>,
            ),
        >,
    ),
) -> (Vec<Errno<Index<Loc>>>, Table<Loc>, Builtins<Loc>) {
    // mark - forces lazys, which may append to tbls.additional_errors
    let mut marker = mark::Marker::new(&[]);
    for b in global_values.values() {
        mark::mark_binding(opts, &mut scopes, &mut tbls, &mut marker, b);
    }
    for b in global_types.values() {
        mark::mark_binding(opts, &mut scopes, &mut tbls, &mut marker, b);
    }
    for (loc, exports) in global_modules.values() {
        mark::mark_builtin_module(opts, &mut scopes, &mut tbls, loc, &exports.borrow());
    }
    mark::mark_errors(&mut marker, &tbls.additional_errors);

    // compact
    // Read additional_errors after mark, since lazy forcing may add errors.
    let parse::Tables {
        locs,
        module_refs,
        local_defs,
        remote_refs,
        pattern_defs,
        patterns,
        additional_errors,
    } = tbls;
    let locs = locs.compact_without_merge();
    let module_refs = module_refs.compact_without_merge();
    let local_defs = local_defs.compact_without_merge();
    let remote_refs = remote_refs.compact_without_merge();
    let pattern_defs = pattern_defs.compact_without_merge();
    let patterns = patterns.compact_without_merge();

    // copy
    let mut cx = create_pack_cx(additional_errors);
    let (locs, _) = locs.copy(|loc| loc.dupe());
    let (module_refs, _) = module_refs.copy(|u| u.dupe());
    let (local_defs, _) = local_defs.copy(|binding| pack::pack_local_binding(&mut cx, binding));
    let (remote_refs, _) = remote_refs.copy(|binding| pack::pack_remote_binding(binding));
    let (pattern_defs, _) = pattern_defs.copy(|parsed| pack::pack_parsed(&mut cx, parsed));
    let (patterns, _) = patterns.copy(|pattern| pack::pack_pattern(pattern));

    let (global_values, global_types, global_modules) = {
        let global_values: BTreeMap<FlowSmolStr, Index<_>> = global_values
            .into_iter()
            .map(|(k, v)| (k, pack::pack_builtin(v)))
            .collect();
        let global_types: BTreeMap<FlowSmolStr, Index<_>> = global_types
            .into_iter()
            .map(|(k, v)| (k, pack::pack_builtin(v)))
            .collect();
        let global_modules: BTreeMap<FlowSmolStr, ModuleDef<Index<Loc>>> = global_modules
            .into_iter()
            .map(|(name, (loc, exports))| {
                let (loc, module_kind) =
                    pack::pack_builtin_module(&mut cx, &name, (loc, &exports.borrow()));
                (name, ModuleDef { loc, module_kind })
            })
            .collect();

        (global_values, global_types, global_modules)
    };

    let builtins = Builtins {
        module_refs,
        local_defs,
        remote_refs,
        pattern_defs,
        patterns,
        global_values,
        global_types,
        global_modules,
    };

    (cx.take_errs(), locs, builtins)
}

fn parse_module<'arena: 'ast, 'ast>(
    opts: &TypeSigOptions,
    arenas: &'arena bumpalo::Bump,
    strict: bool,
    platform_availability_set: Option<PlatformSet>,
    source: Option<FileKey>,
    ast: &'ast Program<Loc, Loc>,
) -> (
    parse::scope::Scopes<'arena, 'ast>,
    parse::Tables<'arena, 'ast>,
    parse::LocNode<'arena>,
    Rc<RefCell<parse::Exports<'arena, 'ast>>>,
) {
    let mut scopes = parse::scope::Scopes::new();
    let scope = parse::scope::create_module(&mut scopes, strict, platform_availability_set);
    let mut tbls = parse::Tables::new(arenas);
    let file_loc = tbls.push_loc(Loc { source, ..LOC_NONE });
    for stmt in ast.statements.iter() {
        parse::statement(opts, scope, &mut scopes, &mut tbls, stmt);
    }
    let exports = parse::scope::exports_exn(&mut scopes, scope);
    (scopes, tbls, file_loc, exports)
}

fn parse_libdef_file_as_empty_module<'arena: 'ast, 'ast>(
    arenas: &'arena bumpalo::Bump,
    strict: bool,
    platform_availability_set: Option<PlatformSet>,
    source: Option<FileKey>,
) -> (
    parse::scope::Scopes<'arena, 'ast>,
    parse::Tables<'arena, 'ast>,
    parse::LocNode<'arena>,
    Rc<RefCell<parse::Exports<'arena, 'ast>>>,
) {
    let mut scopes = parse::scope::Scopes::new();
    let scope = parse::scope::create_module(&mut scopes, strict, platform_availability_set);
    let mut tbls = parse::Tables::new(arenas);
    let file_loc = tbls.push_loc(Loc { source, ..LOC_NONE });
    let exports = parse::scope::exports_exn(&mut scopes, scope);
    (scopes, tbls, file_loc, exports)
}

fn merge_locs(loc0: &Loc, loc1: &Loc) -> Option<Loc> {
    match packed_locs::compare_locs(loc0, loc1) {
        std::cmp::Ordering::Less => None,
        std::cmp::Ordering::Equal => Some(loc0.dupe()),
        std::cmp::Ordering::Greater => {
            panic!("out of order {:?} > {:?}", loc0, loc1)
        }
    }
}

fn pack<'arena, 'ast>(
    opts: &TypeSigOptions,
    locs_to_dirtify: &[Loc],
    source: Option<FileKey>,
    (mut scopes, mut tbls, file_loc, exports): (
        parse::scope::Scopes<'arena, 'ast>,
        parse::Tables<'arena, 'ast>,
        parse::LocNode<'arena>,
        Rc<RefCell<parse::Exports<'arena, 'ast>>>,
    ),
) -> (Vec<Errno<Index<Loc>>>, Table<Loc>, Module<Loc>) {
    // mark
    let mut marker = mark::Marker::new(locs_to_dirtify);
    mark::mark_exports(
        opts,
        &mut scopes,
        &mut tbls,
        &mut marker,
        &file_loc,
        &exports.borrow(),
    );
    mark::mark_errors(&mut marker, &tbls.additional_errors);

    // Destructure tbls after marking is complete
    let parse::Tables {
        locs,
        module_refs,
        local_defs,
        remote_refs,
        pattern_defs,
        patterns,
        additional_errors,
    } = tbls;

    // compact
    let locs = locs.compact_with_merge(merge_locs);
    let module_refs = module_refs.compact_without_merge();
    let local_defs = local_defs.compact_without_merge();
    let remote_refs = remote_refs.compact_without_merge();
    let pattern_defs = pattern_defs.compact_without_merge();
    let patterns = patterns.compact_without_merge();

    // copy
    let mut cx = create_pack_cx(additional_errors);
    let (locs, _) = locs.copy(|loc| loc.dupe());
    let (module_refs, _) = module_refs.copy(|u| u.dupe());
    let (local_defs, dirty_local_defs) =
        local_defs.copy(|binding| pack::pack_local_binding(&mut cx, binding));
    let (remote_refs, _) = remote_refs.copy(|binding| pack::pack_remote_binding(binding));
    let (pattern_defs, dirty_pattern_defs) =
        pattern_defs.copy(|parsed| pack::pack_parsed(&mut cx, parsed));
    let (patterns, _) = patterns.copy(|pattern| pack::pack_pattern(pattern));

    let module_kind = {
        pack::pack_exports(
            &mut cx,
            &file_loc,
            &source
                .as_ref()
                .map(|s| FlowSmolStr::new(s.as_str()))
                .unwrap_or_else(|| FlowSmolStr::new_inline("<unnamed>")),
            &exports.borrow(),
        )
    };

    let module = Module {
        module_kind,
        module_refs,
        local_defs,
        dirty_local_defs,
        remote_refs,
        pattern_defs,
        dirty_pattern_defs,
        patterns,
    };

    (cx.take_errs(), locs, module)
}

pub fn parse_and_pack_builtins<'arena: 'ast, 'ast>(
    opts: &TypeSigOptions,
    arenas: &'arena bumpalo::Bump,
    ordered_asts: &[&'ast Program<Loc, Loc>],
) -> (Vec<Errno<Index<Loc>>>, Table<Loc>, Builtins<Loc>) {
    let (scopes, tbls, globals) = parse_libs(opts, arenas, ordered_asts);
    pack_builtins(opts, scopes, tbls, globals)
}

pub fn parse_and_pack_module<'arena: 'ast, 'ast>(
    opts: &TypeSigOptions,
    arenas: &'arena bumpalo::Bump,
    strict: bool,
    platform_availability_set: Option<PlatformSet>,
    source: Option<FileKey>,
    ast: &'ast Program<Loc, Loc>,
) -> (Vec<Errno<Index<Loc>>>, Table<Loc>, Module<Loc>) {
    let parsed = match &source {
        Some(file_key) if file_key.is_lib_file() => {
            // We will generate an empty file_sig for libdef files in parsing_service.
            // Therefore, we should also generate an empty type sig for libdef files, so that
            // both will consistently report that there are no imports and exports in libdef files.
            // If they are inconsistent, we can potentially crash like what's reported in
            // https://github.com/facebook/flow/issues/9262
            parse_libdef_file_as_empty_module(
                arenas,
                strict,
                platform_availability_set,
                source.dupe(),
            )
        }
        _ => parse_module(
            opts,
            arenas,
            strict,
            platform_availability_set,
            source.dupe(),
            ast,
        ),
    };
    pack(opts, &opts.locs_to_dirtify, source, parsed)
}
