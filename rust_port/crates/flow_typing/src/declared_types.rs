/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::aloc_representation_do_not_use;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc::RProperty;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_env_builder::assertion_call_target;
use flow_env_builder::assertion_call_target::AssertionInfo;
use flow_env_builder::assertion_call_target::AssertionKind;
use flow_env_builder::assertion_call_target::CalleeTarget;
use flow_env_builder::assertion_call_target::ImportedName;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_type_sig::compact_table::Index;
use flow_type_sig::compact_table::Table;
use flow_type_sig::packed_type_sig::Module;
use flow_type_sig::packed_type_sig::TargetedModule;
use flow_type_sig::packed_type_sig::TargetedRoot;
use flow_type_sig::type_sig_options::TypeSigOptions;
use flow_type_sig::type_sig_utils;
use flow_typing_context::Context;
use flow_typing_context::ResolvedRequire;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_utils::annotation_inference;
use flow_typing_utils::type_sig_merge;

pub(crate) struct PackedAnalysis {
    canonical_candidates: Vec<CalleeTarget<ALoc>>,
    all_candidates: Vec<CalleeTarget<ALoc>>,
    local: Option<PackedLocalAnalysis>,
}

struct PackedLocalAnalysis {
    existing: Option<ExistingLocalAnalysis>,
    parsed: Option<ParsedLocalAnalysis>,
}

struct ExistingLocalAnalysis {
    module: Arc<Module<Loc>>,
    roots: BTreeMap<ALoc, TargetedRoot<Loc>>,
}

struct ParsedLocalAnalysis {
    locs: Table<Loc>,
    targeted: TargetedModule<Loc>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct CanonicalCallee {
    root_binding_loc: ALoc,
    property_path: Vec<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
}

impl CanonicalCallee {
    fn of_target(target: &CalleeTarget<ALoc>) -> Self {
        Self {
            root_binding_loc: target.root_binding_loc.dupe(),
            property_path: target.property_path.clone(),
        }
    }
}

pub(crate) fn pack(
    opts: &TypeSigOptions,
    cx: &Context<'_>,
    ast: &ast::Program<Loc, Loc>,
    aloc_ast: &ast::Program<ALoc, ALoc>,
    scope_info: &flow_analysis::scope_api::ScopeInfo<ALoc>,
    provider_info: &flow_env_builder::provider_api::Info<ALoc>,
    current_type_sig: Option<Arc<Module<Loc>>>,
) -> Option<PackedAnalysis> {
    let candidates = assertion_call_target::collect(aloc_ast, scope_info, provider_info);
    if candidates.is_empty() {
        return None;
    }
    let canonical_candidates: Vec<CalleeTarget<ALoc>> = candidates
        .iter()
        .map(|candidate| (CanonicalCallee::of_target(candidate), candidate.clone()))
        .collect::<BTreeMap<_, _>>()
        .into_values()
        .collect();
    // Merge all local roots; imports resolve separately.
    let roots = canonical_candidates
        .iter()
        .filter(|candidate| candidate.import.is_none() || !candidate.provider_locs.is_empty())
        .map(|candidate| candidate.root_binding_loc.dupe())
        .collect::<BTreeSet<_>>();
    let source = Some(cx.file().dupe());
    let aloc_tables = cx.aloc_tables();
    let reused_roots = current_type_sig
        .as_ref()
        .map(|module| {
            module
                .local_defs
                .iter()
                .enumerate()
                .filter_map(|(index, def)| {
                    let keyed_root_loc = aloc_representation_do_not_use::make_keyed(
                        source.dupe(),
                        def.id_loc().as_usize() as u32,
                    );
                    let root_loc = ALoc::of_loc(keyed_root_loc.to_loc_with_tables(&aloc_tables));
                    roots
                        .contains(&root_loc)
                        .then_some((root_loc, TargetedRoot::LocalDef(Index::new(index))))
                })
                .collect::<BTreeMap<_, _>>()
        })
        .unwrap_or_default();
    let fallback_roots = roots
        .iter()
        .filter(|root| !reused_roots.contains_key(*root))
        .map(|root| root.to_loc_exn().dupe())
        .collect::<BTreeSet<_>>();
    let existing = current_type_sig.and_then(|module| {
        (!reused_roots.is_empty()).then_some(ExistingLocalAnalysis {
            module,
            roots: reused_roots,
        })
    });
    let parsed = if fallback_roots.is_empty() {
        None
    } else {
        let arena = bumpalo::Bump::new();
        let (_, locs, targeted) = type_sig_utils::parse_and_pack_targets(
            opts,
            &arena,
            cx.is_strict(),
            cx.metadata().overridable.available_platforms.clone(),
            Some(cx.file().dupe()),
            ast,
            &fallback_roots,
        );
        Some(ParsedLocalAnalysis { locs, targeted })
    };
    let local = (existing.is_some() || parsed.is_some())
        .then_some(PackedLocalAnalysis { existing, parsed });
    Some(PackedAnalysis {
        canonical_candidates,
        all_candidates: candidates,
        local,
    })
}

pub(crate) struct ResolvedAnalysis {
    #[expect(
        dead_code,
        reason = "consumed by the assertion integration in a descendant commit"
    )]
    assertion_calls: BTreeMap<ALoc, AssertionInfo>,
}

pub(crate) fn resolve<'cx>(cx: &Context<'cx>, analysis: PackedAnalysis) -> ResolvedAnalysis {
    let Ok(result) = cx.with_suppressed_errors(|| -> Result<_, !> {
        let merged = analysis
            .local
            .map(|local| MergedLocalAnalysis::new(cx, local));
        // Classify once per canonical callee, then fan out to all call sites.
        // Locals need a covering annotation; imports use dependency types.
        let mut classified = BTreeMap::new();
        for candidate in &analysis.canonical_candidates {
            let canonical = CanonicalCallee::of_target(candidate);
            // The reason must carry a loc: concretization can emit inspection
            // errors, and sourceless errors panic in `add_output` even under
            // suppression.
            let reason = reason::mk_reason(
                reason::VirtualReasonDesc::RIdentifier(candidate.root_name.dupe()),
                candidate.callee_loc.dupe(),
            );
            if candidate.import.is_some() {
                if let Some(info) = imported_assertion_info(cx, &reason, candidate) {
                    classified.insert(canonical, info);
                }
                continue;
            }
            // Deferred heads evaluate before navigation; mapped heads consume one path element.
            let root_type = merged
                .as_ref()
                .and_then(|merged| merged.root(&candidate.root_binding_loc));
            let assertion = root_type
                .and_then(|root_type| {
                    callee_type_for_classification(
                        cx,
                        &candidate.callee_loc,
                        &candidate.property_path,
                        root_type,
                    )
                })
                .as_ref()
                .and_then(|callee_type| assertion_info_of_type(cx, &reason, callee_type));
            if let Some(info) = assertion {
                classified.insert(canonical, info);
            }
        }
        let mut assertion_calls = BTreeMap::new();
        for candidate in &analysis.all_candidates {
            let canonical = CanonicalCallee::of_target(candidate);
            if let Some(info) = classified.get(&canonical) {
                assertion_calls.insert(candidate.callee_loc.dupe(), *info);
            }
        }
        Ok(ResolvedAnalysis { assertion_calls })
    });
    result
}

fn assertion_of_function(function: &flow_typing_type::type_::FunType) -> Option<AssertionInfo> {
    let guard = function.type_guard.as_deref()?;
    if !guard.is_asserts() {
        return None;
    }
    let parameter_index = function
        .params
        .iter()
        .position(|param| param.0.as_ref() == Some(&guard.param_name.1))?;
    Some(AssertionInfo {
        parameter_index,
        kind: if guard.type_guard.is_some() {
            AssertionKind::TypeGuard
        } else {
            AssertionKind::Bare
        },
    })
}

/// Resolve tvars/annotations and evaluate deferred heads.
/// Runs under error suppression; the reason only labels inspection diagnostics.
fn normalized_assertion_head(cx: &Context<'_>, reason: &Reason, type_: &Type) -> Option<Type> {
    let type_ = cx.find_resolved(type_)?;
    let type_ = annotation_inference::concretize_type(cx, reason.dupe(), type_);
    cx.find_resolved(&type_)
}

fn imported_assertion_info(
    cx: &Context<'_>,
    reason: &Reason,
    candidate: &CalleeTarget<ALoc>,
) -> Option<AssertionInfo> {
    let import = candidate.import.as_ref()?;
    if !import.is_value {
        return None;
    }
    let ResolvedRequire::TypedModule(module) =
        cx.find_require(&FlowImportSpecifier::userland(import.source.dupe()))
    else {
        return None;
    };
    let Ok(module) = module(cx, cx) else {
        return None;
    };
    let (export_name, property_path) = match &import.remote {
        ImportedName::Default => ("default", candidate.property_path.as_slice()),
        ImportedName::Named(name) => (name.as_str(), candidate.property_path.as_slice()),
        ImportedName::Namespace => {
            let (name, rest) = candidate.property_path.split_first()?;
            (name.as_str(), rest)
        }
    };
    let exports = cx.find_exports(module.module_export_types.value_exports_tmap.dupe());
    let export = exports.get(&Name::new(export_name))?;
    let callee_type =
        callee_type_for_classification(cx, &candidate.callee_loc, property_path, &export.type_)?;
    assertion_info_of_type(cx, reason, &callee_type)
}

/// Classify a fully-navigated callee type, shared by locals and imports.
/// Navigation (mapped consumption + `get_prop`) runs before, in
/// `callee_type_for_classification`.
fn assertion_info_of_type(
    cx: &Context<'_>,
    reason: &Reason,
    type_: &Type,
) -> Option<AssertionInfo> {
    let type_ = normalized_assertion_head(cx, reason, type_)?;
    match type_.deref() {
        TypeInner::DefT(_, def_t) => match def_t.deref() {
            DefTInner::FunT(_, function) => assertion_of_function(function),
            DefTInner::PolyT(poly) => assertion_info_of_type(cx, reason, &poly.t_out),
            _ => None,
        },
        // Unions never classify, even when all members agree: TS does not
        // narrow calls on union callees. Intersections never classify either.
        // TODO: ban assertion functions in intersections at their definition.
        _ => None,
    }
}

/// Navigate `property_path` from `root_type`: mapped heads consume one path
/// element each (shared value type); the rest via `get_prop` lookup.
/// Shared by locals (roots from merged signatures) and imports (roots from
/// dependency exports).
fn callee_type_for_classification(
    cx: &Context<'_>,
    callee_loc: &ALoc,
    property_path: &[FlowSmolStr],
    root_type: &Type,
) -> Option<Type> {
    let mut current = root_type.dupe();
    let mut path = property_path;
    loop {
        let resolved = cx.find_resolved(&current)?;
        if let TypeInner::EvalT { defer_use_t, .. } = resolved.deref() {
            if let flow_typing_type::type_::Destructor::MappedType(data) = defer_use_t.2.as_ref() {
                let (_, rest) = path.split_first()?;
                current = data.property_type.dupe();
                path = rest;
                continue;
            }
        }
        break;
    }
    // Deferred heads evaluate in the classifier.
    Some(property_type(cx, callee_loc, path, current))
}

fn property_type<'cx>(
    cx: &Context<'cx>,
    callee_loc: &ALoc,
    property_path: &[flow_data_structure_wrapper::smol_str::FlowSmolStr],
    root_type: Type,
) -> Type {
    property_path.iter().fold(root_type, |type_, property| {
        let name = Name::new(property.dupe());
        let reason = reason::mk_reason(RProperty(Some(name.dupe())), callee_loc.dupe());
        annotation_inference::get_prop(
            cx,
            flow_typing_type::type_::unknown_use(),
            reason,
            None,
            name,
            type_,
        )
    })
}

struct MergedLocalAnalysis<'cx> {
    existing: Option<type_sig_merge::TargetedMergeResult<'cx, ALoc>>,
    parsed: Option<type_sig_merge::TargetedMergeResult<'cx, Loc>>,
}

impl<'cx> MergedLocalAnalysis<'cx> {
    fn new(cx: &Context<'cx>, local: PackedLocalAnalysis) -> Self {
        let existing = local.existing.map(|existing| {
            type_sig_merge::merge_packed_target_roots(
                cx,
                Some(cx.file().dupe()),
                &existing.module,
                existing.roots,
            )
        });
        let parsed = local
            .parsed
            .map(|parsed| type_sig_merge::merge_targeted_module(cx, parsed.locs, parsed.targeted));
        Self { existing, parsed }
    }

    fn root(&self, loc: &ALoc) -> Option<&Type> {
        self.existing
            .as_ref()
            .and_then(|merged| merged.roots().get(loc))
            .or_else(|| {
                self.parsed
                    .as_ref()
                    .and_then(|merged| merged.roots().get(loc.to_loc_exn()))
            })
    }
}
