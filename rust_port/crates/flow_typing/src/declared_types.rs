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
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::VirtualReasonDesc::RProperty;
use flow_env_builder::assertion_call_target;
use flow_env_builder::assertion_call_target::CalleeTarget;
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
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_utils::annotation_inference;
use flow_typing_utils::type_sig_merge;

pub(crate) struct PackedAnalysis {
    canonical_candidates: Vec<CalleeTarget<ALoc>>,
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
    let roots = canonical_candidates
        .iter()
        .filter(|candidate| !candidate.provider_locs.is_empty())
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
        local,
    })
}

pub(crate) struct ResolvedAnalysis {
    #[expect(
        dead_code,
        reason = "consumed by the assertion integration in a descendant commit"
    )]
    assertion_candidates: BTreeMap<CanonicalCallee, bool>,
}

pub(crate) fn resolve<'cx>(cx: &Context<'cx>, analysis: PackedAnalysis) -> ResolvedAnalysis {
    let Ok(result) = cx.with_suppressed_errors(|| -> Result<_, !> {
        let merged = analysis
            .local
            .map(|local| MergedLocalAnalysis::new(cx, local));
        let mut assertion_candidates = BTreeMap::new();
        for candidate in analysis.canonical_candidates {
            // Import roots resolve through the import machinery when
            // classification lands; only local roots are covered here.
            let root_type = if candidate.provider_locs.is_empty() {
                None
            } else {
                merged
                    .as_ref()
                    .and_then(|merged| merged.root(&candidate.root_binding_loc))
            };
            let Some(root_type) = root_type else {
                continue;
            };
            let callee_type = property_type(
                cx,
                &candidate.callee_loc,
                &candidate.property_path,
                root_type.dupe(),
            );
            assertion_candidates.insert(
                CanonicalCallee::of_target(&candidate),
                has_type_guard(cx, &callee_type),
            );
        }
        Ok(ResolvedAnalysis {
            assertion_candidates,
        })
    });
    result
}

fn has_type_guard(cx: &Context<'_>, type_: &Type) -> bool {
    let Some(type_) = cx.find_resolved(type_) else {
        return false;
    };
    match type_.deref() {
        TypeInner::DefT(_, def) => match def.deref() {
            DefTInner::FunT(_, fun) => fun.type_guard.is_some(),
            DefTInner::PolyT(box PolyTData { t_out, .. }) => has_type_guard(cx, t_out),
            _ => false,
        },
        _ => false,
    }
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
