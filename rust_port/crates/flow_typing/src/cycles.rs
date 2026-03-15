/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::VirtualReason;
use flow_env_builder::env_api::EnvKey;
use flow_env_builder::env_api::EnvMap;
use flow_env_builder::name_def_types::Def;
use flow_env_builder_resolver::name_def_ordering::Blame;
use flow_env_builder_resolver::name_def_ordering::Element;
use flow_env_builder_resolver::name_def_ordering::OrderingResult;
use flow_typing_context::Context;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_flow_common::flow_js_utils;
use vec1::Vec1;

fn handle_element(cx: &Context, elt: &Element) -> bool {
    match elt {
        Element::Normal(_) | Element::Resolvable(_) => false,
        Element::Illegal(Blame {
            reason,
            recursion,
            payload: _,
            annot_locs,
        }) => {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::ERecursiveDefinition {
                    reason: reason.dupe(),
                    recursion: recursion.clone(),
                    annot_locs: annot_locs.clone(),
                },
            );
            true
        }
    }
}
fn key_of_element(elt: &Element) -> &EnvKey<ALoc> {
    match elt {
        Element::Normal(key) => key,
        Element::Resolvable(key) => key,
        Element::Illegal(Blame { payload: key, .. }) => key,
    }
}

pub fn handle_component<A: Clone, B: Clone>(
    cx: &Context,
    graph: &EnvMap<ALoc, (Def, A, B, VirtualReason<ALoc>)>,
    scc: &OrderingResult,
) {
    match scc {
        OrderingResult::Singleton(elt) => {
            let _: bool = handle_element(cx, elt);
        }
        OrderingResult::ResolvableSCC(elts) => {
            for elt in elts.iter() {
                let _: bool = handle_element(cx, elt);
            }
        }
        OrderingResult::IllegalSCC(elts_blame) => {
            let blame: Vec<_> = elts_blame
                .iter()
                .filter_map(
                    |(
                        Blame {
                            payload: elt,
                            reason,
                            recursion: blame,
                            annot_locs,
                        },
                        display,
                    )| {
                        let illegal_elt = handle_element(cx, elt);
                        if *display {
                            let (def, _, _, _) = graph.get(key_of_element(elt)).unwrap();
                            Some((
                                (def, illegal_elt),
                                (reason.dupe(), blame.clone(), annot_locs.clone()),
                            ))
                        } else {
                            None
                        }
                    },
                )
                .collect();
            // If at least one element of the cycle is recursive, and every element is
            // either an expression or a recursive element, don't emit the cycle error
            // -- the recursion error will contain all the actionable advice
            let fold_result =
                blame
                    .iter()
                    .try_fold(false, |has_illegal, ((def, illegal_elt), _)| match def {
                        Def::ExpressionDef(_) => Ok(has_illegal || *illegal_elt),
                        _ if *illegal_elt => Ok(true),
                        _ => Err(()),
                    });
            match fold_result {
                Ok(true) => {}
                Ok(false) | Err(()) => {
                    let cycle_entries: Vec<_> = blame.into_iter().map(|(_, snd)| snd).collect();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EDefinitionCycle(Vec1::try_from_vec(cycle_entries).unwrap()),
                    );
                }
            }
        }
    }
}
