/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeDestructorT;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualRootUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::eval as type_eval;
use flow_typing_type::type_::string_of_ctor;
use flow_typing_type::type_::string_of_use_ctor;
use flow_typing_type::type_util::mod_use_op_of_use_t;
use flow_typing_type::type_util::reason_of_t;
use flow_typing_type::type_util::reason_of_use_t;

// Cache that remembers pairs of types that are passed to __flow.
pub mod flow_constraint {
    use std::ops::Deref;

    use dupe::Dupe;

    use super::*;

    fn toplevel_use_op(use_op: &UseOp) -> UseOp {
        match &use_op {
            VirtualUseOp::Frame(_, inner_use_op) => toplevel_use_op(inner_use_op),
            VirtualUseOp::Op(root) => {
                if let VirtualRootUseOp::Speculation(inner) = &**root {
                    toplevel_use_op(inner)
                } else {
                    use_op.dupe()
                }
            }
        }
    }

    // attempt to read LB/UB pair from cache, add if absent
    pub fn get<'cx>(cx: &Context<'cx>, l: &Type, u: &UseT<Context<'cx>>) -> bool {
        match (l.deref(), u.deref()) {
            // Don't cache constraints involving type variables, since the
            // corresponding typing rules are already sufficiently robust.
            (TypeInner::OpenT(_), _) => false,
            (_, UseTInner::UseT(_, inner_t)) if matches!(inner_t.deref(), TypeInner::OpenT(_)) => {
                false
            }
            (_, UseTInner::ReposUseT(..)) => false,
            // Don't cache concretization constraints, since the concretization operation should already
            // have defense against bad cyclic types.
            (_, UseTInner::ConcretizeT(..)) => false,
            (
                _,
                UseTInner::BindT(..)
                | UseTInner::CallT(..)
                | UseTInner::MethodT(..)
                | UseTInner::PrivateMethodT(..)
                | UseTInner::ConstructorT(..)
                | UseTInner::LookupT(..)
                | UseTInner::GetPropT(..)
                | UseTInner::ObjRestT { .. }
                | UseTInner::ObjTestT { .. }
                | UseTInner::ArrRestT(..)
                | UseTInner::ValueToTypeReferenceT(..)
                | UseTInner::SpecializeT(..)
                | UseTInner::EvalTypeDestructorT(..)
                | UseTInner::ConcretizeTypeAppsT { .. }
                | UseTInner::ExtendsUseT(..)
                | UseTInner::ResolveUnionT(..)
                | UseTInner::ConditionalT(..)
                | UseTInner::DeepReadOnlyT { .. }
                | UseTInner::ElemT(..)
                | UseTInner::ReposLowerT { .. }
                | UseTInner::ObjKitT { .. }
                | UseTInner::HasOwnPropT(..),
            ) => false,
            _ => {
                // Use ops are purely for better error messages: they should have no
                // effect on type checking. However, recursively nested use ops can pose
                // non-termination problems. To ensure proper caching, we hash use ops
                // to just their toplevel structure.
                let u = mod_use_op_of_use_t(toplevel_use_op, u);
                let mut cache = cx.constraint_cache_mut();
                let found = !cache.add(l.dupe(), u.dupe());
                if !found {
                    // New entry
                } else if cx.is_verbose() {
                    eprintln!(
                        "{}FlowConstraint cache hit on ({}, {})",
                        cx.pid_prefix(),
                        string_of_ctor(l),
                        string_of_use_ctor(&u)
                    );
                }
                found
            }
        }
    }
}

pub mod eval {
    use dupe::Dupe;

    use super::*;

    pub fn id(cx: &Context, t: Type, defer_use: TypeDestructorT) -> Type {
        let mut eval_id_cache = cx.eval_id_cache_mut();
        let mut id_cache = cx.id_cache_mut();

        if let TypeInner::EvalT {
            defer_use_t: d,
            id: i,
            ..
        } = &*t
        {
            if d == &defer_use {
                if let Some(cached_t) = eval_id_cache.get(i) {
                    return cached_t.dupe();
                }
                let i = type_eval::Id::generate_id();
                eval_id_cache.insert(i.dupe(), t.dupe());
                return Type::new(TypeInner::EvalT {
                    type_: t,
                    defer_use_t: defer_use,
                    id: i,
                });
            }
        }

        let cache_key = (t.dupe(), defer_use.dupe());
        let id = match id_cache.get(&cache_key).cloned() {
            Some(i) => i,
            None => {
                let i = type_eval::Id::generate_id();
                id_cache.insert(cache_key, i.dupe());
                i
            }
        };
        Type::new(TypeInner::EvalT {
            type_: t,
            defer_use_t: defer_use,
            id,
        })
    }

    pub fn find_repos(
        cx: &Context,
        t: &Type,
        defer_use: &TypeDestructorT,
        id: &type_eval::Id,
    ) -> Option<Type> {
        let repos_cache = cx.eval_repos_cache();
        let cache_key = (t.dupe(), defer_use.dupe(), id.dupe());
        repos_cache.get(&cache_key).map(|t| t.dupe())
    }

    pub fn add_repos(
        cx: &Context,
        t: Type,
        defer_use: TypeDestructorT,
        id: type_eval::Id,
        tvar: Type,
    ) {
        let mut repos_cache = cx.eval_repos_cache_mut();
        let cache_key = (t, defer_use, id);
        repos_cache.insert(cache_key, tvar);
    }
}

pub mod fix {
    use dupe::Dupe;

    use super::*;

    pub fn find(cx: &Context, is_this: bool, i: &Type) -> Option<Type> {
        let cache = cx.fix_cache();
        let cache_key = (is_this, i.dupe());
        cache.get(&cache_key).map(|t| t.dupe())
    }

    pub fn add(cx: &Context, is_this: bool, i: Type, tvar: Type) {
        let mut cache = cx.fix_cache_mut();
        let cache_key = (is_this, i);
        cache.insert(cache_key, tvar);
    }
}

// debug util: please don't dead-code-eliminate
// Summarize flow constraints in cache as ctor/reason pairs, and return counts
//    for each group.
pub fn summarize_flow_constraint(cx: &Context) -> Vec<(String, i32)> {
    let cache = cx.constraint_cache();
    let mut group_counts: BTreeMap<String, i32> = BTreeMap::new();

    cache.fold(
        |(), (l, u)| {
            let key = format!(
                "[{}] {:?} => [{}] {:?}",
                string_of_ctor(l),
                reason_of_t(l),
                string_of_use_ctor(u),
                reason_of_use_t(u)
            );
            *group_counts.entry(key).or_insert(0) += 1;
        },
        (),
    );

    let mut result: Vec<(String, i32)> = group_counts.into_iter().collect();
    result.sort_by(|(_, i1), (_, i2)| i1.cmp(i2));
    result
}
