/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::locationless_reason;
use flow_common::reason::mk_reason;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_typing_context::Context;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::ArrayATData;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::open_tvar;
use flow_typing_visitors::type_visitor::TypeVisitor;

// (***********************)
// (* instantiation utils *)
// (***********************)

pub mod implicit_type_argument {
    use super::*;

    /// Make a type argument for a given type parameter, given a reason. Note that
    /// not all type arguments are tvars; the following function is used only when
    /// polymorphic types need to be implicitly instantiated, because there was no
    /// explicit instantiation (via a type application), or when we want to cache a
    /// unique instantiation and unify it with other explicit instantiations.
    pub fn mk_targ<'cx>(
        cx: &Context<'cx>,
        typeparam: &TypeParam,
        reason_op: &Reason,
        reason_tapp: &Reason,
    ) -> Type {
        // Create a reason that is positioned at reason_op, but has a def_loc at typeparam.reason.
        let loc_op = reason_op.loc().dupe();
        let desc = VirtualReasonDesc::RTypeParam(Box::new((
            typeparam.name.dupe(),
            (Arc::new(reason_op.desc(true).clone()), loc_op.dupe()),
            (
                Arc::new(reason_tapp.desc(true).clone()),
                reason_tapp.def_loc().dupe(),
            ),
        )));
        let reason = mk_reason(desc, typeparam.reason.def_loc().dupe());
        let reason = reason.reposition(loc_op);
        flow_typing_tvar::mk(cx, reason)
    }

    /// Abstract a type argument that is created by implicit instantiation
    /// above. Sometimes, these type arguments are involved in type expansion
    /// loops, so we abstract them to detect such loops.
    pub fn abstract_targ(tvar: &Type) -> Option<Type> {
        let tvar_inner = open_tvar(tvar);
        let reason = tvar_inner.reason();
        let desc = reason.desc(true).clone();
        match &desc {
            VirtualReasonDesc::RTypeParam(box (_, _, _)) => {
                let reason = locationless_reason(desc);
                Some(Type::new(TypeInner::OpenT(Tvar::new(reason, 0))))
            }
            _ => None,
        }
    }
}

// We maintain a stack of entries representing type applications processed
// during calls to flow, for the purpose of terminating unbounded expansion of
// type applications. Intuitively, we may have a potential infinite loop when
// processing a type application leads to another type application with the same
// root, but expanding type arguments. The entries in a stack contain
// approximate measurements that allow us to detect such expansion.
//
// An entry representing a type application with root C and type args T1,...,Tn
// is of the form (C, [A1,...,An]), where each Ai is a list of the roots of type
// applications nested in Ti. We consider a stack to indicate a potential
// infinite loop when the top of the stack is (C, [A1,...,An]) and there is
// another entry (C, [B1,...,Bn]) in the stack, such that each Bi is non-empty
// and is contained in Ai.

pub mod type_app_expansion {
    use flow_typing_context::type_app_expansion::Bound;
    use flow_typing_context::type_app_expansion::Entry;
    use flow_typing_context::type_app_expansion::Root;
    use flow_typing_context::type_app_expansion::RootSet;

    use super::*;

    // visitor to collect roots of type applications nested in a type
    struct RootsCollector;

    impl RootsCollector {
        fn arrtype(&self, r: &Reason, arr: &ArrType) -> Root {
            match arr {
                ArrType::ArrayAT(box ArrayATData { .. }) => Root::Array(r.dupe()),
                ArrType::ROArrayAT(box (..)) => Root::ROArray(r.dupe()),
                ArrType::TupleAT(box TupleATData { elements, .. }) => {
                    Root::Tuple(r.dupe(), elements.len())
                }
            }
        }
    }

    impl TypeVisitor<RootSet> for RootsCollector {
        fn type_<'cx>(
            &mut self,
            cx: &Context<'cx>,
            pole: Polarity,
            mut acc: RootSet,
            t: &Type,
        ) -> RootSet {
            match t.deref() {
                TypeInner::TypeAppT(box TypeAppTData {
                    type_: inner_type, ..
                }) => {
                    acc.insert(Root::Type(inner_type.dupe()));
                    flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t)
                }
                TypeInner::DefT(r, def_t) => match def_t.deref() {
                    DefTInner::ArrT(arr) => {
                        acc.insert(self.arrtype(r, arr));
                        flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t)
                    }
                    _ => flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t),
                },
                TypeInner::OpenT(_) => match implicit_type_argument::abstract_targ(t) {
                    None => acc,
                    Some(abstract_t) => {
                        acc.insert(Root::Type(abstract_t));
                        acc
                    }
                },
                _ => flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t),
            }
        }
    }

    fn collect_roots<'cx>(cx: &Context<'cx>, t: &Type) -> RootSet {
        let mut collector = RootsCollector;
        collector.type_(cx, Polarity::Neutral, RootSet::new(), t)
    }

    // Say that targs are possibly expanding when, given previous targs and
    // current targs, each previously non-empty targ is contained in the
    // corresponding current targ.
    fn possibly_expanding_targs(prev_tss: &[RootSet], tss: &[RootSet]) -> bool {
        let mut seen_nonempty_prev_ts = false;
        let mut prev_iter = prev_tss.iter();
        let mut curr_iter = tss.iter();

        loop {
            match (prev_iter.next(), curr_iter.next()) {
                (Some(prev_ts), Some(ts)) => {
                    // if prev_ts is not a subset of ts, we have found a counterexample
                    // and we can bail out
                    if !prev_ts.is_subset(ts) {
                        return false;
                    }
                    // otherwise, we recurse on the remaining targs, updating the bit
                    seen_nonempty_prev_ts = seen_nonempty_prev_ts || !prev_ts.is_empty();
                }
                // we have found no counterexamples, so it comes down to whether we've
                // seen any non-empty prev_ts
                (None, None) => return seen_nonempty_prev_ts,
                // something's wrong around arities, but that's not our problem, so bail out
                _ => return false,
            }
        }
    }

    // Detect whether pushing would cause a loop. Push only if no loop is
    // detected, and return whether push happened.
    pub fn push_unless_loop<'cx>(cx: &Context<'cx>, side: Bound, c: &Type, ts: &[Type]) -> bool {
        let tss: Vec<RootSet> = ts.iter().map(|t| collect_roots(cx, t)).collect();
        let limit = cx.type_expansion_recursion_limit();
        let mut is_loop = false;
        {
            let stack = cx.instantiation_stack();
            let mut count = 0;

            for Entry(prev_c, prev_tss, prev_side) in stack.iter() {
                if c == prev_c && possibly_expanding_targs(prev_tss, &tss) && side == *prev_side {
                    count += 1;
                    if count >= limit {
                        is_loop = true;
                        break;
                    }
                }
            }
        }

        if is_loop {
            if cx.is_verbose() {
                eprintln!("encountered the same TypeAppT again for {} times", limit);
            }
            false
        } else {
            cx.instantiation_stack_mut()
                .push(Entry(c.dupe(), tss, side));
            if cx.is_verbose() {
                eprintln!("typeapp stack entry pushed");
            }
            true
        }
    }

    pub fn pop<'cx>(cx: &Context<'cx>) {
        let mut stack = cx.instantiation_stack_mut();
        if !stack.is_empty() {
            stack.pop();
        }
    }

    pub fn get<'cx>(cx: &Context<'cx>) -> FlowVector<Entry> {
        cx.instantiation_stack().dupe()
    }

    pub fn set<'cx>(cx: &Context<'cx>, new_stack: FlowVector<Entry>) {
        *cx.instantiation_stack_mut() = new_stack;
    }
}
