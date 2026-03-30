/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use vec1::Vec1;

use crate::reason::Reason;

#[derive(Debug, Clone, PartialEq)]
pub enum HintKind {
    /// This kind of hint expects that the `typeof e <: hint` when `hint` is an available hint on `e`
    ExpectedTypeHint,
    /// This kind of hint doesn't have the same restriction as `ExpectedTypeHint`.
    /// Instead, it is designed to provide some context when there wasn't an ExpectedTypeHint available.
    /// e.g. We use the type of `a` to contextually type `b` in `a ?? b`.
    BestEffortHint,
}

// In contextual typing we often need to perform decompositions on type hints before
// we descend deeper into an expression during type checking. For example if an
// object literal `{ f: (x) => x + 1 }` is checked with a hint `{ f: (number) => number }`
// then we can check the value of property `f` with the hint `(number) => number`
// and, further, use `number` as the type of `x`.

#[derive(Clone)]
pub struct FunCallImplicitInstantiationHints<'a, T, TArgs, Args, PropsAndChildren> {
    pub reason: Reason,
    pub return_hints: Rc<
        LazyCell<
            Vec<Hint<'a, T, TArgs, Args, PropsAndChildren>>,
            Box<dyn Fn() -> Vec<Hint<'a, T, TArgs, Args, PropsAndChildren>> + 'a>,
        >,
    >,
    pub targs: Rc<LazyCell<TArgs, Box<dyn Fn() -> TArgs + 'a>>>,
    pub arg_list: Rc<LazyCell<Args, Box<dyn Fn() -> Args + 'a>>>,
    pub arg_index: i32,
}

#[derive(Clone)]
pub struct JsxImplicitInstantiationHints<'a, T, TArgs, Args, PropsAndChildren> {
    pub jsx_reason: Reason,
    pub jsx_name: FlowSmolStr,
    pub jsx_targs: Rc<LazyCell<TArgs, Box<dyn Fn() -> TArgs + 'a>>>,
    pub jsx_props_and_children: PropsAndChildren,
    pub jsx_hints: Rc<
        LazyCell<
            Vec<Hint<'a, T, TArgs, Args, PropsAndChildren>>,
            Box<dyn Fn() -> Vec<Hint<'a, T, TArgs, Args, PropsAndChildren>> + 'a>,
        >,
    >,
}

#[derive(Debug, Clone, Dupe, PartialEq)]
pub enum SentinelRefinement {
    SingletonNum(f64),
    SingletonBool(bool),
    SingletonStr(FlowSmolStr),
    SingletonBigInt(i64),
    Member(Reason),
}

#[derive(Debug, Clone, Dupe, PartialEq)]
pub enum PredicateKind {
    TypeGuardKind(ALoc, FlowSmolStr),
}

#[derive(Clone)]
pub struct HintDecomposition<'a, T, TArgs, Args, PropsAndChildren>(
    Rc<HintDecompositionInner<'a, T, TArgs, Args, PropsAndChildren>>,
);

impl<'a, T, TArgs, Args, PropsAndChildren> Dupe
    for HintDecomposition<'a, T, TArgs, Args, PropsAndChildren>
where
    T: Clone,
    TArgs: Clone,
    Args: Clone,
    PropsAndChildren: Clone,
{
}

impl<'a, T, TArgs, Args, PropsAndChildren> HintDecomposition<'a, T, TArgs, Args, PropsAndChildren> {
    pub fn new(inner: HintDecompositionInner<'a, T, TArgs, Args, PropsAndChildren>) -> Self {
        HintDecomposition(Rc::new(inner))
    }

    pub fn inner(&self) -> &HintDecompositionInner<'a, T, TArgs, Args, PropsAndChildren> {
        &self.0
    }

    pub fn to_debug_string(&self) -> String {
        use HintDecompositionInner::*;
        match self.inner() {
            DecompObjProp(_) => "Decomp_ObjProp".to_string(),
            DecompObjComputed(_) => "Decomp_ObjComputed".to_string(),
            DecompObjSpread => "Decomp_ObjSpread".to_string(),
            DecompPrivateProp(..) => "Decomp_PrivateProp".to_string(),
            DecompArrElement(None) => "Decomp_ArrElement (no index)".to_string(),
            DecompArrElement(Some(i)) => format!("Decomp_ArrElement ({})", i),
            DecompArrSpread(i) => format!("Decomp_ArrSpread ({})", i),
            DecompEmptyArrayElement => "Decomp_EmptyArrayElement".to_string(),
            DecompMethodName(_) => "Decomp_MethodName".to_string(),
            DecompMethodPrivateName(..) => "Decomp_MethodPrivateName".to_string(),
            DecompMethodElem => "Decomp_MethodElem".to_string(),
            DecompCallNew => "Decomp_CallNew".to_string(),
            DecompCallSuper => "Decomp_CallSuper".to_string(),
            DecompFuncParam(xs, i, pred) => {
                let xs_str = xs
                    .iter()
                    .map(|x| x.as_deref().unwrap_or("_"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Decomp_FuncParam ([{}], {}, {})",
                    xs_str,
                    i,
                    string_of_predicate_kind(pred)
                )
            }
            DecompFuncRest(xs, pred) => {
                let xs_str = xs
                    .iter()
                    .map(|x| x.as_deref().unwrap_or("_"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Decomp_FuncRest ({}, {})",
                    xs_str,
                    string_of_predicate_kind(pred)
                )
            }
            DecompFuncReturn => "Decomp_FuncReturn".to_string(),
            CompImmediateFuncCall => "Comp_ImmediateFuncCall".to_string(),
            CompMaybeT => "Comp_MaybeT".to_string(),
            DecompJsxProps => "Decomp_JsxProps".to_string(),
            DecompSentinelRefinement(_) => "Decomp_SentinelRefinement".to_string(),
            DecompAwait => "Decomp_Await".to_string(),
            SimplifyCallee(_) => "Simplify_Callee".to_string(),
            InstantiateCallee(_) => "Instantiate_Callee".to_string(),
            InstantiateComponent(_) => "Instantiate_Component".to_string(),
            DecompPromise => "Decomp_Promise".to_string(),
        }
    }

    pub fn map<'b, CX: Dupe + 'b, T2, TArgs2, Args2, PropsAndChildren2>(
        &self,
        cx: &CX,
        map_base_hint: &Rc<impl for<'c> Fn(&'c CX, T) -> T2 + 'b>,
        map_targs: &Rc<impl for<'c> Fn(&'c CX, TArgs) -> TArgs2 + 'b>,
        map_arg_list: &Rc<impl for<'c> Fn(&'c CX, Args) -> Args2 + 'b>,
        map_jsx: &Rc<
            impl for<'c> Fn(&'c CX, Reason, FlowSmolStr, PropsAndChildren) -> PropsAndChildren2 + 'b,
        >,
    ) -> HintDecomposition<'b, T2, TArgs2, Args2, PropsAndChildren2>
    where
        'a: 'b,
        T: Clone + 'a,
        TArgs: Clone + 'a,
        Args: Clone + 'a,
        PropsAndChildren: Clone + 'a,
        T2: Clone + 'b,
        TArgs2: Clone + 'b,
        Args2: Clone + 'b,
        PropsAndChildren2: Clone + 'b,
    {
        use HintDecompositionInner::*;
        let inner = match self.inner() {
            DecompObjProp(prop) => DecompObjProp(prop.dupe()),
            DecompObjComputed(l) => DecompObjComputed(l.dupe()),
            DecompObjSpread => DecompObjSpread,
            DecompPrivateProp(prop, class_stack) => {
                DecompPrivateProp(prop.dupe(), class_stack.clone())
            }
            DecompArrElement(o) => DecompArrElement(*o),
            DecompArrSpread(o) => DecompArrSpread(*o),
            DecompEmptyArrayElement => DecompEmptyArrayElement,
            DecompAwait => DecompAwait,
            DecompMethodName(name) => DecompMethodName(name.dupe()),
            DecompMethodPrivateName(name, class_stack) => {
                DecompMethodPrivateName(name.dupe(), class_stack.clone())
            }
            DecompMethodElem => DecompMethodElem,
            DecompCallNew => DecompCallNew,
            DecompCallSuper => DecompCallSuper,
            DecompFuncParam(i, s, p) => DecompFuncParam(i.clone(), *s, p.dupe()),
            DecompFuncRest(i, p) => DecompFuncRest(i.clone(), p.dupe()),
            DecompFuncReturn => DecompFuncReturn,
            CompImmediateFuncCall => CompImmediateFuncCall,
            CompMaybeT => CompMaybeT,
            DecompJsxProps => DecompJsxProps,
            DecompSentinelRefinement(checks) => DecompSentinelRefinement(checks.clone()),
            SimplifyCallee(r) => SimplifyCallee(r.dupe()),
            InstantiateCallee(hints) => {
                let FunCallImplicitInstantiationHints {
                    reason,
                    return_hints,
                    targs,
                    arg_list,
                    arg_index,
                } = hints;

                // IMPORTANT: These must remain LAZY — only map when accessed.
                let return_hints_mapped = Rc::new(LazyCell::new(Box::new({
                    let return_hints = return_hints.dupe();
                    let cx = cx.dupe();
                    let map_base_hint = map_base_hint.dupe();
                    let map_targs = map_targs.dupe();
                    let map_arg_list = map_arg_list.dupe();
                    let map_jsx = map_jsx.dupe();
                    move || {
                        LazyCell::force(&return_hints)
                            .iter()
                            .map(|h| {
                                h.clone().map(
                                    &cx,
                                    &map_base_hint,
                                    &map_targs,
                                    &map_arg_list,
                                    &map_jsx,
                                )
                            })
                            .collect()
                    }
                })
                    as Box<dyn Fn() -> Vec<Hint<'b, T2, TArgs2, Args2, PropsAndChildren2>> + 'b>));
                let targs_mapped = Rc::new(LazyCell::new(Box::new({
                    let targs = targs.dupe();
                    let cx = cx.dupe();
                    let map_targs = map_targs.dupe();
                    move || (map_targs.as_ref())(&cx, LazyCell::force(&targs).clone())
                })
                    as Box<dyn Fn() -> TArgs2 + 'b>));
                let arg_list_mapped = Rc::new(LazyCell::new(Box::new({
                    let arg_list = arg_list.dupe();
                    let cx = cx.dupe();
                    let map_arg_list = map_arg_list.dupe();
                    move || (map_arg_list.as_ref())(&cx, LazyCell::force(&arg_list).clone())
                })
                    as Box<dyn Fn() -> Args2 + 'b>));

                InstantiateCallee(FunCallImplicitInstantiationHints {
                    reason: reason.dupe(),
                    return_hints: return_hints_mapped,
                    targs: targs_mapped,
                    arg_list: arg_list_mapped,
                    arg_index: *arg_index,
                })
            }
            InstantiateComponent(hints) => {
                let JsxImplicitInstantiationHints {
                    jsx_reason,
                    jsx_name,
                    jsx_targs,
                    jsx_props_and_children,
                    jsx_hints,
                } = hints;

                let jsx_targs_mapped = Rc::new(LazyCell::new(Box::new({
                    let jsx_targs = jsx_targs.dupe();
                    let cx = cx.dupe();
                    let map_targs = map_targs.dupe();
                    move || (map_targs.as_ref())(&cx, LazyCell::force(&jsx_targs).clone())
                })
                    as Box<dyn Fn() -> TArgs2 + 'b>));

                let jsx_props_and_children_mapped = (map_jsx.as_ref())(
                    cx,
                    jsx_reason.dupe(),
                    jsx_name.dupe(),
                    jsx_props_and_children.clone(),
                );

                let jsx_hints_mapped = Rc::new(LazyCell::new(Box::new({
                    let jsx_hints = jsx_hints.dupe();
                    let cx = cx.dupe();
                    let map_base_hint = map_base_hint.dupe();
                    let map_targs = map_targs.dupe();
                    let map_arg_list = map_arg_list.dupe();
                    let map_jsx = map_jsx.dupe();
                    move || {
                        LazyCell::force(&jsx_hints)
                            .iter()
                            .map(|h| {
                                h.clone().map(
                                    &cx,
                                    &map_base_hint,
                                    &map_targs,
                                    &map_arg_list,
                                    &map_jsx,
                                )
                            })
                            .collect()
                    }
                })
                    as Box<dyn Fn() -> Vec<Hint<'b, T2, TArgs2, Args2, PropsAndChildren2>> + 'b>));

                InstantiateComponent(JsxImplicitInstantiationHints {
                    jsx_reason: jsx_reason.dupe(),
                    jsx_name: jsx_name.dupe(),
                    jsx_targs: jsx_targs_mapped,
                    jsx_props_and_children: jsx_props_and_children_mapped,
                    jsx_hints: jsx_hints_mapped,
                })
            }
            DecompPromise => DecompPromise,
        };
        HintDecomposition::new(inner)
    }
}

pub enum HintDecompositionInner<'a, T, TArgs, Args, PropsAndChildren> {
    /// Hint on `{ f: e }` becomes hint on `e`
    DecompObjProp(FlowSmolStr),
    /// Hint on `{ [k]: e }` becomes hint on `e`
    DecompObjComputed(Reason),
    /// Hint on `{ ...e }` becomes hint on `e`
    DecompObjSpread,
    /// Type of `obj` becomes type of `obj.#privateName`
    DecompPrivateProp(FlowSmolStr, FlowVector<ALoc>),
    /// Hint on array literal `[e]` becomes hint on `e`
    DecompArrElement(Option<i32>),
    /// Hint on array literal `[...e]` becomes hint on `e`
    DecompArrSpread(i32),
    /// Hint on empty array literal `[]` becomes hint on its element type
    DecompEmptyArrayElement,
    /// Type of `await e` becomes hint on `e`
    DecompAwait,
    /// Type of `o` in `o.m(..)` becomes the type of `o.m`
    DecompMethodName(FlowSmolStr),
    /// Type of `o` in `o.#m(..)` becomes the type of `o.m`
    DecompMethodPrivateName(FlowSmolStr, FlowVector<ALoc>),
    /// Type of `o` in `o[e](..)` becomes the type of `o[e]`
    DecompMethodElem,
    /// Type of `C` in `new C(..)` becomes the type of the constructor of C
    DecompCallNew,
    /// Type of the super-class becomes the type of the super constructor
    DecompCallSuper,
    /// Type of function becomes hint on the i-th argument
    DecompFuncParam(Vec<Option<FlowSmolStr>>, i32, Option<PredicateKind>),
    /// Type of function becomes hint on rest argument
    DecompFuncRest(Vec<Option<FlowSmolStr>>, Option<PredicateKind>),
    /// Type of function becomes hint on return
    DecompFuncReturn,
    /// Hint on call `f()` becomes hint on `f`. This is only meant to be used for the
    /// case of immediate function call `(function() {})()`.
    CompImmediateFuncCall,
    /// Given t, returns MaybeT(t).
    /// This is helpful for constructing a hint for `a` in `a ?? b` when the hint on `a ?? b` is t.
    CompMaybeT,
    /// Type of C in `<C [props]/>` becomes hint on `props`
    DecompJsxProps,
    DecompSentinelRefinement(BTreeMap<FlowSmolStr, SentinelRefinement>),
    /// Type of a callee is concretized so that we can inspect its structure
    SimplifyCallee(Reason),
    /// Type of f in f(...) is instantiated with arguments and return hint.
    /// Returns f if the type of f is not polymorphic.
    InstantiateCallee(FunCallImplicitInstantiationHints<'a, T, TArgs, Args, PropsAndChildren>),
    /// Type of Comp in <Comp ... /> is instantiated with props and children.
    /// Returns Comp if the type of Comp is not polymorphic.
    InstantiateComponent(JsxImplicitInstantiationHints<'a, T, TArgs, Args, PropsAndChildren>),
    /// T of Promise<T> becomes hint on return in async scope
    DecompPromise,
}

#[derive(Clone)]
pub enum Hint<'a, T, TArgs, Args, PropsAndChildren> {
    HintT(T, HintKind),
    HintDecomp(
        Vec1<(
            usize,
            HintDecomposition<'a, T, TArgs, Args, PropsAndChildren>,
        )>,
        T,
        HintKind,
    ),
    /// The hint placeholder used in env_resolution to pass to expression type checkers.
    /// It will eventually be removed once we move all hint decomposition logic into env_resolution.
    HintPlaceholder,
}

impl<'a, T, TArgs, Args, PropsAndChildren> Hint<'a, T, TArgs, Args, PropsAndChildren> {
    pub fn decompose(
        decomp: HintDecomposition<'a, T, TArgs, Args, PropsAndChildren>,
        hints: Vec<Self>,
    ) -> Vec<Self>
    where
        T: Clone,
        TArgs: Clone,
        Args: Clone,
        PropsAndChildren: Clone,
    {
        hints
            .into_iter()
            .map(|hint| match hint {
                Hint::HintT(t, kind) => Hint::HintDecomp(
                    Vec1::try_from_vec(vec![(crate::reason::mk_id(), decomp.dupe())]).unwrap(),
                    t,
                    kind,
                ),
                Hint::HintDecomp(mut decomps, t, kind) => {
                    decomps.push((crate::reason::mk_id(), decomp.dupe()));
                    Hint::HintDecomp(decomps, t, kind)
                }
                Hint::HintPlaceholder => Hint::HintPlaceholder,
            })
            .collect()
    }

    pub fn to_debug_string(&self, on_hint: impl Fn(&T) -> String) -> String
    where
        T: Clone + PartialEq,
        TArgs: Clone + PartialEq,
        Args: Clone + PartialEq,
        PropsAndChildren: Clone + PartialEq,
    {
        match self {
            Hint::HintT(t, _) => format!("Hint_t ({})", on_hint(t)),
            Hint::HintDecomp(ops, t, _) => {
                let ops_str = ops
                    .iter()
                    .map(|(_, op)| op.to_debug_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Hint_Decomp ({})({})", ops_str, on_hint(t))
            }
            Hint::HintPlaceholder => "Hint_Placeholder".to_string(),
        }
    }

    pub fn map<'b, CX: Dupe + 'b, T2, TArgs2, Args2, PropsAndChildren2>(
        self,
        cx: &CX,
        map_base_hint: &Rc<impl for<'c> Fn(&'c CX, T) -> T2 + 'b>,
        map_targs: &Rc<impl for<'c> Fn(&'c CX, TArgs) -> TArgs2 + 'b>,
        map_arg_list: &Rc<impl for<'c> Fn(&'c CX, Args) -> Args2 + 'b>,
        map_jsx: &Rc<
            impl for<'c> Fn(&'c CX, Reason, FlowSmolStr, PropsAndChildren) -> PropsAndChildren2 + 'b,
        >,
    ) -> Hint<'b, T2, TArgs2, Args2, PropsAndChildren2>
    where
        'a: 'b,
        T: Clone + 'a,
        TArgs: Clone + 'a,
        Args: Clone + 'a,
        PropsAndChildren: Clone + 'a,
        T2: Clone + 'b,
        TArgs2: Clone + 'b,
        Args2: Clone + 'b,
        PropsAndChildren2: Clone + 'b,
    {
        match self {
            Hint::HintT(t, kind) => Hint::HintT((map_base_hint.as_ref())(cx, t), kind),
            Hint::HintDecomp(ops, t, kind) => {
                let mapped_ops = ops
                    .into_iter()
                    .map(|(i, op)| {
                        (
                            i,
                            op.map(cx, map_base_hint, map_targs, map_arg_list, map_jsx),
                        )
                    })
                    .collect::<Vec<_>>();
                Hint::HintDecomp(
                    Vec1::try_from_vec(mapped_ops).unwrap(),
                    (map_base_hint.as_ref())(cx, t),
                    kind,
                )
            }
            Hint::HintPlaceholder => Hint::HintPlaceholder,
        }
    }

    pub fn string_of_hints(on_hint: impl Fn(&T) -> String, hints: &[Self]) -> String
    where
        T: Clone + PartialEq,
        TArgs: Clone + PartialEq,
        Args: Clone + PartialEq,
        PropsAndChildren: Clone + PartialEq,
    {
        let hints_str = hints
            .iter()
            .map(|hint| hint.to_debug_string(&on_hint))
            .collect::<Vec<_>>()
            .join(",");
        format!("[{}]", hints_str)
    }
}

fn string_of_predicate_kind(pred: &Option<PredicateKind>) -> &'static str {
    match pred {
        None => "no pred",
        Some(PredicateKind::TypeGuardKind(..)) => "type guard kind",
    }
}
