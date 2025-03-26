(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
module Ast = Flow_ast

type syntactic_flags = {
  encl_ctx: Type.enclosing_context;
  decl: Ast.Variable.kind option;
  as_const: bool;
  frozen: Type.frozen_kind;
  has_hint: bool Lazy.t;
}

let empty_syntactic_flags =
  {
    encl_ctx = Type.NoContext;
    decl = None;
    as_const = false;
    frozen = Type.NotFrozen;
    has_hint = lazy false;
  }

let mk_syntactic_flags
    ?(encl_ctx = Type.NoContext)
    ?decl
    ?(as_const = false)
    ?(frozen = Type.NotFrozen)
    ?(has_hint = lazy false)
    () =
  { encl_ctx; decl; as_const; frozen; has_hint }

(**
 * [generalize_singletons cx ~force_general t] walks a `t` and replacing instances
 * of singleton types that originate from literals with the general version of the
 * type.
 *
 * A key assumption is that `t` is the type inferred for a literal expression
 * (object, array, primitive literal) or the result of a conditional or logical
 * expression. This allows us to keep the cost of this visit low, by only
 * descending into types that are considered literals (for now we use reasons to
 * determine this.)
 *)
let generalize_singletons =
  let open Reason in
  let mapper =
    object (self)
      inherit [bool] Type_mapper.t as super

      method exports _cx _map_cx id = id

      method props cx force_general id =
        let props_map = Context.find_props cx id in
        let props_map' =
          NameUtils.Map.ident_map (Property.ident_map_t (self#type_ cx force_general)) props_map
        in
        let id' =
          if props_map == props_map' then
            id
          else
            Context.generate_property_map cx props_map'
        in
        id'

      method eval_id _cx _map_cx id = id

      method call_prop _cx _map_cx id = id

      method tvar cx force_general _r id =
        match Context.find_constraints cx id with
        | (_, Type.Constraint.FullyResolved s) ->
          let t = Context.force_fully_resolved_tvar cx s in
          let t' = self#type_ cx force_general t in
          if t == t' then
            id
          else
            Tvar.mk_fully_resolved_no_wrap cx t'
        | _ -> id

      method! type_ cx force_general t =
        match t with
        | OpenT _ -> super#type_ cx force_general t
        | DefT (r, ArrT _) when is_literal_array_reason r -> super#type_ cx force_general t
        | DefT (r, ObjT _) when is_literal_object_reason r -> super#type_ cx force_general t
        | DefT (r, FunT _) when is_literal_function_reason r -> super#type_ cx force_general t
        | UnionT (r, _) when is_literal_union_reason r -> super#type_ cx force_general t
        | DefT (r, SingletonStrT { from_annot = false; value }) ->
          if force_general || Context.natural_inference_local_primitive_literals_full cx then
            DefT (replace_desc_reason RString r, StrGeneralT AnyLiteral)
          else
            DefT (r, StrT_UNSOUND (None, value))
        | DefT (r, SingletonNumT { from_annot = false; value }) ->
          if force_general || Context.natural_inference_local_primitive_literals_full cx then
            DefT (replace_desc_reason RNumber r, NumGeneralT AnyLiteral)
          else
            DefT (r, NumT_UNSOUND (None, value))
        | DefT (r, SingletonBoolT { from_annot = false; value }) ->
          if force_general || Context.natural_inference_local_primitive_literals_full cx then
            DefT (replace_desc_reason RBoolean r, BoolGeneralT)
          else
            DefT (r, BoolT_UNSOUND value)
        | DefT (r, SingletonBigIntT { from_annot = false; value }) ->
          if force_general || Context.natural_inference_local_primitive_literals_full cx then
            DefT (replace_desc_reason RBigInt r, BigIntGeneralT AnyLiteral)
          else
            DefT (r, BigIntT_UNSOUND (None, value))
        | _ -> t
    end
  in
  (fun cx ~force_general t -> mapper#type_ cx force_general t)

let aloc_contains ~outer ~inner =
  try
    let outer = ALoc.to_loc_exn outer in
    let inner = ALoc.to_loc_exn inner in
    Loc.contains outer inner
  with
  | _ -> false

let rec needs_generalization cx t =
  match t with
  | DefT (_, SingletonStrT { from_annot = false; _ })
  | DefT (_, SingletonBoolT { from_annot = false; _ })
  | DefT (_, SingletonNumT { from_annot = false; _ })
  | DefT (_, SingletonBigIntT { from_annot = false; _ }) ->
    true
  | OpenT (_, id) -> begin
    match Context.find_constraints cx id with
    | (_, Type.Constraint.FullyResolved s) ->
      needs_generalization cx (Context.force_fully_resolved_tvar cx s)
    | _ -> false
  end
  | UnionT (_, rep) ->
    let members = UnionRep.members rep in
    List.exists (needs_generalization cx) members
  | _ -> false

let loc_has_hint cx loc =
  let open Hint in
  let { Loc_env.ast_hint_map; _ } = Context.environment cx in
  match Loc_collections.ALocMap.find_opt loc ast_hint_map with
  | None
  | Some [] ->
    false
  | Some hints ->
    Base.List.exists hints ~f:(fun hint ->
        match hint with
        | Hint_Placeholder
        | Hint_t (_, ExpectedTypeHint)
        | Hint_Decomp (_, _, ExpectedTypeHint) ->
          true
        | Hint_t (_, BestEffortHint)
        | Hint_Decomp (_, _, BestEffortHint) ->
          false
    )

let enclosing_context_needs_precise = function
  | NoContext -> false
  | SwitchTest _
  | OtherTest
  | IndexContext
  | JsxTitleNameContext ->
    true

(* When do we need to preserve a precise primitive literal type?
 *
 * 1. The enclosing context is such that requires increased precision:
 *    a. conditional contexts, since this means that some refinement
 *       will be performed, and so precise types can give us more accurate
 *       results, and
 *    b. index contexts, since this allows for more precise operations over
 *       the object or array that is being accessed.
 *
 * 2. We are initializing a const-variable.
 *
 * 3. We are in an `as const` context.
 *
 * 4. We are computing a property of a frozen object.
 *
 * 5. The value is about to be compared against an annotation. We use the presence
 *    of a hint as a proxy to determine this. If the primitive literal is part of
 *    another container literal expression, we might not have a hint recorded for
 *    it. For this reason, we first check if the enclosing expression has a hint.
 *    This is done by forcing the value of `has_hint` that has been accumulated
 *    until this point. For example, in `{f:['a']}`, if the outer object has a
 *    hint then `'a'` is also considered to have a hint. Finally, if none of the
 *    above hold, we check for a hint for the target location.
 *)
let needs_precise_type cx ~encl_ctx ~decl ~as_const ~frozen ~has_hint loc =
  enclosing_context_needs_precise encl_ctx
  || decl = Some Ast.Variable.Const
  || as_const
  || frozen = FrozenProp
  || Lazy.force has_hint
  || loc_has_hint cx loc

let adjust_precision cx reason syntactic_flags ~precise ~general loc =
  let { encl_ctx; decl; as_const; frozen; has_hint } = syntactic_flags in
  match Context.typing_mode cx with
  | Context.SynthesisMode { target_loc = Some target_loc }
    when aloc_contains ~outer:target_loc ~inner:loc ->
    Context.mk_placeholder cx reason
  | Context.SynthesisMode { target_loc = _ } ->
    Context.set_synthesis_produced_uncacheable_result cx;
    general ()
  | Context.CheckingMode
  | Context.HintEvaluationMode ->
    if needs_precise_type cx ~encl_ctx ~decl ~as_const ~frozen ~has_hint loc then
      precise ()
    else
      general ()

let try_generalize cx syntactic_flags loc t =
  if Context.natural_inference_local_primitive_literals_partial cx then
    if needs_generalization cx t then
      let general () = generalize_singletons cx ~force_general:true t in
      let precise () = t in
      adjust_precision cx (TypeUtil.reason_of_t t) syntactic_flags ~precise ~general loc
    else
      t
  else
    t

let primitive_literal cx reason syntactic_flags ~legacy ~precise ~general loc =
  (* We need to at least have set the flag to "partial" to adjust based on hints. *)
  if Context.natural_inference_local_primitive_literals_partial cx then
    let general =
      if Context.natural_inference_local_primitive_literals_full cx then
        general
      else
        legacy
    in
    adjust_precision cx reason syntactic_flags ~precise ~general loc
  else
    legacy ()
