(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Enclosing_context
open Type
module Ast = Flow_ast

type syntactic_flags = {
  encl_ctx: enclosing_context;
  decl: Ast.Variable.kind option;
  as_const: bool;
  frozen: Type.frozen_kind;
  has_hint: bool Lazy.t;
}

let empty_syntactic_flags =
  {
    encl_ctx = NoContext;
    decl = None;
    as_const = false;
    frozen = Type.NotFrozen;
    has_hint = lazy false;
  }

let mk_syntactic_flags
    ?(encl_ctx = NoContext)
    ?decl
    ?(as_const = false)
    ?(frozen = Type.NotFrozen)
    ?(has_hint = lazy false)
    () =
  { encl_ctx; decl; as_const; frozen; has_hint }

let is_builtin_promise =
  let rec loop cx seen = function
    | OpenT (_, id) ->
      let (root_id, constraints) = Context.find_constraints cx id in
      if ISet.mem root_id seen then
        false
      else (
        match constraints with
        | Type.Constraint.FullyResolved s ->
          let t = Context.force_fully_resolved_tvar cx s in
          loop cx (ISet.add root_id seen) t
        | _ -> false
      )
    | DefT (_, PolyT { t_out = DefT (r, ClassT _); _ }) when Reason.is_promise_reason r -> true
    | _ -> false
  in
  (fun cx t -> loop cx ISet.empty t)

let is_literal_union r rep =
  let open UnionRep in
  match union_kind rep with
  | ConditionalKind
  | ImplicitInstiationKind
  | LogicalKind ->
    true
  | ProvidersKind
  | ResolvedKind
  | UnknownKind ->
    let open Reason in
    (match desc_of_reason r with
    | RInferredUnionElemArray _ -> true
    | _ -> false)

type singleton_action =
  | DoNotKeep
  | KeepAsIs
  | KeepAsConst

(* `literal_type_mapper` walks a literal type and replaces singleton types that
 * originate from literals according to `singleton_action`.
 *
 * A key assumption is that `t` is the type inferred for a literal expression
 * (object, array, primitive literal) or the result of a conditional or logical
 * expression. This allows us to keep the cost of this visit low, by only
 * descending into types that are considered literals (for now we use reasons to
 * determine this.)
 *)
class literal_type_mapper ~singleton_action =
  let open Reason in
  let singleton_str t r value =
    match singleton_action (loc_of_reason r) with
    | KeepAsIs -> t
    | KeepAsConst -> DefT (r, SingletonStrT { from_annot = true; value })
    | DoNotKeep -> DefT (replace_desc_reason RString r, StrGeneralT AnyLiteral)
  in
  let singleton_num t r value =
    match singleton_action (loc_of_reason r) with
    | KeepAsIs -> t
    | KeepAsConst -> DefT (r, SingletonNumT { from_annot = true; value })
    | DoNotKeep -> DefT (replace_desc_reason RNumber r, NumGeneralT AnyLiteral)
  in
  let singleton_bool t r value =
    match singleton_action (loc_of_reason r) with
    | KeepAsIs -> t
    | KeepAsConst -> DefT (r, SingletonBoolT { from_annot = true; value })
    | DoNotKeep -> DefT (replace_desc_reason RBoolean r, BoolGeneralT)
  in
  let singleton_bigint t r value =
    match singleton_action (loc_of_reason r) with
    | KeepAsIs -> t
    | KeepAsConst -> DefT (r, SingletonBigIntT { from_annot = true; value })
    | DoNotKeep -> DefT (replace_desc_reason RBigInt r, BigIntGeneralT AnyLiteral)
  in
  object (self)
    inherit [ISet.t] Type_mapper.t as super

    method exports _cx _map_cx id = id

    method props cx map_cx id =
      let props_map = Context.find_props cx id in
      let props_map' =
        NameUtils.Map.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props_map
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

    method tvar cx map_cx _r id =
      match Context.find_constraints cx id with
      | (root_id, Type.Constraint.FullyResolved s) ->
        if ISet.mem root_id map_cx then
          id
        else
          let map_cx = ISet.add root_id map_cx in
          let t = Context.force_fully_resolved_tvar cx s in
          let t' = self#type_ cx map_cx t in
          if t == t' then
            id
          else
            Tvar.mk_fully_resolved_no_wrap cx t'
      | _ -> id

    method! type_ cx map_cx t =
      match t with
      | OpenT _ -> super#type_ cx map_cx t
      | DefT (r, ArrT _) when is_literal_array_reason r -> super#type_ cx map_cx t
      | DefT (r, ObjT _) when is_literal_object_reason r -> super#type_ cx map_cx t
      | DefT (r, FunT _) when is_literal_function_reason r -> super#type_ cx map_cx t
      | TypeAppT { type_; _ } when is_builtin_promise cx type_ ->
        (* async expressions will wrap result in Promise<>, so we need to descend here *)
        super#type_ cx map_cx t
      | UnionT (r, rep) when is_literal_union r rep -> super#type_ cx map_cx t
      | IntersectionT (_, rep)
        when match InterRep.inter_kind rep with
             | InterRep.ImplicitInstiationKind -> true
             | InterRep.UnknownKind -> false ->
        super#type_ cx map_cx t
      | DefT (r, SingletonStrT { from_annot = false; value }) -> singleton_str t r value
      | DefT (r, SingletonNumT { from_annot = false; value }) -> singleton_num t r value
      | DefT (r, SingletonBoolT { from_annot = false; value }) -> singleton_bool t r value
      | DefT (r, SingletonBigIntT { from_annot = false; value }) -> singleton_bigint t r value
      | _ -> t
  end

let convert_literal_type cx ~singleton_action t =
  let mapper = new literal_type_mapper ~singleton_action in
  mapper#type_ cx ISet.empty t

let rec is_literal_type cx seen t =
  let open Reason in
  match t with
  | OpenT (_, id) -> begin
    match Context.find_constraints cx id with
    | (root_id, Type.Constraint.FullyResolved s) ->
      if ISet.mem root_id seen then
        false
      else
        let t = Context.force_fully_resolved_tvar cx s in
        is_literal_type cx (ISet.add root_id seen) t
    | _ -> false
  end
  | DefT (r, ArrT _) -> is_literal_array_reason r
  | DefT (r, ObjT _) -> is_literal_object_reason r
  | DefT (r, FunT _) -> is_literal_function_reason r
  | DefT (_, SingletonStrT { from_annot = false; _ })
  | DefT (_, SingletonNumT { from_annot = false; _ })
  | DefT (_, SingletonBoolT { from_annot = false; _ })
  | DefT (_, SingletonBigIntT { from_annot = false; _ }) ->
    true
  | _ -> false

class implicit_instantiation_literal_mapper ~singleton_action =
  object (self)
    inherit [unit] Type_subst.type_subst_base as super

    method! type_ cx map_cx t =
      if is_literal_type cx ISet.empty t then
        convert_literal_type cx ~singleton_action t
      else
        match t with
        | EvalT (x, TypeDestructorT (op, r, d), _) ->
          let x' = self#type_ cx map_cx x in
          let d' = self#destructor cx map_cx d in
          if x == x' && d == d' then
            t
          else
            Flow_cache.Eval.id cx x' (TypeDestructorT (op, r, d'))
        | OpenT _
        | DefT _
        | ThisInstanceT _
        | ThisTypeAppT _
        | TypeAppT _
        | GenericT _
        | OpaqueT _
        | OptionalT _
        | MaybeT _
        | IntersectionT _
        | UnionT _ ->
          super#type_ cx map_cx t
        (* Stop here cause we need the precision *)
        | AnnotT _
        | KeysT _ ->
          t
        (* Stop cause there's nothing interesting *)
        | FunProtoT _
        | ObjProtoT _
        | NullProtoT _
        | FunProtoBindT _
        | NamespaceT _
        | StrUtilT _
        | AnyT _ ->
          t

    (* The EvalT case is the only case that calls this function. We've explicitly
     * overridden it in all cases, so this should never be called *)
    method eval_id _cx _map_cx _id = assert false
  end

let convert_implicit_instantiation_literal_type cx ~singleton_action t =
  let mapper = new implicit_instantiation_literal_mapper ~singleton_action in
  mapper#type_ cx () t

let aloc_contains ~outer ~inner =
  try
    let outer = ALoc.to_loc_exn outer in
    let inner = ALoc.to_loc_exn inner in
    Loc.contains outer inner
  with
  | _ -> false

let convert_literal_type_to_const ~loc_range =
  let open Reason in
  let reason_def_loc_within_call reason =
    aloc_contains ~outer:loc_range ~inner:(Reason.def_loc_of_reason reason)
  in
  let singleton_action _ = KeepAsConst in
  let mapper =
    object (self)
      inherit literal_type_mapper ~singleton_action as super

      method! type_ cx map_cx t =
        match t with
        | DefT
            (r, ArrT (ArrayAT { elem_t; tuple_view = Some (TupleView { elements; arity; _ }); _ }))
          ->
          if is_literal_array_reason r && reason_def_loc_within_call r then
            let elem_t = self#type_ cx map_cx elem_t in
            let elements =
              Base.List.map
                elements
                ~f:(fun (TupleElement { reason; name; t; polarity = _; optional }) ->
                  let t = self#type_ cx map_cx t in
                  TupleElement { reason; name; t; polarity = Polarity.Positive; optional }
              )
            in
            let r = replace_desc_reason RConstArrayLit r in
            DefT (r, ArrT (TupleAT { elem_t; elements; react_dro = None; arity; inexact = false }))
          else
            t
        | DefT (r, ObjT _) ->
          if is_literal_object_reason r && reason_def_loc_within_call r then
            let t = super#type_ cx map_cx t in
            TypeUtil.mod_reason_of_t (replace_desc_reason RConstObjectLit) t
          else
            t
        | _ -> super#type_ cx map_cx t

      (* This method is only reachable through a literal object argument to this call. *)
      method! props cx map_cx id =
        let props_map = Context.find_props cx id in
        let props_map' =
          NameUtils.Map.ident_map
            (fun p ->
              match p with
              | Field { preferred_def_locs; key_loc; type_; polarity = _ } ->
                let type_ = self#type_ cx map_cx type_ in
                Field { preferred_def_locs; key_loc; type_; polarity = Polarity.Positive }
              | _ -> p)
            props_map
        in
        let id' =
          if props_map == props_map' then
            id
          else
            Context.generate_property_map cx props_map'
        in
        id'
    end
  in
  (fun cx t -> mapper#type_ cx ISet.empty t)

let rec is_generalization_candidate cx seen t =
  match t with
  | DefT (_, SingletonStrT { from_annot = false; _ })
  | DefT (_, SingletonBoolT { from_annot = false; _ })
  | DefT (_, SingletonNumT { from_annot = false; _ })
  | DefT (_, SingletonBigIntT { from_annot = false; _ }) ->
    true
  | OpenT (_, id) -> begin
    match Context.find_constraints cx id with
    | (root_id, Type.Constraint.FullyResolved s) ->
      if ISet.mem root_id seen then
        false
      else
        let seen = ISet.add root_id seen in
        is_generalization_candidate cx seen (Context.force_fully_resolved_tvar cx s)
    | _ -> false
  end
  | UnionT (_, rep) ->
    let members = UnionRep.members rep in
    List.exists (is_generalization_candidate cx seen) members
  | _ -> false

let is_generalization_candidate cx t = is_generalization_candidate cx ISet.empty t

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
  | SwitchTestContext _
  | OtherTestContext
  | IndexContext
  | JsxTitleNameContext
  | JsxAttrOrChildrenContext
  | LiteralTestContext
  | MatchPattern
  | StrictComparison ->
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

let adjust_precision cx syntactic_flags ~precise ~general loc =
  let { encl_ctx; decl; as_const; frozen; has_hint } = syntactic_flags in
  match Context.typing_mode cx with
  | Context.SynthesisMode { target_loc = Some target_loc }
    when aloc_contains ~outer:target_loc ~inner:loc ->
    Context.mk_placeholder cx (TypeUtil.reason_of_t (general ()))
  | Context.SynthesisMode { target_loc = _ } ->
    Context.set_synthesis_produced_uncacheable_result cx;
    if needs_precise_type cx ~encl_ctx ~decl ~as_const ~frozen ~has_hint loc then
      precise ()
    else
      general ()
  | Context.CheckingMode
  | Context.HintEvaluationMode ->
    if needs_precise_type cx ~encl_ctx ~decl ~as_const ~frozen ~has_hint loc then
      precise ()
    else
      general ()

let try_generalize cx syntactic_flags loc t =
  if is_generalization_candidate cx t then
    let general () = convert_literal_type cx ~singleton_action:(fun _ -> DoNotKeep) t in
    let precise () = t in
    adjust_precision cx syntactic_flags ~precise ~general loc
  else
    t
