(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AST handling for destructuring exprs *)


open Utils_js
open Reason
open Type

(* Destructuring visitor for tree-shaped patterns, parameteric over an action f
   to perform at the leaves. A type for the pattern is passed, which is taken
   apart as the visitor goes deeper. *)

(** NOTE: Since the type of the pattern may contain (unsubstituted) type
    parameters, it is important that this visitor does not emit constraints:
    otherwise, we may end up with (unsubstituted) type parameters appearing as
    lower or upper bounds of constraints, which would violate a core
    invariant. So, instead we model the operation of destructuring with a
    wrapper constructor, `DestructuringT` (with lazy evaluation rules that
    trigger whenever a result is needed, e.g., to interact in flows with other
    lower and upper bounds). **)

let destructuring cx ~expr ~f = Ast.Pattern.(
  let rec recurse ?parent_pattern_t curr_t init default = function
  | _, Array { Array.elements; _; } -> Array.(
      elements |> List.iteri (fun i -> function
        | Some (Element ((loc, _) as p)) ->
            let key = DefT (mk_reason RNumber loc, NumT (
              Literal (None, (float i, string_of_int i))
            )) in
            let reason = mk_reason (RCustom (spf "element %d" i)) loc in
            let init = Option.map init (fun init ->
              loc, Ast.Expression.(Member Member.({
                _object = init;
                property = PropertyExpression (
                  loc,
                  Ast.Expression.Literal { Ast.Literal.
                    value = Ast.Literal.Number (float i);
                    raw = string_of_int i;
                  }
                );
                computed = true;
                optional = false;
              }))
            ) in
            let refinement = Option.bind init (fun init ->
              Refinement.get cx init loc
            ) in
            let parent_pattern_t, tvar = (match refinement with
            | Some refined_t -> refined_t, refined_t
            | None ->
                curr_t,
                EvalT (curr_t, DestructuringT (reason, Elem key), mk_id())
            ) in
            let default = Option.map default (Default.elem key reason) in
            recurse ~parent_pattern_t tvar init default p
        | Some (RestElement (loc, { RestElement.argument = p })) ->
            let reason = mk_reason RArrayPatternRestProp loc in
            let tvar =
              EvalT (curr_t, DestructuringT (reason, ArrRest i), mk_id())
            in
            let default = Option.map default (Default.arr_rest i reason) in
            recurse ~parent_pattern_t:curr_t tvar init default p
        | None ->
            ()
      )
    )

  | _, Object { Object.properties; _; } -> Object.(
      let xs = ref [] in
      properties |> List.iter (function
        | Property (loc, prop) ->
            begin match prop with
            | { Property.
                key = Property.Identifier (loc, name);
                pattern = p; _;
              }
            | { Property.key =
                  Property.Literal (loc, { Ast.Literal.
                    value = Ast.Literal.String name; _ });
                pattern = p; _; }
              ->
                let reason = mk_reason (RProperty (Some name)) loc in
                xs := name :: !xs;
                let init = Option.map init (fun init ->
                  loc, Ast.Expression.(Member Member.({
                    _object = init;
                    property = PropertyIdentifier (loc, name);
                    computed = false;
                    optional = false;
                  }))
                ) in
                let refinement = Option.bind init (fun init ->
                  Refinement.get cx init loc
                ) in
                let parent_pattern_t, tvar = (match refinement with
                | Some refined_t -> refined_t, refined_t
                | None ->
                  (* use the same reason for the prop name and the lookup.
                     given `var {foo} = ...`, `foo` is both. compare to `a.foo`
                     where `foo` is the name and `a.foo` is the lookup. *)
                    curr_t,
                    EvalT (curr_t, DestructuringT (reason, Prop name), mk_id())
                ) in
                let default = Option.map default (Default.prop name reason) in
                recurse ~parent_pattern_t tvar init default p
            | { Property.key = Property.Computed key; pattern = p; _; } ->
                let key_t = expr cx key in
                let loc = fst key in
                let reason = mk_reason (RProperty None) loc in
                let init = Option.map init (fun init ->
                  loc, Ast.Expression.(Member Member.({
                    _object = init;
                    property = PropertyExpression key;
                    computed = true;
                    optional = false;
                  }))
                ) in
                let refinement = Option.bind init (fun init ->
                  Refinement.get cx init loc
                ) in
                let parent_pattern_t, tvar = (match refinement with
                | Some refined_t -> refined_t, refined_t
                | None ->
                    curr_t,
                    EvalT (curr_t, DestructuringT (reason, Elem key_t), mk_id())
                ) in
                let default = Option.map default (Default.elem key_t reason) in
                recurse ~parent_pattern_t tvar init default p
            | { Property.key = Property.Literal _; _ } ->
                Flow_js.add_output cx Flow_error.(EUnsupportedSyntax
                  (loc, DestructuringObjectPropertyLiteralNonString))
            end

        | RestProperty (loc, { RestProperty.argument = p }) ->
            let reason = mk_reason RObjectPatternRestProp loc in
            let tvar =
              EvalT (curr_t, DestructuringT (reason, ObjRest !xs), mk_id())
            in
            let default = Option.map default (Default.obj_rest !xs reason) in
            recurse ~parent_pattern_t:curr_t tvar init default p
      )
    )

  | loc, Identifier { Identifier.name = (id_loc, name); _ } ->
      Type_inference_hooks_js.dispatch_lval_hook cx name loc (
        match parent_pattern_t with
        (**
         * If there was a parent_pattern, we must be within a destructuring
         * pattern and a `get-def` on this identifier should point at the "def"
         * of the original property. To accompish this, we emit the type of the
         * parent pattern so that get-def can dive in to that type and extract
         * the location of the "def" of this property.
         *)
        | Some rhs_t -> Type_inference_hooks_js.Parent rhs_t

        (**
         * If there was no parent_pattern, we must not be within a destructuring
         * pattern and a `get-def` on this identifier should point at the
         * location where the binding is introduced.
         *)
        | None -> Type_inference_hooks_js.Id
      );
      let curr_t = mod_reason_of_t (replace_reason (function
      | RDefaultValue
      | RArrayPatternRestProp
      | RObjectPatternRestProp
        -> RIdentifier name
      | desc -> desc
      )) curr_t in
      let id_info = name, curr_t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) id_loc id_info;
      let use_op = Op (AssignVar {
        var = Some (mk_reason (RIdentifier name) loc);
        init = (match init with
        | Some init -> mk_expression_reason init
        | None -> reason_of_t curr_t);
      }) in
      f ~use_op loc name default curr_t

  | loc, Assignment { Assignment.left; right } ->
      let default = Some (Default.expr ?default right) in
      let reason = mk_reason RDefaultValue loc in
      let tvar =
        EvalT (curr_t, DestructuringT (reason, Default), mk_id())
      in
      recurse ?parent_pattern_t tvar init default left

  | loc, Expression _ ->
      Flow_js.add_output cx Flow_error.(EUnsupportedSyntax
        (loc, DestructuringExpressionPattern))

  in fun t init default pattern -> recurse t init default pattern
)


let type_of_pattern = Ast.Pattern.(function
  | _, Array { Array.annot; _; } -> annot

  | _, Object { Object.annot; _; } -> annot

  | _, Identifier { Identifier.annot; _; } -> annot

  | _, _ -> None
)
(* instantiate pattern visitor for assignments *)
let destructuring_assignment cx ~expr rhs_t init =
  let f ~use_op loc name _default t =
    (* TODO destructuring+defaults unsupported in assignment expressions *)
    ignore Env.(set_var cx ~use_op name t loc)
  in
  destructuring cx ~expr rhs_t (Some init) None ~f
