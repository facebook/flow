(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* AST handling for destructuring exprs *)

module Ast = Spider_monkey_ast

open Utils_js
open Reason
open Type

(**
 * Given a LHS destructuring pattern, extract a list of (loc, identifier-name)
 * tuples from the pattern that represent new bindings. This is primarily useful
 * for exporting a destructuring variable declaration.
 *)
let rec extract_destructured_bindings accum pattern = Ast.Pattern.(
  match pattern with
  | Identifier { Identifier.name; _ } ->
    name::accum

  | Object n ->
    let props = n.Object.properties in
    List.fold_left extract_obj_prop_pattern_bindings accum props

  | Array n ->
    let elems = n.Array.elements in
    List.fold_left extract_arr_elem_pattern_bindings accum elems

  | Assignment a ->
    extract_destructured_bindings accum (snd a.Assignment.left)

  | Expression _ ->
    failwith "Parser Error: Expression patterns don't exist in JS."
)

and extract_obj_prop_pattern_bindings accum = Ast.Pattern.(function
  | Object.Property (_, prop) ->
    let (_, rhs_pattern) = prop.Object.Property.pattern in
    extract_destructured_bindings accum rhs_pattern

  | Object.RestProperty _ ->
    failwith "Unsupported: Destructuring object spread properties"
)

and extract_arr_elem_pattern_bindings accum = Ast.Pattern.Array.(function
  | Some (Element (_, pattern)) ->
    extract_destructured_bindings accum pattern

  | Some (RestElement (_, {RestElement.argument = (_, pattern)})) ->
    extract_destructured_bindings accum pattern

  | None -> accum
)

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
            let key = NumT (
              mk_reason RNumber loc,
              Literal (float i, string_of_int i)
            ) in
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
              }))
            ) in
            let refinement = Option.bind init (fun init ->
              Refinement.get cx init reason
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
                  }))
                ) in
                let refinement = Option.bind init (fun init ->
                  Refinement.get cx init reason
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
                let reason = mk_reason (RCustom "computed property/element") loc in
                let init = Option.map init (fun init ->
                  loc, Ast.Expression.(Member Member.({
                    _object = init;
                    property = PropertyExpression key;
                    computed = true;
                  }))
                ) in
                let refinement = Option.bind init (fun init ->
                  Refinement.get cx init reason
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
                Flow_error.(add_output cx (EUnsupportedSyntax
                  (loc, DestructuringObjectPropertyLiteralNonString)))
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

  | loc, Identifier { Identifier.name = (_, name); _ } ->
      Type_inference_hooks_js.dispatch_lval_hook cx name loc (
        match (parent_pattern_t, init) with
        (**
         * If there was a parent_pattern, we must be within a destructuring
         * pattern and a `get-def` on this identifier should point at the "def"
         * of the original property. To accompish this, we emit the type of the
         * parent pattern so that get-def can dive in to that type and extract
         * the location of the "def" of this property.
         *)
        | (Some rhs_t, _) -> Type_inference_hooks_js.RHSType rhs_t

        (**
         * If there was no parent_pattern, we must not be within a destructuring
         * pattern and a `get-def` on this identifier should point at the
         * location of the RHS of the assignment.
         *)
        | (None, Some (loc, _)) -> Type_inference_hooks_js.RHSLoc loc

        (**
         * If there was no parent_pattern and no RHS expression (i.e. `var a;`,
         * function parameters, etc), there's nothing useful we can do for a
         * `get-def` on this identifier.
         *)
        | (None, None) -> Type_inference_hooks_js.NoRHS
      );
      f loc name default curr_t

  | loc, Assignment { Assignment.left; right } ->
      let default = Some (Default.expr ?default right) in
      let reason = mk_reason (RCustom "default value") loc in
      let tvar =
        EvalT (curr_t, DestructuringT (reason, Default), mk_id())
      in
      recurse ?parent_pattern_t tvar init default left

  | loc, Expression _ ->
      Flow_error.(add_output cx (EUnsupportedSyntax
        (loc, DestructuringExpressionPattern)))

  in fun t init default pattern -> recurse t init default pattern
)


let type_of_pattern = Ast.Pattern.(function
  | _, Array { Array.typeAnnotation; _; } -> typeAnnotation

  | _, Object { Object.typeAnnotation; _; } -> typeAnnotation

  | _, Identifier { Identifier.typeAnnotation; _; } -> typeAnnotation

  | _, _ -> None
)
(* instantiate pattern visitor for assignments *)
let destructuring_assignment cx ~expr rhs_t init =
  let f loc name _default t =
    (* TODO destructuring+defaults unsupported in assignment expressions *)
    let reason = mk_reason (RIdentifierAssignment name) loc in
    ignore Env.(set_var cx name t reason)
  in
  destructuring cx ~expr rhs_t (Some init) None ~f
