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
module FlowError = Flow_error

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
  | Identifier (loc, n) ->
    let name = n.Ast.Identifier.name in
    (loc, name)::accum

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

  | Object.SpreadProperty _ ->
    failwith "Unsupported: Destructuring object spread properties"
)

and extract_arr_elem_pattern_bindings accum = Ast.Pattern.(function
  | Some (Array.Element (_, pattern)) ->
    extract_destructured_bindings accum pattern

  | Some (Array.Spread (_, {Array.SpreadElement.argument = (_, pattern)})) ->
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

let rec destructuring ?parent_pattern_t cx curr_t init default f =
  Ast.Pattern.(function
  | _, Array { Array.elements; _; } -> Array.(
      elements |> List.iteri (fun i -> function
        | Some (Element ((loc, _) as p)) ->
            let key = NumT (
              mk_reason "number" loc,
              Literal (float i, string_of_int i)
            ) in
            let reason = mk_reason (spf "element %d" i) loc in
            let init = Option.map init (fun expr ->
              loc, Ast.Expression.(Member Member.({
                _object = expr;
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
            let refinement = Option.bind init (fun expr ->
              Refinement.get cx expr reason
            ) in
            let parent_pattern_t, tvar = (match refinement with
            | Some refined_t -> refined_t, refined_t
            | None ->
                curr_t,
                EvalT (curr_t, DestructuringT (reason, Elem key), mk_id())
            ) in
            let default = Option.map default (Default.elem key reason) in
            destructuring ~parent_pattern_t cx tvar init default f p
        | Some (Spread (loc, { SpreadElement.argument = p })) ->
            let reason = mk_reason "rest of array pattern" loc in
            let tvar =
              EvalT (curr_t, DestructuringT (reason, ArrRest i), mk_id())
            in
            let default = Option.map default (Default.arr_rest i reason) in
            destructuring ~parent_pattern_t:curr_t cx tvar init default f p
        | None ->
            ()
      )
    )

  | _, Object { Object.properties; _; } -> Object.(
      let xs = ref [] in
      properties |> List.iter (function
        | Property (loc, prop) ->
            begin match prop with
            | { Property.key = Property.Identifier (loc, id);
                pattern = p; _;
              } ->
                let x = id.Ast.Identifier.name in
                let reason = mk_reason (spf "property `%s`" x) loc in
                xs := x :: !xs;
                let init = Option.map init (fun expr ->
                  loc, Ast.Expression.(Member Member.({
                    _object = expr;
                    property = PropertyIdentifier (loc, id);
                    computed = false;
                  }))
                ) in
                let refinement = Option.bind init (fun expr ->
                  Refinement.get cx expr reason
                ) in
                let parent_pattern_t, tvar = (match refinement with
                | Some refined_t -> refined_t, refined_t
                | None ->
                  (* use the same reason for the prop name and the lookup.
                     given `var {foo} = ...`, `foo` is both. compare to `a.foo`
                     where `foo` is the name and `a.foo` is the lookup. *)
                    curr_t,
                    EvalT (curr_t, DestructuringT (reason, Prop x), mk_id())
                ) in
                let default = Option.map default (Default.prop x reason) in
                destructuring ~parent_pattern_t cx tvar init default f p
            | _ ->
              error_destructuring cx loc
            end

        | SpreadProperty (loc, { SpreadProperty.argument = p }) ->
            let reason = mk_reason "object pattern spread property" loc in
            let tvar =
              EvalT (curr_t, DestructuringT (reason, ObjRest !xs), mk_id())
            in
            let default = Option.map default (Default.obj_rest !xs reason) in
            destructuring ~parent_pattern_t:curr_t cx tvar init default f p
      )
    )

  | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
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
      f cx loc name default curr_t

  | loc, Assignment { Assignment.left; right } ->
      let default = Some (Default.expr ?default right) in
      let reason = mk_reason "default value" loc in
      let tvar =
        EvalT (curr_t, DestructuringT (reason, Default), mk_id())
      in
      destructuring ?parent_pattern_t cx tvar init default f left

  | loc, _ -> error_destructuring cx loc
)

and error_destructuring cx loc =
  FlowError.add_error cx (loc, ["unsupported destructuring"])

let type_of_pattern = Ast.Pattern.(function
  | _, Array { Array.typeAnnotation; _; } -> typeAnnotation

  | _, Object { Object.typeAnnotation; _; } -> typeAnnotation

  | _, Identifier (_, { Ast.Identifier.typeAnnotation; _; }) -> typeAnnotation

  | _, _ -> None
)
(* instantiate pattern visitor for assignments *)
let destructuring_assignment cx rhs_t init =
  destructuring cx rhs_t (Some init) None (fun cx loc name _default t ->
    (* TODO destructuring+defaults unsupported in assignment expressions *)
    let reason = mk_reason (spf "assignment of identifier `%s`" name) loc in
    ignore Env.(set_var cx name t reason)
  )
