(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Destructuring visitor for tree-shaped patterns, parameteric over an action f
   to perform at the leaves. A type for the pattern is passed, which is taken
   apart as the visitor goes deeper. *)

(** TODO currently type annotations internal to patterns get parsed but not
  * checked. We should either update this to give users a warning that internal
  * annotations aren't checked, or update this to check internal annotations.
  *)

module Ast = Flow_ast
module Tast_utils = Typed_ast_utils

open Reason
open Type

type state = {
  parent: Type.t option;
  current: Type.t;
  init: (ALoc.t, ALoc.t) Flow_ast.Expression.t option;
  default: Type.t Default.t option;
}

type expr =
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

type callback =
  use_op:Type.use_op ->
  ALoc.t ->
  string ->
  Type.t Default.t option ->
  Type.t ->
  unit

let empty ?init ?default current = {
  parent = None;
  current;
  init;
  default;
}

let pattern_default cx ~expr acc = function
  | None -> acc, None
  | Some e ->
    let { current; default; _ } = acc in
    let (loc, t), _ as e = expr cx e in
    let default = Some (Default.expr ?default t) in
    let reason = mk_reason RDefaultValue loc in
    let current = Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, Default, tout))
    ) in
    let acc = { acc with current; default } in
    acc, Some e

let array_element cx acc i loc =
  let { current; init; default; _ } = acc in
  let key = DefT (mk_reason RNumber loc, bogus_trust (), NumT (
    Literal (None, (float i, string_of_int i))
  )) in
  let reason = mk_reason (RCustom (Utils_js.spf "element %d" i)) loc in
  let init = Option.map init (fun init ->
    loc, Ast.Expression.(Member Member.({
      _object = init;
      property = PropertyExpression (
        loc,
        Ast.Expression.Literal { Ast.Literal.
          value = Ast.Literal.Number (float i);
          raw = string_of_int i;
          comments = Flow_ast_utils.mk_comments_opt ()
        }
      );
    }))
  ) in
  let refinement = Option.bind init (fun init ->
    Refinement.get cx init loc
  ) in
  let parent, current = match refinement with
  | Some t -> None, t
  | None ->
    Some current,
    Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, Elem key, tout)))
  in
  let default = Option.map default (Default.elem key reason) in
  { parent; current; init; default }

let array_rest_element cx acc i loc =
  let { current; default; _ } = acc in
  let reason = mk_reason RArrayPatternRestProp loc in
  let parent, current =
    Some current,
    Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, ArrRest i, tout)))
  in
  let default = Option.map default (Default.arr_rest i reason) in
  { acc with parent; current; default }

let object_named_property cx acc loc x comments =
  let { current; init; default; _ } = acc in
  let reason = mk_reason (RProperty (Some x)) loc in
  let init = Option.map init (fun init ->
    loc, Ast.Expression.(Member Member.({
      _object = init;
      property = PropertyIdentifier (loc, { Ast.Identifier.name= x; comments });
    }))
  ) in
  let refinement = Option.bind init (fun init ->
    Refinement.get cx init loc
  ) in
  let parent, current = match refinement with
  | Some t -> None, t
  | None ->
    (* use the same reason for the prop name and the lookup.
       given `var {foo} = ...`, `foo` is both. compare to `a.foo`
       where `foo` is the name and `a.foo` is the lookup. *)
    Some current,
    Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, Prop x, tout)))
  in
  let default = Option.map default (Default.prop x reason) in
  let () = match parent with
  | None -> () (* TODO: get-def when object property is refined *)
  | Some t ->
    (**
     * We are within a destructuring pattern and a `get-def` on this identifier should
     * point at the "def" of the original property. To accompish this, we emit the type
     * of the parent pattern so that get-def can dive in to that type and extract the
     * location of the "def" of this property.
     *)
    Type_inference_hooks_js.dispatch_lval_hook cx x loc
      (Type_inference_hooks_js.Parent t);
  in
  { parent; current; init; default }

let object_computed_property cx ~expr acc e =
  let { current; init; default; _ } = acc in
  let (loc, t), _ as e' = expr cx e in
  let reason = mk_reason (RProperty None) loc in
  let init = Option.map init (fun init ->
    loc, Ast.Expression.(Member Member.({
      _object = init;
      property = PropertyExpression e;
    }))
  ) in
  let refinement = Option.bind init (fun init ->
    Refinement.get cx init loc
  ) in
  let parent, current = match refinement with
  | Some t -> None, t
  | None ->
    Some current,
    Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, Elem t, tout)))
  in
  let default = Option.map default (Default.elem t reason) in
  { parent; current; init; default }, e'

let object_rest_property cx acc xs loc =
  let { current; default; _ } = acc in
  let reason = mk_reason RObjectPatternRestProp loc in
  let parent, current =
    Some current,
    Tvar.mk_where cx reason (fun tout ->
      Flow_js.flow cx (current, DestructuringT (reason, ObjRest xs, tout)))
  in
  let default = Option.map default (Default.obj_rest xs reason) in
  { acc with parent; current; default }

let object_property cx ~expr (acc: state) xs (key: (ALoc.t, ALoc.t) Ast.Pattern.Object.Property.key):
  (state * string list * (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Object.Property.key)
  =
  let open Ast.Pattern.Object in
  match key with
  | Property.Identifier (loc, { Ast.Identifier.name = x; comments }) ->
    let acc = object_named_property cx acc loc x comments in
    acc, x::xs,
    Property.Identifier ((loc, acc.current), {
      Ast.Identifier.name = x;
      comments
    })
  | Property.Literal (loc, ({ Ast.Literal.value = Ast.Literal.String x; _ } as lit)) ->
    let acc = object_named_property cx acc loc x None in
    acc, x::xs,
    Property.Literal (loc, lit)
  | Property.Computed e ->
    let acc, e = object_computed_property cx ~expr acc e in
    acc, xs, Property.Computed e
  | Property.Literal (loc, _) ->
    Flow_js.add_output cx Error_message.(EUnsupportedSyntax
      (loc, DestructuringObjectPropertyLiteralNonString));
    acc, xs, Tast_utils.error_mapper#pattern_object_property_key key

let identifier cx ~f acc loc name =
  let { parent; current; init; default } = acc in
  let () = match parent with
  (* If there was a parent pattern, we already dispatched the hook if relevant. *)
  | Some _ -> ()
  (**
   * If there was no parent_pattern, we must not be within a destructuring
   * pattern and a `get-def` on this identifier should point at the
   * location where the binding is introduced.
   *)
  | None ->
    Type_inference_hooks_js.dispatch_lval_hook cx name loc Type_inference_hooks_js.Id
  in
  let current = mod_reason_of_t (replace_reason (function
    | RDefaultValue
    | RArrayPatternRestProp
    | RObjectPatternRestProp
      -> RIdentifier name
    | desc -> desc
  )) current in
  let use_op = Op (AssignVar {
    var = Some (mk_reason (RIdentifier name) loc);
    init = (
      match init with
      | Some init -> mk_expression_reason init
      | None -> reason_of_t current
    );
  }) in
  f ~use_op loc name default current

let rec pattern cx ~expr ~f acc (loc, p) =
  let open Ast.Pattern in
  (loc, acc.current), match p with
  | Array { Array.elements; annot } ->
    let elements = array_elements cx ~expr ~f acc elements in
    let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
    Array { Array.elements; annot }
  | Object { Object.properties; annot } ->
    let properties = object_properties cx ~expr ~f acc properties in
    let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
    Object { Object.properties; annot }
  | Identifier { Identifier.name = id; optional; annot } ->
    let id_loc, { Ast.Identifier.name; comments } = id in
    let id = (id_loc, acc.current), { Ast.Identifier.name; comments } in
    let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
    identifier cx ~f acc id_loc name;
    Identifier { Identifier.name = id; optional; annot }
  | Expression e ->
    Flow_js.add_output cx Error_message.(EUnsupportedSyntax
      (loc, DestructuringExpressionPattern));
    Expression (Tast_utils.error_mapper#expression e)

and array_elements cx ~expr ~f acc =
  let open Ast.Pattern.Array in
  List.mapi (fun i -> Option.map ~f:(
    function
    | Element (loc, { Element.argument = p; default = d }) ->
      let acc = array_element cx acc i loc in
      let acc, d = pattern_default cx ~expr acc d in
      let p = pattern cx ~expr ~f acc p in
      Element (loc, { Element.argument = p; default = d })
    | RestElement (loc, { RestElement.argument = p }) ->
      let acc = array_rest_element cx acc i loc in
      let p = pattern cx ~expr ~f acc p in
      RestElement (loc, { RestElement.argument = p })
  ))

and object_properties =
  let open Ast.Pattern.Object in
  let prop cx ~expr ~f acc xs p =
    match p with
    | Property (loc, { Property.key; pattern = p; default = d; shorthand }) ->
      let acc, xs, key = object_property cx ~expr acc xs key in
      let acc, d = pattern_default cx ~expr acc d in
      let p = pattern cx ~expr ~f acc p in
      xs, Property (loc, { Property.key; pattern = p; default = d; shorthand })
    | RestProperty (loc, { RestProperty.argument = p }) ->
      let acc = object_rest_property cx acc xs loc in
      let p = pattern cx ~expr ~f acc p in
      xs, RestProperty (loc, { RestProperty.argument = p })
  in
  let rec loop cx ~expr ~f acc xs rev_ps = function
    | [] -> List.rev rev_ps
    | p::ps ->
      let xs, p = prop cx ~expr ~f acc xs p in
      loop cx ~expr ~f acc xs (p::rev_ps) ps
  in
  fun cx ~expr ~f acc ps ->
    loop cx ~expr ~f acc [] [] ps

let type_of_pattern (_, p) =
  let open Ast.Pattern in
  match p with
  | Array { Array.annot; _ }
  | Object { Object.annot; _ }
  | Identifier { Identifier.annot; _ }
    -> annot
  | _ -> Ast.Type.Missing ALoc.none

(* instantiate pattern visitor for assignments *)
let assignment cx ~expr rhs_t init =
  let acc = empty ~init rhs_t in
  let f ~use_op loc name _default t =
    (* TODO destructuring+defaults unsupported in assignment expressions *)
    ignore Env.(set_var cx ~use_op name t loc)
  in
  pattern cx ~expr ~f acc
