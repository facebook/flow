(*
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
open TypeUtil

type state = {
  parent: Type.t option;
  current: Type.t;
  init: (ALoc.t, ALoc.t) Flow_ast.Expression.t option;
  default: Type.t Default.t option;
  annot: bool;
}

type expr =
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

type callback =
  use_op:Type.use_op -> name_loc:ALoc.t -> string -> Type.t Default.t option -> Type.t -> Type.t

let empty ?init ?default ~annot current = { parent = None; current; init; default; annot }

let destruct cx reason ~annot selector t =
  let kind =
    if annot then
      DestructAnnot
    else
      DestructInfer
  in
  Tvar.mk_no_wrap_where cx reason (fun tout ->
      Flow_js.flow cx (t, DestructuringT (reason, kind, selector, tout, Reason.mk_id ())))

let pattern_default cx ~expr acc = function
  | None -> (acc, None)
  | Some e ->
    let { current; default; annot; _ } = acc in
    let (((loc, t), _) as e) = expr cx e in
    let default = Some (Default.expr ?default t) in
    let reason = mk_reason RDefaultValue loc in
    let current = destruct cx reason ~annot Default current in
    let acc = { acc with current; default } in
    (acc, Some e)

let array_element cx acc i loc =
  let { current; init; default; annot; _ } = acc in
  let key =
    DefT (mk_reason RNumber loc, bogus_trust (), NumT (Literal (None, (float i, string_of_int i))))
  in
  let reason = mk_reason (RCustom (Utils_js.spf "element %d" i)) loc in
  let init =
    Base.Option.map init (fun init ->
        ( loc,
          let open Ast.Expression in
          Member
            Member.
              {
                _object = init;
                property =
                  PropertyExpression
                    ( loc,
                      Ast.Expression.Literal
                        {
                          Ast.Literal.value = Ast.Literal.Number (float i);
                          raw = string_of_int i;
                          comments = None;
                        } );
                comments = None;
              } ))
  in
  let refinement =
    Base.Option.bind init (fun init -> Refinement.get ~allow_optional:true cx init loc)
  in
  let (parent, current) =
    match refinement with
    | Some t -> (None, t)
    | None -> (Some current, destruct cx reason ~annot (Elem key) current)
  in
  let default = Base.Option.map default (Default.elem key reason) in
  { acc with parent; current; init; default }

let array_rest_element cx acc i loc =
  let { current; default; annot; _ } = acc in
  let reason = mk_reason RArrayPatternRestProp loc in
  let (parent, current) = (Some current, destruct cx reason ~annot (ArrRest i) current) in
  let default = Base.Option.map default (Default.arr_rest i reason) in
  { acc with parent; current; default }

let object_named_property ~has_default cx acc loc x comments =
  let { current; init; default; annot; _ } = acc in
  let reason = mk_reason (RProperty (Some (OrdinaryName x))) loc in
  let init =
    Base.Option.map init (fun init ->
        ( loc,
          let open Ast.Expression in
          Member
            Member.
              {
                _object = init;
                property = PropertyIdentifier (loc, { Ast.Identifier.name = x; comments });
                comments = None;
              } ))
  in
  let refinement =
    Base.Option.bind init (fun init -> Refinement.get ~allow_optional:true cx init loc)
  in
  let default =
    Base.Option.map default (fun default ->
        let d = Default.prop x reason has_default default in
        if has_default then
          Default.default reason d
        else
          d)
  in
  let (parent, current) =
    match refinement with
    | Some t -> (None, t)
    | None ->
      (* use the same reason for the prop name and the lookup.
         given `var {foo} = ...`, `foo` is both. compare to `a.foo`
         where `foo` is the name and `a.foo` is the lookup. *)
      (Some current, destruct cx reason ~annot (Prop (x, has_default)) current)
  in
  let () =
    match parent with
    | None -> () (* TODO: get-def when object property is refined *)
    | Some t ->
      (*
       * We are within a destructuring pattern and a `get-def` on this identifier should
       * point at the "def" of the original property. To accompish this, we emit the type
       * of the parent pattern so that get-def can dive in to that type and extract the
       * location of the "def" of this property.
       *)
      Type_inference_hooks_js.dispatch_lval_hook cx x loc (Type_inference_hooks_js.Parent t)
  in
  { acc with parent; current; init; default }

let object_computed_property cx ~expr acc e =
  let { current; init; default; annot; _ } = acc in
  let (((loc, t), _) as e') = expr cx e in
  let reason = mk_reason (RProperty None) loc in
  let init =
    Base.Option.map init (fun init ->
        ( loc,
          Ast.Expression.(
            Member Member.{ _object = init; property = PropertyExpression e; comments = None }) ))
  in
  let refinement =
    Base.Option.bind init (fun init -> Refinement.get ~allow_optional:true cx init loc)
  in
  let (parent, current) =
    match refinement with
    | Some t -> (None, t)
    | None -> (Some current, destruct cx reason ~annot (Elem t) current)
  in
  let default = Base.Option.map default (Default.elem t reason) in
  ({ acc with parent; current; init; default }, e')

let object_rest_property cx acc xs loc =
  let { current; default; annot; _ } = acc in
  let reason = mk_reason RObjectPatternRestProp loc in
  let (parent, current) = (Some current, destruct cx reason ~annot (ObjRest xs) current) in
  let default = Base.Option.map default (Default.obj_rest xs reason) in
  { acc with parent; current; default }

let object_property
    cx ~expr ~has_default (acc : state) xs (key : (ALoc.t, ALoc.t) Ast.Pattern.Object.Property.key)
    : state * string list * (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Object.Property.key =
  let open Ast.Pattern.Object in
  match key with
  | Property.Identifier (loc, { Ast.Identifier.name = x; comments }) ->
    let acc = object_named_property ~has_default cx acc loc x comments in
    (acc, x :: xs, Property.Identifier ((loc, acc.current), { Ast.Identifier.name = x; comments }))
  | Property.Literal (loc, ({ Ast.Literal.value = Ast.Literal.String x; _ } as lit)) ->
    let acc = object_named_property ~has_default cx acc loc x None in
    (acc, x :: xs, Property.Literal (loc, lit))
  | Property.Computed (loc, { Ast.ComputedKey.expression; comments }) ->
    let (acc, e) = object_computed_property cx ~expr acc expression in
    (acc, xs, Property.Computed (loc, { Ast.ComputedKey.expression = e; comments }))
  | Property.Literal (loc, _) ->
    Flow_js.add_output
      cx
      Error_message.(EUnsupportedSyntax (loc, DestructuringObjectPropertyLiteralNonString));
    (acc, xs, Tast_utils.error_mapper#pattern_object_property_key key)

let identifier cx ~f acc name_loc name =
  let { parent; current; init; default; annot } = acc in
  let () =
    match parent with
    (* If there was a parent pattern, we already dispatched the hook if relevant. *)
    | Some _ -> ()
    (*
     * If there was no parent_pattern, we must not be within a destructuring
     * pattern and a `get-def` on this identifier should point at the
     * location where the binding is introduced.
     *)
    | None -> Type_inference_hooks_js.dispatch_lval_hook cx name name_loc Type_inference_hooks_js.Id
  in
  let current =
    mod_reason_of_t
      (update_desc_reason (function
          | RDefaultValue
          | RArrayPatternRestProp
          | RObjectPatternRestProp ->
            RIdentifier (OrdinaryName name)
          | desc -> desc))
      current
  in
  let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
  let current =
    (* If we are destructuring an annotation, the chain of constraints leading
     * to here will preserve the 0->1 constraint. The mk_typeof_annotation
     * helper will wrap the destructured type in an AnnotT, to ensure it is
     * resolved before it is used as an upper bound. The helper also enforces
     * the destructured type is 0->1 via BecomeT.
     *
     * The BecomeT part should not be necessary, but for now it is. Ideally an
     * annotation would recursively be 0->1, but it's possible for them to
     * contain inferred parts. For example, a class's instance type where one of
     * the fields is unannotated. *)
    if annot then
      Flow_js.mk_typeof_annotation cx reason current
    else
      current
  in
  let use_op =
    Op
      (AssignVar
         {
           var = Some reason;
           init =
             (match (default, init) with
             | (Some (Default.Expr t), _) -> reason_of_t t
             | (_, Some init) -> mk_expression_reason init
             | _ -> reason_of_t current);
         })
  in
  f ~use_op ~name_loc name default current

let rec pattern cx ~(expr : expr) ~(f : callback) acc (loc, p) =
  Ast.Pattern.
    ( (loc, acc.current),
      match p with
      | Array { Array.elements; annot; comments } ->
        let elements = array_elements cx ~expr ~f acc elements in
        let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
        Array { Array.elements; annot; comments }
      | Object { Object.properties; annot; comments } ->
        let properties = object_properties cx ~expr ~f acc properties in
        let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
        Object { Object.properties; annot; comments }
      | Identifier { Identifier.name = id; optional; annot } ->
        let (id_loc, { Ast.Identifier.name; comments }) = id in
        let annot = Tast_utils.unimplemented_mapper#type_annotation_hint annot in
        let id_ty = identifier cx ~f acc id_loc name in
        let id = ((id_loc, id_ty), { Ast.Identifier.name; comments }) in
        Identifier { Identifier.name = id; optional; annot }
      | Expression e ->
        Flow_js.add_output
          cx
          Error_message.(EUnsupportedSyntax (loc, DestructuringExpressionPattern));
        Expression (Tast_utils.error_mapper#expression e) )

and array_elements cx ~expr ~f acc =
  let open Ast.Pattern.Array in
  Base.List.mapi ~f:(fun i ->
    function
    | Hole loc -> Hole loc
    | Element (loc, { Element.argument = p; default = d }) ->
      let acc = array_element cx acc i loc in
      let (acc, d) = pattern_default cx ~expr acc d in
      let p = pattern cx ~expr ~f acc p in
      Element (loc, { Element.argument = p; default = d })
    | RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments }) ->
      let acc = array_rest_element cx acc i loc in
      let p = pattern cx ~expr ~f acc p in
      RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments }))

and object_properties =
  let open Ast.Pattern.Object in
  let prop cx ~expr ~f acc xs p =
    match p with
    | Property (loc, { Property.key; pattern = p; default = d; shorthand }) ->
      let has_default = d <> None in
      let (acc, xs, key) = object_property cx ~expr ~has_default acc xs key in
      let (acc, d) = pattern_default cx ~expr acc d in
      let p = pattern cx ~expr ~f acc p in
      (xs, Property (loc, { Property.key; pattern = p; default = d; shorthand }))
    | RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments }) ->
      let acc = object_rest_property cx acc xs loc in
      let p = pattern cx ~expr ~f acc p in
      (xs, RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments }))
  in
  let rec loop cx ~expr ~f acc xs rev_ps = function
    | [] -> List.rev rev_ps
    | p :: ps ->
      let (xs, p) = prop cx ~expr ~f acc xs p in
      loop cx ~expr ~f acc xs (p :: rev_ps) ps
  in
  (fun cx ~expr ~f acc ps -> loop cx ~expr ~f acc [] [] ps)

let type_of_pattern (_, p) =
  let open Ast.Pattern in
  match p with
  | Array { Array.annot; _ }
  | Object { Object.annot; _ }
  | Identifier { Identifier.annot; _ } ->
    annot
  | _ -> Ast.Type.Missing ALoc.none

(* instantiate pattern visitor for assignments *)
let assignment cx ~expr rhs_t init =
  let acc = empty ~init ~annot:false rhs_t in
  let f ~use_op ~name_loc name _default t =
    (* TODO destructuring+defaults unsupported in assignment expressions *)
    ignore Env.(set_var cx ~use_op name t name_loc);
    t
  in
  pattern cx ~expr ~f acc
