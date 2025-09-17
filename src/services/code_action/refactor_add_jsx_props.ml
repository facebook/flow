(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let id_snippet ~snippets_enabled idx name =
  let name =
    if snippets_enabled && idx = 0 then
      Utils_js.spf "${0:%s}" name
    else
      name
  in
  (Loc.none, Ast.Expression.Identifier (Loc.none, { Ast.Identifier.name; comments = None }))

let mk_attribute ~snippets_enabled idx name =
  let open Ast.JSX in
  Opening.Attribute
    ( Loc.none,
      {
        Attribute.name = Attribute.Identifier (Loc.none, { Identifier.name; comments = None });
        value =
          Some
            (Attribute.ExpressionContainer
               ( Loc.none,
                 {
                   ExpressionContainer.expression =
                     ExpressionContainer.Expression (id_snippet ~snippets_enabled idx name);
                   comments = None;
                 }
               )
            );
      }
    )

let get_obj_prop_names ~include_optional cx reason t =
  let open Type in
  let ts = Flow_js.possible_concrete_types_for_operators_checking cx reason t in
  match ts with
  | DefT (_, ObjT { props_tmap; _ }) :: _ ->
    Some
      (Context.fold_props
         cx
         props_tmap
         (fun name prop acc ->
           match (name, Property.read_t prop) with
           | (Reason.OrdinaryName n, Some (OptionalT _)) ->
             (* TODO do we need a more robust check for optionality? *)
             if include_optional then
               SSet.add n acc
             else
               acc
           | (Reason.OrdinaryName n, _) -> SSet.add n acc)
         SSet.empty
      )
  | _ -> None

let get_required_attribute_names cx loc t =
  let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementConfig")) (ALoc.of_loc loc)) in
  let use_op = Type.Op (Type.TypeApplication { type_ = reason }) in
  let id = Type.Eval.generate_id () in
  let conf =
    Flow_js.mk_type_destructor
      cx
      use_op
      reason
      t
      (Type.ReactElementConfigType { from_userland = false })
      id
  in
  get_obj_prop_names ~include_optional:false cx reason conf

let name_of_jsx_id (_, { Ast.JSX.Identifier.name; _ }) = name

let name_of_attribute attribute =
  let open Ast.JSX in
  match attribute with
  | Opening.Attribute (_, { Attribute.name = Attribute.Identifier id; _ }) -> name_of_jsx_id id
  | Opening.Attribute
      (_, { Attribute.name = Attribute.NamespacedName (_, { NamespacedName.namespace; name }); _ })
    ->
    name_of_jsx_id namespace ^ "." ^ name_of_jsx_id name
  | Opening.SpreadAttribute _ -> "_"

let get_existing_attributes_names cx ~tast attributes children =
  let open Ast.JSX in
  let init_set =
    if List.is_empty (snd children) then
      SSet.empty
    else
      SSet.singleton "children"
  in
  List.fold_left
    (fun acc attr ->
      match attr with
      | Opening.Attribute (_, { Attribute.name = Attribute.Identifier id; _ }) ->
        SSet.add (name_of_jsx_id id) acc
      | Opening.Attribute (_, { Attribute.name = Attribute.NamespacedName _; _ }) ->
        acc (* non-react jsx *)
      | Opening.SpreadAttribute (_, { SpreadAttribute.argument = (expr_loc, _); _ }) ->
        (match Typed_ast_finder.find_exact_match_annotation tast (ALoc.of_loc expr_loc) with
        | Some t ->
          (match get_obj_prop_names ~include_optional:true cx (TypeUtil.reason_of_t t) t with
          | Some names -> SSet.union names acc
          | None -> acc)
        | None -> acc))
    init_set
    attributes

let attr_compare (_, x) (_, y) =
  (* Ensure regular attributes come first *)
  let open Ast.JSX in
  match (x, y) with
  | (Opening.SpreadAttribute _, Opening.Attribute _) -> 1
  | (Opening.Attribute _, Opening.SpreadAttribute _) -> -1
  | _ -> compare (name_of_attribute x) (name_of_attribute y)

let loc_of_attr = function
  | Ast.JSX.Opening.SpreadAttribute (loc, _) -> loc
  | Ast.JSX.Opening.Attribute (loc, _) -> loc

let concat_and_sort_attrs ~init_loc ~existing_attrs ~new_attrs =
  let existing_attrs = Base.List.map ~f:(fun a -> (`Existing, a)) existing_attrs in
  let new_attrs = Base.List.map ~f:(fun a -> (`New, a)) new_attrs in
  let attrs =
    if Base.List.is_sorted ~compare:attr_compare existing_attrs then
      Base.List.sort ~compare:attr_compare (existing_attrs @ new_attrs)
    else
      existing_attrs @ new_attrs
  in
  let (_, rev_attrs) =
    Base.List.fold_left attrs ~init:(init_loc, []) ~f:(fun (prev_loc, acc) (k, attr) ->
        match k with
        | `Existing -> (loc_of_attr attr, acc)
        | `New -> (prev_loc, (Loc.end_loc prev_loc, attr) :: acc)
    )
  in
  List.rev rev_attrs

exception Found of (Loc.t * (Loc.t, Loc.t) Ast.JSX.Opening.attribute) list

class mapper cx ~snippets_enabled ~tast target_loc =
  object (_this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! jsx_element loc elt =
      let open Ast.JSX in
      let elt = super#jsx_element loc elt in
      let (_annot', { Opening.name; attributes; _ }) = elt.opening_element in
      match name with
      | Identifier (id_loc, _) when Loc.contains id_loc target_loc -> begin
        let attributes_from_conf_opt =
          match Typed_ast_finder.find_exact_match_annotation tast (ALoc.of_loc id_loc) with
          | Some t -> get_required_attribute_names cx loc t
          | None -> None
        in
        match attributes_from_conf_opt with
        | None -> elt
        | Some attributes_from_conf ->
          let existing_attributes_names =
            get_existing_attributes_names cx ~tast attributes elt.children
          in
          let new_attrs =
            SSet.diff attributes_from_conf existing_attributes_names
            |> SSet.elements
            |> Base.List.sort ~compare:String.compare
            |> Base.List.mapi ~f:(mk_attribute ~snippets_enabled)
          in
          if not (List.is_empty new_attrs) then
            raise
              (Found
                 (concat_and_sort_attrs
                    ~init_loc:(Loc.end_loc id_loc)
                    ~existing_attrs:attributes
                    ~new_attrs
                 )
              );
          elt
      end
      | _ -> elt
  end

let fill_props cx ~snippets_enabled ~ast ~tast target_loc =
  let mapper = new mapper cx ~snippets_enabled ~tast target_loc in
  match mapper#program ast with
  | exception Found xs -> Some xs
  | _ -> None
