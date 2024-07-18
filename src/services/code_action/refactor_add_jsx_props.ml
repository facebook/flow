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
      (Context.fold_real_props
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
           | (Reason.OrdinaryName n, _) -> SSet.add n acc
           | (_, _) -> acc)
         SSet.empty
      )
  | _ -> None

let get_required_attribute_names cx loc t =
  let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementConfig")) (ALoc.of_loc loc)) in
  let use_op = Type.Op (Type.TypeApplication { type_ = reason }) in
  let id = Type.Eval.generate_id () in
  let conf = Flow_js.mk_type_destructor cx use_op reason t Type.ReactElementConfigType id in
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

let get_existing_attributes_names cx ~tast attributes =
  let open Ast.JSX in
  List.fold_left
    (fun acc attr ->
      match attr with
      | Opening.Attribute (_, { Attribute.name = Attribute.Identifier id; _ }) ->
        SSet.add (name_of_jsx_id id) acc
      | Opening.Attribute (_, { Attribute.name = Attribute.NamespacedName _; _ }) ->
        acc (* non-react jsx *)
      | Opening.SpreadAttribute (_, { SpreadAttribute.argument = (expr_loc, _); _ }) ->
        (match Typed_ast_finder.find_exact_match_annotation cx tast (ALoc.of_loc expr_loc) with
        | Some t ->
          (match get_obj_prop_names ~include_optional:true cx (TypeUtil.reason_of_t t) t with
          | Some names -> SSet.union names acc
          | None -> acc)
        | None -> acc))
    SSet.empty
    attributes

let attr_compare x y =
  (* Ensure regular attributes come first *)
  let open Ast.JSX in
  match (x, y) with
  | (Opening.SpreadAttribute _, Opening.Attribute _) -> 1
  | (Opening.Attribute _, Opening.SpreadAttribute _) -> -1
  | _ -> compare (name_of_attribute x) (name_of_attribute y)

let concat_and_sort_attrs ~exising_attrs ~new_attrs =
  if Base.List.is_sorted ~compare:attr_compare exising_attrs then
    Base.List.sort ~compare:attr_compare (exising_attrs @ new_attrs)
  else
    exising_attrs @ new_attrs

class mapper cx ~snippets_enabled ~tast target_loc =
  object (_this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    val mutable found = false

    method was_found = found

    method! jsx_element loc elt =
      let open Ast.JSX in
      let elt = super#jsx_element loc elt in
      let (annot', { Opening.name; targs; self_closing; attributes }) = elt.opening_element in
      match name with
      | Identifier (id_loc, _) when (not found) && Loc.contains id_loc target_loc -> begin
        let attributes_from_conf_opt =
          match Typed_ast_finder.find_exact_match_annotation cx tast (ALoc.of_loc id_loc) with
          | Some t -> get_required_attribute_names cx loc t
          | None -> None
        in
        match attributes_from_conf_opt with
        | None -> elt
        | Some attributes_from_conf ->
          let existing_attributes_names = get_existing_attributes_names cx ~tast attributes in
          let new_attrs =
            SSet.diff attributes_from_conf existing_attributes_names
            |> SSet.elements
            |> Base.List.sort ~compare:String.compare
            |> Base.List.mapi ~f:(mk_attribute ~snippets_enabled)
          in
          if List.is_empty new_attrs then
            elt
          else begin
            found <- true;
            let attributes = concat_and_sort_attrs ~exising_attrs:attributes ~new_attrs in
            let opening_element = (annot', { Opening.name; targs; self_closing; attributes }) in
            { elt with Ast.JSX.opening_element }
          end
      end
      | _ -> elt
  end

let fill_props cx ~snippets_enabled ~ast ~tast target_loc =
  let mapper = new mapper cx ~snippets_enabled ~tast target_loc in
  let tast' = mapper#program ast in
  Base.Option.some_if mapper#was_found tast'
