(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ident_type =
  | NormalIdent
  | JSXIdent

type autocomplete_type =
  | Acid of {
      ac_loc: ALoc.t;
      id_type: ident_type;
      include_super: bool;
      include_this: bool;
    }
  | Acmem of string * ALoc.t * Type.t
  | Acjsx of string * SSet.t * ALoc.t * Type.t

let autocomplete_suffix = "AUTO332"

let suffix_len = String.length autocomplete_suffix

let is_autocomplete x =
  String.length x >= suffix_len
  &&
  let suffix = String.sub x (String.length x - suffix_len) suffix_len in
  suffix = autocomplete_suffix

let type_of_jsx_name =
  Flow_ast.JSX.(
    function
    | Identifier ((_, t), _)
    | NamespacedName (_, NamespacedName.{ name = ((_, t), _); _ })
    | MemberExpression (_, MemberExpression.{ property = ((_, t), _); _ }) ->
      t)

exception Found of autocomplete_type

class searcher (from_trigger_character : bool) =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method find x =
      match x with
      | Acid _ when from_trigger_character -> ()
      | _ -> raise (Found x)

    method! t_identifier (((ac_loc, _), { Flow_ast.Identifier.name; _ }) as ident) =
      if is_autocomplete name then
        this#find
          (Acid { ac_loc; id_type = NormalIdent; include_super = false; include_this = false });
      ident

    method! jsx_identifier (((ac_loc, _), { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if is_autocomplete name then
        this#find
          (Acid { ac_loc; id_type = JSXIdent; include_super = false; include_this = false });
      ident

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object = ((_, obj_t), _); property } = expr in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), { Flow_ast.Identifier.name; _ })
          when is_autocomplete name ->
          this#find (Acmem (name, prop_loc, obj_t))
        | _ -> ()
      end;
      super#member expr

    method! pattern ?kind pat =
      let open Flow_ast.Pattern in
      begin
        match pat with
        | ((_, obj_t), Object { Object.properties; _ }) ->
          List.iter
            (function
              | Object.(
                  Property
                    ( _,
                      {
                        Property.key =
                          Property.Identifier ((prop_loc, _), Flow_ast.Identifier.{ name; _ });
                        _;
                      } ))
                when is_autocomplete name ->
                this#find (Acmem (name, prop_loc, obj_t))
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind x =
      match kind with
      | None -> super#t_pattern_identifier ?kind x
      | Some _ -> x

    method! jsx_opening_element elt =
      let open Flow_ast.JSX in
      let (_, Opening.{ name = component_name; attributes; _ }) = elt in
      let (used_attr_names, found) =
        List.fold_left
          (fun (used_attr_names, found) -> function
            | Opening.Attribute
                ( _,
                  {
                    Attribute.name =
                      Attribute.Identifier ((loc, _), { Identifier.name = attribute_name });
                    _;
                  } ) ->
              let found' =
                match found with
                | Some _ -> found
                | None when is_autocomplete attribute_name ->
                  Some (attribute_name, loc, type_of_jsx_name component_name)
                | None -> None
              in
              (SSet.add attribute_name used_attr_names, found')
            | _ -> (used_attr_names, found))
          (SSet.empty, None)
          attributes
      in
      Option.iter
        ~f:(fun (attribute_name, loc, component_t) ->
          this#find (Acjsx (attribute_name, used_attr_names, loc, component_t)))
        found;
      super#jsx_opening_element elt

    (* we don't currently autocomplete object keys *)
    method! object_key_identifier x = x

    (* prevent shorthand properties from triggering autocomplete on object keys *)
    method! object_property prop =
      let open Flow_ast.Expression.Object.Property in
      match prop with
      | (_, Init { shorthand = true; _ }) -> prop
      | _ -> super#object_property prop

    method! class_body x =
      try super#class_body x
      with Found (Acid id) ->
        raise (Found (Acid { id with include_super = true; include_this = true }))

    method! function_expression x =
      try super#function_expression x
      with Found (Acid id) -> raise (Found (Acid { id with include_this = true }))

    method! function_declaration x =
      try super#function_declaration x
      with Found (Acid id) -> raise (Found (Acid { id with include_this = true }))
  end

let autocomplete_id from_trigger_character _cx ac_name _ac_loc =
  is_autocomplete ac_name && not from_trigger_character

let autocomplete_object_key from_trigger_character _cx ac_name _ac_loc =
  is_autocomplete ac_name && not from_trigger_character

let autocomplete_member _cx ac_name _ac_loc _this_t = is_autocomplete ac_name

let autocomplete_jsx _cx ac_name _ac_loc _class_t = is_autocomplete ac_name

let process_location ~trigger_character ~typed_ast =
  try
    ignore ((new searcher (trigger_character <> None))#program typed_ast);
    None
  with Found res -> Some res

let autocomplete_set_hooks ~trigger_character =
  Type_inference_hooks_js.set_id_hook (autocomplete_id (trigger_character <> None));
  Type_inference_hooks_js.set_obj_prop_decl_hook
    (autocomplete_object_key (trigger_character <> None));
  Type_inference_hooks_js.set_member_hook autocomplete_member;
  Type_inference_hooks_js.set_jsx_hook autocomplete_jsx

let autocomplete_unset_hooks () = Type_inference_hooks_js.reset_hooks ()
