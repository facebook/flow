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
  (* ignore extraneous requests the IDE sends *)
  | Acignored
  (* binding identifiers introduce new names *)
  | Acbinding
  (* inside a comment *)
  | Accomment
  (* identifier references *)
  | Acid of {
      id_type: ident_type;
      include_super: bool;
      include_this: bool;
    }
  (* object key, not supported yet *)
  | Ackey
  (* inside a literal like a string or regex *)
  | Acliteral
  (* a module name *)
  | Acmodule
  (* type identifiers *)
  | Actype
  (* qualified type identifiers *)
  | Acqualifiedtype of Type.t
  (* member expressions *)
  | Acmem of Type.t
  (* JSX attributes *)
  | Acjsx of string * SSet.t * Type.t
  (* JSX text child *)
  | Acjsxtext

let autocomplete_suffix = "AUTO332"

let suffix_len = String.length autocomplete_suffix

let is_autocomplete x =
  String.length x >= suffix_len
  &&
  let suffix = String.sub x (String.length x - suffix_len) suffix_len in
  suffix = autocomplete_suffix

let identifier_is_autocomplete ((loc, _), { Flow_ast.Identifier.name; _ }) =
  if is_autocomplete name then
    Some loc
  else
    None

let type_of_jsx_name =
  Flow_ast.JSX.(
    function
    | Identifier ((_, t), _)
    | NamespacedName (_, NamespacedName.{ name = ((_, t), _); _ })
    | MemberExpression (_, MemberExpression.{ property = ((_, t), _); _ }) ->
      t)

let type_of_qualification =
  Flow_ast.Type.Generic.Identifier.(
    function
    | Unqualified ((_, t), _)
    | Qualified (_, { id = ((_, t), _); _ }) ->
      t)

exception Found of (ALoc.t * string) list * ALoc.t * autocomplete_type

class process_request_searcher (from_trigger_character : bool) (cursor : Loc.t) =
  object (this)
    inherit Typed_ast_utils.type_parameter_mapper as super

    method covers_target loc = Reason.in_range cursor (ALoc.to_loc_exn loc)

    method find : 'a. ALoc.t -> autocomplete_type -> 'a =
      fun loc x ->
        let x =
          match x with
          | Acid _ when from_trigger_character ->
            (* space is a trigger character, so this avoids completing immediately following
               a space ('.' is also a trigger, but it'd be an Acmem for member expressions) *)
            Acignored
          | _ -> x
        in
        this#annot_with_tparams (fun tparams -> raise (Found (tparams, loc, x)))

    method! comment ((loc, _) as c) =
      if this#covers_target loc then
        (* don't autocomplete in comments *)
        this#find loc Accomment
      else
        super#comment c

    method! t_identifier ident =
      match identifier_is_autocomplete ident with
      | Some loc ->
        this#find loc (Acid { id_type = NormalIdent; include_super = false; include_this = false })
      | None -> ident

    method! jsx_identifier (((ac_loc, _), { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if is_autocomplete name then
        this#find ac_loc (Acid { id_type = JSXIdent; include_super = false; include_this = false });
      ident

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object = ((_, obj_t), _); property } = expr in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), { Flow_ast.Identifier.name; _ })
          when is_autocomplete name ->
          this#find prop_loc (Acmem obj_t)
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
                this#find prop_loc (Acmem obj_t)
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind ident =
      match kind with
      | None -> super#t_pattern_identifier ?kind ident
      | Some _ ->
        (* binding identifiers *)
        (match identifier_is_autocomplete ident with
        | Some loc ->
          (* don't offer suggestions for bindings, because this is where names are created,
             so existing names aren't useful. *)
          this#find loc Acbinding
        | None -> ident)

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
          this#find loc (Acjsx (attribute_name, used_attr_names, component_t)))
        found;
      super#jsx_opening_element elt

    method! jsx_attribute_value value =
      let open Flow_ast.JSX.Attribute in
      match value with
      | Literal ((loc, _), _) when this#covers_target loc -> this#find loc Acliteral
      | _ -> super#jsx_attribute_value value

    method! jsx_child child =
      match child with
      | (loc, Flow_ast.JSX.Text _) when this#covers_target loc -> this#find loc Acjsxtext
      | _ -> super#jsx_child child

    method! pattern_object_property_key ?kind key =
      let open Flow_ast.Pattern.Object.Property in
      match key with
      | Literal (loc, _) when this#covers_target loc -> this#find loc Acliteral
      | _ -> super#pattern_object_property_key ?kind key

    method! object_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | Literal ((loc, _), _) when this#covers_target loc -> this#find loc Acliteral
      | _ -> super#object_key key

    (* we don't currently autocomplete object keys *)
    method! object_key_identifier x =
      match identifier_is_autocomplete x with
      | Some loc -> this#find loc Ackey
      | None -> x

    method! class_body x =
      try super#class_body x
      with Found (tparams, loc, Acid id) ->
        raise (Found (tparams, loc, Acid { id with include_super = true; include_this = true }))

    method! function_expression x =
      try super#function_expression x
      with Found (tparams, loc, Acid id) ->
        raise (Found (tparams, loc, Acid { id with include_this = true }))

    method! function_declaration x =
      try super#function_declaration x
      with Found (tparams, loc, Acid id) ->
        raise (Found (tparams, loc, Acid { id with include_this = true }))

    method! generic_identifier_type id =
      let open Flow_ast.Type.Generic.Identifier in
      begin
        match id with
        | Unqualified ((loc, _), { Flow_ast.Identifier.name; _ }) when is_autocomplete name ->
          this#find loc Actype
        | Qualified (_, { qualification; id = ((loc, _), { Flow_ast.Identifier.name; _ }) })
          when is_autocomplete name ->
          let qualification_type = type_of_qualification qualification in
          this#find loc (Acqualifiedtype qualification_type)
        | _ -> ()
      end;
      id

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | ((loc, _), Literal _) when this#covers_target loc -> this#find loc Acliteral
      | _ -> super#expression expr

    method! template_literal_element elem =
      match elem with
      | (loc, _) when this#covers_target loc -> this#find loc Acliteral
      | _ -> super#template_literal_element elem

    method! import_declaration decl_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { importKind = _; source; specifiers = _; default = _ } = decl in
      match source with
      | (loc, _) when this#covers_target loc -> this#find loc Acmodule
      | _ -> super#import_declaration decl_loc decl

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | ((loc, _), (StringLiteral _ | NumberLiteral _)) when this#covers_target loc ->
        this#find loc Acliteral
      | _ -> super#type_ t
  end

let autocomplete_id from_trigger_character _cx ac_name _ac_loc =
  is_autocomplete ac_name && not from_trigger_character

let autocomplete_object_key from_trigger_character _cx ac_name _ac_loc =
  is_autocomplete ac_name && not from_trigger_character

let autocomplete_member _cx ac_name _ac_loc _this_t = is_autocomplete ac_name

let autocomplete_jsx _cx ac_name _ac_loc _class_t = is_autocomplete ac_name

let process_location ~trigger_character ~cursor ~typed_ast =
  try
    ignore ((new process_request_searcher (trigger_character <> None) cursor)#program typed_ast);
    None
  with Found (tparams, loc, res) -> Some (tparams, loc, res)

let autocomplete_set_hooks ~trigger_character =
  Type_inference_hooks_js.set_id_hook (autocomplete_id (trigger_character <> None));
  Type_inference_hooks_js.set_obj_prop_decl_hook
    (autocomplete_object_key (trigger_character <> None));
  Type_inference_hooks_js.set_member_hook autocomplete_member;
  Type_inference_hooks_js.set_jsx_hook autocomplete_jsx

let autocomplete_unset_hooks () = Type_inference_hooks_js.reset_hooks ()
