(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment  (** inside a comment *)
  | Ac_id of {
      token: string;
      include_super: bool;
      include_this: bool;
    }  (** identifier references *)
  | Ac_key  (** object key, not supported yet *)
  | Ac_literal  (** inside a literal like a string or regex *)
  | Ac_module  (** a module name *)
  | Ac_type of { token: string }  (** type identifiers *)
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
    }  (** member expressions *)
  | Ac_jsx_element of { token: string }  (** JSX element name *)
  | Ac_jsx_attribute of {
      attribute_name: string;
      used_attr_names: SSet.t;
      component_t: Type.t;
      has_value: bool;
    }  (** JSX attributes *)
  | Ac_jsx_text  (** JSX text child *)

let type_of_jsx_name =
  let open Flow_ast.JSX in
  function
  | Identifier ((_, t), _)
  | NamespacedName (_, NamespacedName.{ name = ((_, t), _); _ })
  | MemberExpression (_, MemberExpression.{ property = ((_, t), _); _ }) ->
    t

let type_of_qualification =
  let open Flow_ast.Type.Generic.Identifier in
  function
  | Unqualified ((_, t), _)
  | Qualified (_, { id = ((_, t), _); _ }) ->
    t

let covers_target cursor loc = Reason.in_range cursor (ALoc.to_loc_exn loc)

exception Found of (ALoc.t * string) list * ALoc.t * autocomplete_type

class process_request_searcher (from_trigger_character : bool) (cursor : Loc.t) =
  object (this)
    inherit Typed_ast_utils.type_parameter_mapper as super

    method covers_target loc = covers_target cursor loc

    method find : 'a. ALoc.t -> autocomplete_type -> 'a =
      fun loc x ->
        let x =
          match x with
          | Ac_id _ when from_trigger_character ->
            (* space is a trigger character, so this avoids completing immediately following
               a space ('.' is also a trigger, but it'd be an Acmem for member expressions) *)
            Ac_ignored
          | _ -> x
        in
        this#annot_with_tparams (fun tparams -> raise (Found (tparams, loc, x)))

    method! comment ((loc, _) as c) =
      if this#covers_target loc then
        (* don't autocomplete in comments *)
        this#find loc Ac_comment
      else
        super#comment c

    method! t_identifier (((loc, _), { Flow_ast.Identifier.name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc (Ac_id { token = name; include_super = false; include_this = false })
      else
        super#t_identifier ident

    method! jsx_identifier (((ac_loc, _), { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if this#covers_target ac_loc then
        this#find ac_loc (Ac_id { token = name; include_super = false; include_this = false });
      ident

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object = ((_, obj_type), _); property; comments = _ } = expr in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), _) when this#covers_target prop_loc ->
          this#find prop_loc (Ac_member { obj_type; in_optional_chain = false })
        | _ -> ()
      end;
      super#member expr

    method! optional_member expr =
      let open Flow_ast.Expression.OptionalMember in
      let open Flow_ast.Expression.Member in
      let { member = { _object = ((_, obj_type), _) as obj; property; comments }; optional } =
        expr
      in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), _) when this#covers_target prop_loc ->
          this#find prop_loc (Ac_member { obj_type; in_optional_chain = true })
        | _ -> ()
      end;
      (* the reason we don't simply call `super#optional_member` is because that would
       * call `this#member`, which would be redundant *)
      { member = { _object = this#expression obj; property; comments }; optional }

    method! pattern ?kind pat =
      let open Flow_ast.Pattern in
      begin
        match pat with
        | ((_, obj_type), Object { Object.properties; _ }) ->
          List.iter
            (function
              | Object.(Property (_, { Property.key = Property.Identifier ((prop_loc, _), _); _ }))
                when this#covers_target prop_loc ->
                this#find prop_loc (Ac_member { obj_type; in_optional_chain = false })
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind (((loc, _), _) as ident) =
      match kind with
      | None -> super#t_pattern_identifier ?kind ident
      | Some _ ->
        (* binding identifiers *)
        if this#covers_target loc then
          (* don't offer suggestions for bindings, because this is where names are created,
             so existing names aren't useful. *)
          this#find loc Ac_binding
        else
          ident

    method! jsx_opening_element elt =
      let open Flow_ast.JSX in
      let (_, Opening.{ name = component_name; attributes; self_closing = _ }) = elt in

      (match component_name with
      | Identifier ((loc, _), { Identifier.name; comments = _ }) ->
        if this#covers_target loc then this#find loc (Ac_jsx_element { token = name })
      | NamespacedName _
      | MemberExpression _ ->
        (* TODO: these are rare so we haven't implemented them yet *)
        ());

      let (used_attr_names, found) =
        List.fold_left
          (fun (used_attr_names, found) -> function
            | Opening.Attribute
                ( _,
                  {
                    Attribute.name =
                      Attribute.Identifier
                        ((loc, _), { Identifier.name = attribute_name; comments = _ });
                    value;
                  } ) ->
              let found' =
                match found with
                | Some _ -> found
                | None when this#covers_target loc ->
                  let has_value = Base.Option.is_some value in
                  Some (attribute_name, loc, type_of_jsx_name component_name, has_value)
                | None -> None
              in
              (SSet.add attribute_name used_attr_names, found')
            | _ -> (used_attr_names, found))
          (SSet.empty, None)
          attributes
      in
      Base.Option.iter
        ~f:(fun (attribute_name, loc, component_t, has_value) ->
          this#find
            loc
            (Ac_jsx_attribute { attribute_name; used_attr_names; component_t; has_value }))
        found;
      super#jsx_opening_element elt

    method! jsx_closing_element elem =
      let open Flow_ast.JSX in
      let open Flow_ast.JSX.Closing in
      let (_, { name }) = elem in
      match name with
      | Identifier ((loc, _), { Identifier.name; comments = _ }) ->
        if this#covers_target loc then this#find loc (Ac_jsx_element { token = name });
        elem
      | NamespacedName _
      | MemberExpression _ ->
        (* TODO: these are rare so we haven't implemented them yet *)
        super#jsx_closing_element elem

    method! jsx_attribute_value value =
      let open Flow_ast.JSX.Attribute in
      match value with
      | Literal ((loc, _), _) when this#covers_target loc -> this#find loc Ac_literal
      | _ -> super#jsx_attribute_value value

    method! jsx_child child =
      match child with
      | (loc, Flow_ast.JSX.Text _) when this#covers_target loc -> this#find loc Ac_jsx_text
      | _ -> super#jsx_child child

    method! pattern_object_property_key ?kind key =
      let open Flow_ast.Pattern.Object.Property in
      match key with
      | Literal (loc, _) when this#covers_target loc -> this#find loc Ac_literal
      | _ -> super#pattern_object_property_key ?kind key

    method! object_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | Literal ((loc, _), _) when this#covers_target loc -> this#find loc Ac_literal
      | _ -> super#object_key key

    (* we don't currently autocomplete object keys *)
    method! object_key_identifier (((loc, _), _) as ident) =
      if this#covers_target loc then
        this#find loc Ac_key
      else
        ident

    method! enum_identifier ((loc, _) as ident) =
      if this#covers_target loc then
        this#find loc Ac_key
      else
        ident

    method! class_body x =
      try super#class_body x
      with Found (tparams, loc, Ac_id id) ->
        raise (Found (tparams, loc, Ac_id { id with include_super = true; include_this = true }))

    method! function_expression x =
      try super#function_expression x
      with Found (tparams, loc, Ac_id id) ->
        raise (Found (tparams, loc, Ac_id { id with include_this = true }))

    method! function_declaration x =
      try super#function_declaration x
      with Found (tparams, loc, Ac_id id) ->
        raise (Found (tparams, loc, Ac_id { id with include_this = true }))

    method! generic_identifier_type id =
      let open Flow_ast.Type.Generic.Identifier in
      begin
        match id with
        | Unqualified ((loc, _), { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          this#find loc (Ac_type { token = name })
        | Qualified (_, { qualification; id = ((loc, _), _) }) when this#covers_target loc ->
          let qualification_type = type_of_qualification qualification in
          this#find loc (Ac_qualified_type qualification_type)
        | _ -> ()
      end;
      id

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | ((loc, _), Literal _) when this#covers_target loc -> this#find loc Ac_literal
      | _ -> super#expression expr

    method! template_literal_element elem =
      match elem with
      | (loc, _) when this#covers_target loc -> this#find loc Ac_literal
      | _ -> super#template_literal_element elem

    method! import_declaration decl_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { import_kind = _; source; specifiers = _; default = _; comments = _ } = decl in
      match source with
      | (loc, _) when this#covers_target loc -> this#find loc Ac_module
      | _ -> super#import_declaration decl_loc decl

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | ((loc, _), (StringLiteral _ | NumberLiteral _)) when this#covers_target loc ->
        this#find loc Ac_literal
      | _ -> super#type_ t
  end

let autocomplete_id from_trigger_character ~cursor _cx _ac_name ac_loc =
  covers_target cursor ac_loc && not from_trigger_character

let autocomplete_object_key from_trigger_character ~cursor _cx _ac_name ac_loc =
  covers_target cursor ac_loc && not from_trigger_character

let autocomplete_member ~cursor _cx _ac_name ac_loc _this_t = covers_target cursor ac_loc

let autocomplete_jsx ~cursor _cx _ac_name ac_loc _class_t = covers_target cursor ac_loc

let process_location ~trigger_character ~cursor ~typed_ast =
  try
    ignore ((new process_request_searcher (trigger_character <> None) cursor)#program typed_ast);
    None
  with Found (tparams, loc, res) -> Some (tparams, loc, res)

let autocomplete_set_hooks ~trigger_character ~cursor =
  Type_inference_hooks_js.set_id_hook (autocomplete_id (trigger_character <> None) ~cursor);
  Type_inference_hooks_js.set_obj_prop_decl_hook
    (autocomplete_object_key (trigger_character <> None) ~cursor);
  Type_inference_hooks_js.set_member_hook (autocomplete_member ~cursor);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx ~cursor)

let autocomplete_unset_hooks () = Type_inference_hooks_js.reset_hooks ()
