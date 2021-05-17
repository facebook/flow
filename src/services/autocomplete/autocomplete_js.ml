(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ac_id = {
  include_super: bool;
  include_this: bool;
  type_: Type.t;
}

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment  (** inside a comment *)
  | Ac_id of ac_id  (** identifier references *)
  | Ac_key  (** object key, not supported yet *)
  | Ac_literal of { lit_type: Type.t }  (** inside a literal like a string or regex *)
  | Ac_module  (** a module name *)
  | Ac_type  (** type identifiers *)
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
      bracket_syntax: ac_id option;
      member_loc: Loc.t option; (* loc of `.foo` or `[foo]` *)
    }  (** member expressions *)
  | Ac_jsx_element of { type_: Type.t }  (** JSX element name *)
  | Ac_jsx_attribute of {
      attribute_name: string;
      used_attr_names: SSet.t;
      component_t: Type.t;
      has_value: bool;
    }  (** JSX attributes *)
  | Ac_jsx_text  (** JSX text child *)

type process_location_result = {
  tparams_rev: Type.typeparam list;
  ac_loc: ALoc.t;
  token: string;
  autocomplete_type: autocomplete_type;
}

let default_ac_id type_ = { include_super = false; include_this = false; type_ }

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

let compute_member_loc ~expr_loc ~obj_loc =
  let expr_loc = ALoc.to_loc_exn expr_loc in
  let obj_loc = ALoc.to_loc_exn obj_loc in
  Loc.btwn (Loc.end_loc obj_loc) expr_loc

let covers_target cursor loc = Reason.in_range cursor (ALoc.to_loc_exn loc)

exception Found of process_location_result

class process_request_searcher (from_trigger_character : bool) (cursor : Loc.t) =
  object (this)
    inherit Typed_ast_utils.type_parameter_mapper as super

    method covers_target loc = covers_target cursor loc

    method find : 'a. ALoc.t -> string -> autocomplete_type -> 'a =
      fun ac_loc token autocomplete_type ->
        let autocomplete_type =
          match autocomplete_type with
          | Ac_id _ when from_trigger_character ->
            (* space is a trigger character, so this avoids completing immediately following
               a space ('.' is also a trigger, but it'd be an Acmem for member expressions) *)
            Ac_ignored
          | _ -> autocomplete_type
        in
        this#annot_with_tparams (fun ~tparams_rev ->
            raise (Found { tparams_rev; ac_loc; token; autocomplete_type }))

    method! comment ((loc, Flow_ast.Comment.{ text; _ }) as c) =
      if this#covers_target loc then
        (* don't autocomplete in comments *)
        this#find loc text Ac_comment
      else
        super#comment c

    method! t_identifier (((loc, type_), { Flow_ast.Identifier.name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc name (Ac_id (default_ac_id type_))
      else
        super#t_identifier ident

    method! jsx_element_name_identifier
        (((ac_loc, type_), { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if this#covers_target ac_loc then this#find ac_loc name (Ac_id (default_ac_id type_));
      ident

    method member_with_loc expr_loc expr =
      let open Flow_ast.Expression.Member in
      let { _object = ((obj_loc, obj_type), _); property; comments = _ } = expr in
      let member_loc = Some (compute_member_loc ~expr_loc ~obj_loc) in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), { Flow_ast.Identifier.name; _ })
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            name
            (Ac_member { obj_type; in_optional_chain = false; bracket_syntax = None; member_loc })
        | PropertyExpression
            ( (prop_loc, type_),
              Flow_ast.Expression.(
                ( Literal { Flow_ast.Literal.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) )) )
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = false;
                 bracket_syntax = Some (default_ac_id type_);
                 member_loc;
               })
        | _ -> ()
      end;
      super#member expr

    method optional_member_with_loc expr_loc expr =
      let open Flow_ast.Expression.OptionalMember in
      let open Flow_ast.Expression.Member in
      let { member = { _object = ((obj_loc, obj_type), _) as obj; property; comments }; optional } =
        expr
      in
      let member_loc = Some (compute_member_loc ~expr_loc ~obj_loc) in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), { Flow_ast.Identifier.name; _ })
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            name
            (Ac_member { obj_type; in_optional_chain = true; bracket_syntax = None; member_loc })
        | PropertyExpression
            ( (prop_loc, type_),
              Flow_ast.Expression.(
                ( Literal { Flow_ast.Literal.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) )) )
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = true;
                 bracket_syntax = Some (default_ac_id type_);
                 member_loc;
               })
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
              | Object.(
                  Property
                    ( _,
                      {
                        Property.key =
                          Property.Identifier ((prop_loc, _), Flow_ast.Identifier.{ name; _ });
                        _;
                      } ))
                when this#covers_target prop_loc ->
                this#find
                  prop_loc
                  name
                  (Ac_member
                     {
                       obj_type;
                       in_optional_chain = false;
                       bracket_syntax = None;
                       member_loc = None;
                     })
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind (((loc, _), Flow_ast.Identifier.{ name; _ }) as ident) =
      match kind with
      | None -> super#t_pattern_identifier ?kind ident
      | Some _ ->
        (* binding identifiers *)
        if this#covers_target loc then
          (* don't offer suggestions for bindings, because this is where names are created,
             so existing names aren't useful. *)
          this#find loc name Ac_binding
        else
          ident

    method! jsx_opening_element elt =
      let open Flow_ast.JSX in
      let (_, Opening.{ name = component_name; attributes; self_closing = _ }) = elt in

      (match component_name with
      | Identifier ((loc, type_), { Identifier.name; comments = _ }) ->
        if this#covers_target loc then this#find loc name (Ac_jsx_element { type_ })
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
            attribute_name
            (Ac_jsx_attribute { attribute_name; used_attr_names; component_t; has_value }))
        found;
      super#jsx_opening_element elt

    method! jsx_closing_element elem =
      let open Flow_ast.JSX in
      let open Flow_ast.JSX.Closing in
      let (_, { name }) = elem in
      match name with
      | Identifier ((loc, type_), { Identifier.name; comments = _ }) ->
        if this#covers_target loc then this#find loc name (Ac_jsx_element { type_ });
        elem
      | NamespacedName _
      | MemberExpression _ ->
        (* TODO: these are rare so we haven't implemented them yet *)
        super#jsx_closing_element elem

    method! jsx_attribute_value value =
      let open Flow_ast.JSX.Attribute in
      match value with
      | Literal ((loc, lit_type), Flow_ast.Literal.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | _ -> super#jsx_attribute_value value

    method! jsx_child child =
      match child with
      | (loc, Flow_ast.JSX.(Text Text.{ raw; _ })) when this#covers_target loc ->
        this#find loc raw Ac_jsx_text
      | _ -> super#jsx_child child

    method! pattern_object_property_key ?kind key =
      let open Flow_ast.Pattern.Object.Property in
      match key with
      (* TODO: we shouldn't have to fabricate a type here! *)
      | Literal (loc, Flow_ast.Literal.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type = Type.(AnyT.at Untyped loc) })
      | _ -> super#pattern_object_property_key ?kind key

    method! object_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | Literal ((loc, lit_type), Flow_ast.Literal.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | _ -> super#object_key key

    (* we don't currently autocomplete object keys *)
    method! object_key_identifier (((loc, _), Flow_ast.Identifier.{ name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc name Ac_key
      else
        ident

    method! enum_identifier ((loc, Flow_ast.Identifier.{ name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc name Ac_key
      else
        ident

    method! class_body x =
      try super#class_body x with
      | Found ({ autocomplete_type = Ac_id id; _ } as f) ->
        raise
          (Found
             {
               f with
               autocomplete_type = Ac_id { id with include_super = true; include_this = true };
             })
      | Found ({ autocomplete_type = Ac_member ({ bracket_syntax = Some id; _ } as member); _ } as f)
        ->
        raise
          (Found
             {
               f with
               autocomplete_type =
                 Ac_member
                   {
                     member with
                     bracket_syntax = Some { id with include_super = true; include_this = true };
                   };
             })

    method! function_expression x =
      try super#function_expression x with
      | Found ({ autocomplete_type = Ac_id id; _ } as f) ->
        raise (Found { f with autocomplete_type = Ac_id { id with include_this = true } })
      | Found ({ autocomplete_type = Ac_member ({ bracket_syntax = Some id; _ } as member); _ } as f)
        ->
        raise
          (Found
             {
               f with
               autocomplete_type =
                 Ac_member { member with bracket_syntax = Some { id with include_this = true } };
             })

    method! function_declaration x =
      try super#function_declaration x with
      | Found ({ autocomplete_type = Ac_id id; _ } as f) ->
        raise (Found { f with autocomplete_type = Ac_id { id with include_this = true } })
      | Found ({ autocomplete_type = Ac_member ({ bracket_syntax = Some id; _ } as member); _ } as f)
        ->
        raise
          (Found
             {
               f with
               autocomplete_type =
                 Ac_member { member with bracket_syntax = Some { id with include_this = true } };
             })

    method! generic_identifier_type id =
      let open Flow_ast.Type.Generic.Identifier in
      begin
        match id with
        | Unqualified ((loc, _), { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          this#find loc name Ac_type
        | Qualified (_, { qualification; id = ((loc, _), Flow_ast.Identifier.{ name; _ }) })
          when this#covers_target loc ->
          let qualification_type = type_of_qualification qualification in
          this#find loc name (Ac_qualified_type qualification_type)
        | _ -> ()
      end;
      id

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | ((loc, lit_type), Literal Flow_ast.Literal.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | (((loc, _) as annot), Member member) ->
        (this#on_type_annot annot, Member (this#member_with_loc loc member))
      | (((loc, _) as annot), OptionalMember opt_member) ->
        (this#on_type_annot annot, OptionalMember (this#optional_member_with_loc loc opt_member))
      | _ -> super#expression expr

    method! template_literal_element elem =
      match elem with
      | (loc, Flow_ast.Expression.TemplateLiteral.Element.{ value = { raw; _ }; _ })
        when this#covers_target loc ->
        (* TODO: we shouldn't have to fabricate a type here! *)
        this#find loc raw (Ac_literal { lit_type = Type.(AnyT.at Untyped loc) })
      | _ -> super#template_literal_element elem

    method! import_declaration decl_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { import_kind = _; source; specifiers = _; default = _; comments = _ } = decl in
      match source with
      | (loc, Flow_ast.StringLiteral.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw Ac_module
      | _ -> super#import_declaration decl_loc decl

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | ( (loc, lit_type),
          ( StringLiteral Flow_ast.StringLiteral.{ raw; _ }
          | NumberLiteral Flow_ast.NumberLiteral.{ raw; _ } ) )
        when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | _ -> super#type_ t
  end

let autocomplete_id ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_literal ~cursor _cx ac_loc = covers_target cursor ac_loc

let autocomplete_object_key ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_member ~cursor _cx _ac_name ac_loc _this_t = covers_target cursor ac_loc

let autocomplete_jsx ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let process_location ~trigger_character ~cursor ~typed_ast =
  try
    ignore ((new process_request_searcher (trigger_character <> None) cursor)#program typed_ast);
    None
  with Found f -> Some f

let autocomplete_set_hooks ~cursor =
  Type_inference_hooks_js.set_id_hook (autocomplete_id ~cursor);
  Type_inference_hooks_js.set_literal_hook (autocomplete_literal ~cursor);
  Type_inference_hooks_js.set_obj_prop_decl_hook (autocomplete_object_key ~cursor);
  Type_inference_hooks_js.set_member_hook (autocomplete_member ~cursor);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx ~cursor)

let autocomplete_unset_hooks = Type_inference_hooks_js.reset_hooks
