(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ac_id = {
  include_super: bool;
  include_this: bool;
  type_: Type.t;
  enclosing_class_t: Type.t option;
}

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment of {
      text: string;
      loc: ALoc.t;  (** Loc of the whole comment *)
    }  (** inside a comment *)
  | Ac_id of ac_id  (** identifier references *)
  | Ac_class_key of { enclosing_class_t: Type.t option }  (** class method name or property name *)
  | Ac_enum  (** identifier in enum declaration *)
  | Ac_import_specifier of {
      module_type: Type.t;
      used_keys: SSet.t;
      is_type: bool;
    }  (** Import named specifiers *)
  | Ac_key of {
      obj_type: Type.t;
      used_keys: SSet.t;
      spreads: (Loc.t * Type.t) list;
    }  (** object key *)
  | Ac_literal of { lit_type: Type.t }  (** inside a string literal *)
  | Ac_module  (** a module name *)
  | Ac_type of { allow_react_element_shorthand: bool }  (** type identifiers *)
  | Ac_type_binding  (** introduces a new type name. like Ac_binding, but for types *)
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
      bracket_syntax: ac_id option;
      (* loc of `.foo` or `[foo]` *)
      member_loc: Loc.t option;
      is_type_annotation: bool;
      is_super: bool;
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

let type_of_typeof =
  let open Flow_ast.Type.Typeof.Target in
  function
  | Unqualified ((_, t), _)
  | Qualified ((_, t), _) ->
    t

let compute_member_loc ~expr_loc ~obj_loc =
  let expr_loc = ALoc.to_loc_exn expr_loc in
  let obj_loc = ALoc.to_loc_exn obj_loc in
  Loc.btwn (Loc.end_loc obj_loc) expr_loc

let covers_target cursor loc = Reason.in_range cursor (ALoc.to_loc_exn loc)

let extract_word cursor_loc text =
  match Autocomplete_sigil.split_opt text with
  | None -> ("", Loc.none)
  | Some (before, after) ->
    let before =
      (* drop everything up to (and including) the last space *)
      Base.String.drop_prefix
        before
        (Base.Option.value
           ~default:0
           (Base.Option.map
              (Base.String.rfindi before ~f:(fun _ c -> Base.Char.is_whitespace c))
              ~f:(fun x -> x + 1)
           )
        )
    in
    let after =
      (* take up to the first space *)
      Base.String.prefix
        after
        (Base.Option.value
           ~default:(Base.String.length after)
           (Base.String.lfindi after ~f:(fun _ c -> Base.Char.is_whitespace c))
        )
    in
    let word_loc =
      Loc.
        {
          cursor_loc with
          start =
            { cursor_loc.start with column = cursor_loc.start.column - Base.String.length before };
          _end = { cursor_loc._end with column = cursor_loc._end.column + Base.String.length after };
        }
    in
    (before ^ after, word_loc)

exception Found of process_location_result

class process_request_searcher (from_trigger_character : bool) (cursor : Loc.t) =
  object (this)
    inherit Typed_ast_finder.type_parameter_mapper as super

    val mutable enclosing_classes : Type.t list = []

    val mutable allow_react_element_shorthand = false

    method covers_target loc = covers_target cursor loc

    method private get_enclosing_class = Base.List.hd enclosing_classes

    method default_ac_id type_ =
      {
        include_super = false;
        include_this = false;
        type_;
        enclosing_class_t = this#get_enclosing_class;
      }

    method find : 'a. ALoc.t -> string -> autocomplete_type -> 'a =
      fun ac_loc token autocomplete_type ->
        let autocomplete_type =
          match autocomplete_type with
          | Ac_id _
          | Ac_type _
            when from_trigger_character ->
            (* space is a trigger character, so this avoids completing immediately following
               a space ('.' is also a trigger, but it'd be an Acmem for member expressions) *)
            Ac_ignored
          | _ -> autocomplete_type
        in
        this#annot_with_tparams (fun ~tparams_rev ->
            raise (Found { tparams_rev; ac_loc; token; autocomplete_type })
        )

    method with_enclosing_class_t : 'a. Type.t -> 'a Lazy.t -> 'a =
      fun class_t f ->
        let previously_enclosing = enclosing_classes in
        enclosing_classes <- class_t :: enclosing_classes;
        let result = Lazy.force f in
        enclosing_classes <- previously_enclosing;
        result

    method! comment ((loc, Flow_ast.Comment.{ text; _ }) as c) =
      if this#covers_target loc then
        let (token, word_loc) = extract_word cursor text in
        this#find (ALoc.of_loc word_loc) token (Ac_comment { text; loc })
      else
        super#comment c

    method! t_identifier (((loc, type_), { Flow_ast.Identifier.name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc name (Ac_id (this#default_ac_id type_))
      else
        super#t_identifier ident

    method! jsx_element_name_identifier
        (((ac_loc, type_), { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if this#covers_target ac_loc then this#find ac_loc name (Ac_id (this#default_ac_id type_));
      ident

    (* Override to avoid providing autocomplete for object key identifier in type annotations *)
    method! object_property_type opt =
      let open Flow_ast.Type.Object.Property in
      match opt with
      | ( _,
          {
            key =
              Flow_ast.Expression.Object.Property.Identifier
                ((key_loc, _), { Flow_ast.Identifier.name; _ });
            _;
          }
        )
        when this#covers_target key_loc ->
        this#find key_loc name Ac_type_binding
      | _ -> super#object_property_type opt

    method member_with_loc expr_loc expr =
      let open Flow_ast.Expression.Member in
      let { _object = ((obj_loc, obj_type), _); property; comments = _ } = expr in
      let member_loc = Some (compute_member_loc ~expr_loc ~obj_loc) in
      let is_super = Flow_ast_utils.is_super_member_access expr in
      begin
        match property with
        | PropertyIdentifier ((prop_loc, _), { Flow_ast.Identifier.name; _ })
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            name
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = false;
                 bracket_syntax = None;
                 member_loc;
                 is_type_annotation = false;
                 is_super;
               }
            )
        | PropertyExpression
            ( (prop_loc, type_),
              Flow_ast.Expression.(
                ( StringLiteral { Flow_ast.StringLiteral.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) ))
            )
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = false;
                 bracket_syntax = Some (this#default_ac_id type_);
                 member_loc;
                 is_type_annotation = false;
                 is_super;
               }
            )
        | _ -> ()
      end;
      super#member expr

    method optional_member_with_loc expr_loc expr =
      let open Flow_ast.Expression.OptionalMember in
      let open Flow_ast.Expression.Member in
      let {
        member = { _object = ((obj_loc, obj_type), _) as obj; property; comments };
        optional;
        filtered_out;
      } =
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
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = true;
                 bracket_syntax = None;
                 member_loc;
                 is_type_annotation = false;
                 is_super = false;
               }
            )
        | PropertyExpression
            ( (prop_loc, type_),
              Flow_ast.Expression.(
                ( StringLiteral { Flow_ast.StringLiteral.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) ))
            )
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = true;
                 bracket_syntax = Some (this#default_ac_id type_);
                 member_loc;
                 is_type_annotation = false;
                 is_super = false;
               }
            )
        | _ -> ()
      end;
      (* the reason we don't simply call `super#optional_member` is because that would
       * call `this#member`, which would be redundant *)
      { member = { _object = this#expression obj; property; comments }; optional; filtered_out }

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
                      }
                    ))
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
                       is_type_annotation = false;
                       is_super = false;
                     }
                  )
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! pattern_identifier ?kind (((loc, _), Flow_ast.Identifier.{ name; _ }) as ident) =
      match kind with
      | None -> super#pattern_identifier ?kind ident
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
      let (_, Opening.{ name = component_name; targs = _; attributes; self_closing = _ }) = elt in

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
                  }
                ) ->
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
      | StringLiteral ((loc, lit_type), { Flow_ast.StringLiteral.raw; _ })
        when this#covers_target loc ->
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
      | StringLiteral (loc, { Flow_ast.StringLiteral.raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type = Type.(AnyT.at Untyped loc) })
      | _ -> super#pattern_object_property_key ?kind key

    method! class_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | Identifier ((loc, _), { Flow_ast.Identifier.name; _ })
      | StringLiteral ((loc, _), Flow_ast.StringLiteral.{ raw = name; _ })
        when this#covers_target loc ->
        this#find loc name (Ac_class_key { enclosing_class_t = this#get_enclosing_class })
      | PrivateName (loc, { Flow_ast.PrivateName.name; _ }) when this#covers_target loc ->
        this#find loc ("#" ^ name) (Ac_class_key { enclosing_class_t = this#get_enclosing_class })
      | _ -> super#class_key key

    method! object_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | StringLiteral ((loc, lit_type), Flow_ast.StringLiteral.{ raw; _ })
        when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | _ -> super#object_key key

    method! enum_member_identifier ((loc, Flow_ast.Identifier.{ name; _ }) as ident) =
      if this#covers_target loc then
        this#find loc name Ac_enum
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
             }
          )
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
             }
          )

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
             }
          )

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
             }
          )

    method! typeof_expression id =
      let open Flow_ast.Type.Typeof.Target in
      begin
        match id with
        | Unqualified ((loc, t), { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          this#find loc name (Ac_id (this#default_ac_id t))
        | Qualified
            ((expr_loc, _), { qualification; id = ((loc, _), Flow_ast.Identifier.{ name; _ }) })
          when this#covers_target loc ->
          let obj_type = type_of_typeof qualification in
          this#find
            loc
            name
            (Ac_member
               {
                 obj_type;
                 in_optional_chain = false;
                 bracket_syntax = None;
                 member_loc = Some (compute_member_loc ~expr_loc ~obj_loc:loc);
                 is_type_annotation = false;
                 is_super = false;
               }
            )
        | _ -> ()
      end;
      id

    method! generic_identifier_type id =
      let open Flow_ast.Type.Generic.Identifier in
      begin
        match id with
        | Unqualified ((loc, _), { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          this#find loc name (Ac_type { allow_react_element_shorthand })
        | Qualified (_, { qualification; id = ((loc, _), Flow_ast.Identifier.{ name; _ }) })
          when this#covers_target loc ->
          let qualification_type = type_of_qualification qualification in
          this#find loc name (Ac_qualified_type qualification_type)
        | _ -> ()
      end;
      id

    method! statement (annot, stmt) =
      let open Flow_ast.Statement in
      match stmt with
      | ClassDeclaration { Flow_ast.Class.id = Some ((_, t), _); _ } ->
        this#with_enclosing_class_t t (lazy (super#statement (annot, stmt)))
      | _ -> super#statement (annot, stmt)

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | ((loc, lit_type), StringLiteral Flow_ast.StringLiteral.{ raw; _ })
        when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | (((loc, _) as annot), Member member) ->
        (this#on_type_annot annot, Member (this#member_with_loc loc member))
      | (((loc, _) as annot), OptionalMember opt_member) ->
        (this#on_type_annot annot, OptionalMember (this#optional_member_with_loc loc opt_member))
      | (((_, obj_type) as annot), Object obj) ->
        (this#on_type_annot annot, Object (this#object_with_type obj_type obj))
      | ((_, class_t), Class _) -> this#with_enclosing_class_t class_t (lazy (super#expression expr))
      | _ -> super#expression expr

    method object_with_type obj_type obj =
      let open Flow_ast.Expression.Object in
      let { properties; comments } = obj in
      let (used_keys, spreads) =
        Base.List.fold properties ~init:(SSet.empty, []) ~f:(fun acc prop ->
            let (used_keys, spreads) = acc in
            match prop with
            | Property (_, (Property.Init { key; _ } | Property.Method { key; _ })) ->
              (match key with
              | Property.Identifier (_, { Flow_ast.Identifier.name; _ })
              | Property.StringLiteral (_, { Flow_ast.StringLiteral.value = name; _ }) ->
                (SSet.add name used_keys, spreads)
              | _ -> acc)
            | Property _ -> acc
            | SpreadProperty (_, { SpreadProperty.argument = ((spread_loc, spread_type), _); _ }) ->
              (used_keys, (ALoc.to_loc_exn spread_loc, spread_type) :: spreads)
        )
      in
      Base.Option.iter ~f:(fun syntax -> ignore (this#syntax syntax)) comments;
      Base.List.iter
        ~f:(fun prop ->
          ignore
            (this#object_property_or_spread_property_with_type ~used_keys ~spreads obj_type prop))
        properties;
      obj

    method object_property_or_spread_property_with_type ~used_keys ~spreads obj_type prop =
      let open Flow_ast.Expression.Object in
      match prop with
      | Property p -> Property (this#object_property_with_type ~used_keys ~spreads obj_type p)
      | SpreadProperty s -> SpreadProperty (this#spread_property s)

    method object_property_with_type ~used_keys ~spreads obj_type prop =
      let open Flow_ast.Expression.Object.Property in
      (match snd prop with
      | Init
          {
            key =
              ( Identifier ((loc, _), Flow_ast.Identifier.{ name = token; _ })
              | StringLiteral ((loc, _), Flow_ast.StringLiteral.{ raw = token; _ }) );
            _;
          }
        when this#covers_target loc ->
        this#find loc token (Ac_key { obj_type; used_keys; spreads })
      | _ -> ());
      this#object_property prop

    method! template_literal_element elem =
      match elem with
      | (loc, Flow_ast.Expression.TemplateLiteral.Element.{ value = { raw; _ }; _ })
        when this#covers_target loc ->
        (* TODO: we shouldn't have to fabricate a type here! *)
        this#find loc raw (Ac_literal { lit_type = Type.(AnyT.at Untyped loc) })
      | _ -> super#template_literal_element elem

    method! import_declaration decl_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default = _; comments = _ } = decl in
      let ((source_loc, module_type), Flow_ast.StringLiteral.{ raw = from; _ }) = source in
      if this#covers_target source_loc then
        this#find source_loc from Ac_module
      else if this#covers_target decl_loc then
        match specifiers with
        | Some (ImportNamedSpecifiers ns) ->
          let (found, used_keys) =
            Base.List.fold ns ~init:(None, SSet.empty) ~f:(fun (found, used_keys) specifier ->
                let { remote = ((loc, _), Flow_ast.Identifier.{ name; _ }); kind; _ } = specifier in
                let found =
                  if this#covers_target loc then
                    let is_type =
                      match Base.Option.value kind ~default:import_kind with
                      | ImportType -> true
                      | ImportTypeof -> false
                      | ImportValue -> false
                    in
                    Some (loc, name, is_type)
                  else
                    found
                in
                (found, SSet.add name used_keys)
            )
          in
          (match found with
          | None -> decl
          | Some (loc, token, is_type) ->
            this#find loc token (Ac_import_specifier { module_type; used_keys; is_type }))
        | Some (ImportNamespaceSpecifier _)
        | None ->
          super#import_declaration decl_loc decl
      else
        decl

    method indexed_access_type_with_loc loc ia =
      let open Flow_ast in
      let { Type.IndexedAccess._object; index; _ } = ia in
      let ((obj_loc, obj_type), _) = _object in
      (match index with
      | ( (index_loc, index_type),
          ( Type.StringLiteral { StringLiteral.raw = token; _ }
          | Type.Generic
              {
                Type.Generic.id =
                  Type.Generic.Identifier.Unqualified (_, { Identifier.name = token; _ });
                _;
              } )
        )
        when this#covers_target index_loc ->
        this#find
          index_loc
          token
          (Ac_member
             {
               obj_type;
               in_optional_chain = false;
               bracket_syntax = Some (this#default_ac_id index_type);
               member_loc = Some (compute_member_loc ~expr_loc:loc ~obj_loc);
               is_type_annotation = true;
               is_super = false;
             }
          )
      | _ -> ());
      super#indexed_access_type ia

    method optional_indexed_access_type_with_loc loc ia =
      let open Flow_ast in
      let {
        Type.OptionalIndexedAccess.indexed_access = { Type.IndexedAccess._object; index; comments };
        optional;
        _;
      } =
        ia
      in
      let ((obj_loc, obj_type), _) = _object in
      (match index with
      | ( (index_loc, index_type),
          ( Type.StringLiteral { StringLiteral.raw = token; _ }
          | Type.Generic
              {
                Type.Generic.id =
                  Type.Generic.Identifier.Unqualified (_, { Identifier.name = token; _ });
                _;
              } )
        )
        when this#covers_target index_loc ->
        this#find
          index_loc
          token
          (Ac_member
             {
               obj_type;
               in_optional_chain = true;
               bracket_syntax = Some (this#default_ac_id index_type);
               member_loc = Some (compute_member_loc ~expr_loc:loc ~obj_loc);
               is_type_annotation = true;
               is_super = false;
             }
          )
      | _ -> ());
      (* The reason we don't simply call `super#optional_indexed_access` is because that would
       * call `this#indexed_access`, which would be redundant. *)
      {
        Type.OptionalIndexedAccess.indexed_access =
          { Type.IndexedAccess._object = this#type_ _object; index = this#type_ index; comments };
        optional;
      }

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | ((loc, lit_type), StringLiteral { Flow_ast.StringLiteral.raw; _ })
        when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type })
      | (((loc, _) as annot), IndexedAccess ia) ->
        (this#on_type_annot annot, IndexedAccess (this#indexed_access_type_with_loc loc ia))
      | (((loc, _) as annot), OptionalIndexedAccess ia) ->
        ( this#on_type_annot annot,
          OptionalIndexedAccess (this#optional_indexed_access_type_with_loc loc ia)
        )
      | _ -> super#type_ t

    method! render_type t =
      let saved_allow_react_element_shorthand = allow_react_element_shorthand in
      allow_react_element_shorthand <- true;
      let t' = super#render_type t in
      allow_react_element_shorthand <- saved_allow_react_element_shorthand;
      t'

    method! generic_type gt =
      let open Flow_ast.Type.Generic in
      let { id; targs; comments } = gt in
      let id' = this#generic_identifier_type id in
      let saved_allow_react_element_shorthand = allow_react_element_shorthand in
      allow_react_element_shorthand <- true;
      let targs' = Base.Option.map ~f:this#type_args targs in
      allow_react_element_shorthand <- saved_allow_react_element_shorthand;
      let comments' = this#syntax_opt comments in
      { id = id'; targs = targs'; comments = comments' }

    (* Don't autocomplete type identifier bindings *)
    method! binding_type_identifier id =
      let ((loc, _), Flow_ast.Identifier.{ name; _ }) = id in
      if this#covers_target loc then
        (* don't offer suggestions for bindings, because this is where names are created,
           so existing names aren't useful. *)
        this#find loc name Ac_type_binding
      else
        id

    (* Do't autocomplete type param bindings *)
    method! type_param_identifier id =
      let (loc, Flow_ast.Identifier.{ name; _ }) = id in
      if this#covers_target loc then
        (* don't offer suggestions for bindings, because this is where names are created,
           so existing names aren't useful. *)
        this#find loc name Ac_type_binding
      else
        id
  end

let autocomplete_id ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_literal ~cursor _cx ac_loc = covers_target cursor ac_loc

let autocomplete_object_key ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_jsx ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let process_location ~trigger_character ~cursor ~typed_ast =
  try
    ignore ((new process_request_searcher (trigger_character <> None) cursor)#program typed_ast);
    None
  with
  | Found f -> Some f

let autocomplete_set_hooks ~cursor =
  Type_inference_hooks_js.set_id_hook (autocomplete_id ~cursor);
  Type_inference_hooks_js.set_literal_hook (autocomplete_literal ~cursor);
  Type_inference_hooks_js.set_obj_prop_decl_hook (autocomplete_object_key ~cursor);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx ~cursor)

let autocomplete_unset_hooks = Type_inference_hooks_js.reset_hooks
