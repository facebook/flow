(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type bracket_syntax = {
  include_super: bool;
  include_this: bool;
  type_: Type.t;
}

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment of {
      text: string;
      loc: ALoc.t;  (** Loc of the whole comment *)
    }  (** inside a comment *)
  | Ac_id of {
      include_super: bool;
      include_this: bool;
      type_: Type.t;
      enclosing_class_t: Type.t option;
    }  (** identifier references *)
  | Ac_class_key of { enclosing_class_t: Type.t option }  (** class method name or property name *)
  | Ac_enum  (** identifier in enum declaration *)
  | Ac_import_specifier of {
      module_type_opt: Type.moduletype option;
      used_keys: SSet.t;
      is_type: bool;
    }  (** Import named specifiers *)
  | Ac_key of {
      obj_type: Type.t;
      used_keys: SSet.t;
      spreads: (Loc.t * Type.t) list;
    }  (** object key *)
  | Ac_literal of { lit_type: Type.t option }  (** inside a string literal *)
  | Ac_module  (** a module name *)
  | Ac_type  (** type identifiers *)
  | Ac_type_binding  (** introduces a new type name. like Ac_binding, but for types *)
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
      bracket_syntax: bracket_syntax option;
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
  tparams_rev: string list;
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

let loc_of_qualification =
  let open Flow_ast.Type.Generic.Identifier in
  function
  | Unqualified (loc, _)
  | Qualified (_, { id = (loc, _); _ }) ->
    loc

let loc_of_typeof =
  let open Flow_ast.Type.Typeof.Target in
  function
  | Unqualified (loc, _)
  | Qualified (loc, _) ->
    loc

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

exception Internal_exn of string

module Statement = Fix_statement.Statement_

module Inference = struct
  let type_of_component_name_of_jsx_element cx loc expr =
    let open Ast.JSX in
    match Statement.expression cx (loc, Ast.Expression.JSXElement expr) with
    | (_, Ast.Expression.JSXElement { opening_element = (_, Opening.{ name; _ }); _ }) ->
      type_of_jsx_name name
    | _ -> raise (Internal_exn "typed AST structure mismatch")

  let type_of_expression cx expr =
    let ((_, t), _) = Statement.expression cx expr in
    t

  let type_of_class_id cx loc cls =
    let open Ast.Statement in
    match Statement.statement cx (loc, ClassDeclaration cls) with
    | (_, ClassDeclaration { Flow_ast.Class.id = Some ((_, t), _); _ }) -> t
    | _ -> raise (Internal_exn "typed AST structure mismatch")

  let type_of_identifier cx loc id = Statement.identifier cx id loc

  let type_of_match_member_pattern cx loc mem =
    Match_pattern.type_of_member_pattern
      cx
      ~on_identifier:Statement.identifier
      ~on_expression:Statement.expression
      (loc, mem)
end

class process_request_searcher cx ~from_trigger_character ~cursor =
  object (this)
    inherit
      [ALoc.t, ALoc.t, ALoc.t, ALoc.t, string] Typed_ast_finder.type_parameter_mapper_generic as super

    method on_type_annot x = x

    method on_loc_annot x = x

    method private make_typeparam tparam =
      let open Flow_ast.Type.TypeParam in
      let (_, { name = (_, { Flow_ast.Identifier.name; _ }); _ }) = tparam in
      name

    method private make_class_this _cls = "this"

    method private make_declare_class_this _decl = "this"

    val mutable enclosing_classes : Type.t Lazy.t list = []

    method private covers_target loc = covers_target cursor loc

    method private get_enclosing_class = Base.List.hd enclosing_classes

    method private check_closest_enclosing_statement =
      let last_stmt = ref None in
      Base.List.iter
        ~f:(function
          | Typed_ast_finder.EnclosingStatement stmt -> last_stmt := Some stmt
          | _ -> ())
        enclosing_node_stack;
      Base.Option.iter !last_stmt ~f:(fun ((loc, _) as s) ->
          (* Check the closest statement *)
          let s' = Statement.statement cx s in
          let ast =
            ( loc,
              {
                Ast.Program.statements = [s];
                interpreter = None;
                comments = None;
                all_comments = [];
              }
            )
          in
          let tast =
            ( loc,
              {
                Ast.Program.statements = [s'];
                interpreter = None;
                comments = None;
                all_comments = [];
              }
            )
          in
          let metadata = Context.metadata cx in
          (* We need to also run post-inference checks since some errors are
           * raised there. *)
          Merge_js.post_merge_checks cx ast tast metadata
      )

    method private type_from_enclosing_node loc =
      let node = this#enclosing_node in
      let typed_node = Typed_ast_finder.infer_node cx node in
      match Typed_ast_finder.find_type_annot_in_node loc typed_node with
      | None -> raise (Internal_exn "enclosing loc missing")
      | Some t -> t

    method private default_ac_id type_ =
      Ac_id
        {
          include_super = false;
          include_this = false;
          type_;
          enclosing_class_t = Base.Option.map ~f:Lazy.force this#get_enclosing_class;
        }

    method private default_bracket_syntax type_ : bracket_syntax =
      { include_super = false; include_this = false; type_ }

    method private find : 'a. ALoc.t -> string -> autocomplete_type -> 'a =
      fun ac_loc token autocomplete_type ->
        let autocomplete_type =
          match autocomplete_type with
          | Ac_id _
          | Ac_type
            when from_trigger_character ->
            (* space is a trigger character, so this avoids completing immediately following
               a space ('.' is also a trigger, but it'd be an Acmem for member expressions) *)
            Ac_ignored
          | _ -> autocomplete_type
        in
        this#annot_with_tparams (fun ~tparams_rev ->
            raise (Found { tparams_rev; ac_loc; token; autocomplete_type })
        )

    method private with_enclosing_class_t : 'a. Type.t Lazy.t -> 'a Lazy.t -> 'a =
      fun class_t f ->
        let previously_enclosing = enclosing_classes in
        enclosing_classes <- class_t :: enclosing_classes;
        let result = Lazy.force f in
        enclosing_classes <- previously_enclosing;
        result

    method! comment ((loc, Flow_ast.Comment.{ text; _ }) as c) =
      if this#covers_target loc then (
        let (token, word_loc) = extract_word cursor text in
        this#check_closest_enclosing_statement;
        this#find (ALoc.of_loc word_loc) token (Ac_comment { text; loc })
      ) else
        super#comment c

    method! t_identifier ((loc, { Flow_ast.Identifier.name; _ }) as ident) =
      if this#covers_target loc then begin
        let t = this#type_from_enclosing_node loc in
        this#find loc name (this#default_ac_id t)
      end else
        super#t_identifier ident

    method! jsx_element_name_identifier ((loc, { Flow_ast.JSX.Identifier.name; _ }) as ident) =
      if this#covers_target loc then begin
        let type_ = this#type_from_enclosing_node loc in
        this#find loc name (this#default_ac_id type_)
      end;
      ident

    (* Override to avoid providing autocomplete for object key identifier in type annotations *)
    method! object_property_type opt =
      let open Flow_ast.Type.Object.Property in
      match opt with
      | ( _,
          {
            key =
              Flow_ast.Expression.Object.Property.Identifier
                (key_loc, { Flow_ast.Identifier.name; _ });
            _;
          }
        )
        when this#covers_target key_loc ->
        this#find key_loc name Ac_type_binding
      | _ -> super#object_property_type opt

    method private member_with_annot expr_loc expr =
      let open Flow_ast.Expression.Member in
      let { _object = (obj_loc, _) as _object; property; comments = _ } = expr in
      let member_loc = Some (compute_member_loc ~expr_loc ~obj_loc) in
      let is_super = Flow_ast_utils.is_super_member_access expr in
      begin
        match property with
        | PropertyIdentifier (prop_loc, { Flow_ast.Identifier.name; _ })
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            name
            (Ac_member
               {
                 obj_type = Inference.type_of_expression cx _object;
                 in_optional_chain = false;
                 bracket_syntax = None;
                 member_loc;
                 is_type_annotation = false;
                 is_super;
               }
            )
        | PropertyExpression
            ( prop_loc,
              Flow_ast.Expression.(
                ( StringLiteral { Flow_ast.StringLiteral.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) ))
            )
          when this#covers_target prop_loc ->
          let type_ = this#type_from_enclosing_node prop_loc in
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type = Inference.type_of_expression cx _object;
                 in_optional_chain = false;
                 bracket_syntax = Some (this#default_bracket_syntax type_);
                 member_loc;
                 is_type_annotation = false;
                 is_super;
               }
            )
        | _ -> ()
      end;
      super#member expr_loc expr

    method private optional_member_with_loc expr_loc expr =
      let open Flow_ast.Expression.OptionalMember in
      let open Flow_ast.Expression.Member in
      let { member = { _object = (obj_loc, _) as obj; property; comments }; optional; filtered_out }
          =
        expr
      in
      let member_loc = Some (compute_member_loc ~expr_loc ~obj_loc) in
      begin
        match property with
        | PropertyIdentifier (prop_loc, { Flow_ast.Identifier.name; _ })
          when this#covers_target prop_loc ->
          this#find
            prop_loc
            name
            (Ac_member
               {
                 obj_type = Inference.type_of_expression cx obj;
                 in_optional_chain = true;
                 bracket_syntax = None;
                 member_loc;
                 is_type_annotation = false;
                 is_super = false;
               }
            )
        | PropertyExpression
            ( prop_loc,
              Flow_ast.Expression.(
                ( StringLiteral { Flow_ast.StringLiteral.raw = token; _ }
                | Identifier (_, { Flow_ast.Identifier.name = token; _ }) ))
            )
          when this#covers_target prop_loc ->
          let type_ = this#type_from_enclosing_node prop_loc in
          this#find
            prop_loc
            token
            (Ac_member
               {
                 obj_type = Inference.type_of_expression cx obj;
                 in_optional_chain = true;
                 bracket_syntax = Some (this#default_bracket_syntax type_);
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
        | (obj_loc, Object { Object.properties; _ }) ->
          List.iter
            (function
              | Object.(
                  Property
                    ( _,
                      {
                        Property.key =
                          Property.Identifier (prop_loc, Flow_ast.Identifier.{ name; _ });
                        _;
                      }
                    ))
                when this#covers_target prop_loc ->
                let obj_type = this#type_from_enclosing_node obj_loc in
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

    method! pattern_identifier ?kind ((loc, Flow_ast.Identifier.{ name; _ }) as ident) =
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

    method! jsx_element expr_loc expr =
      let open Flow_ast.JSX in
      let { opening_element; closing_element; children; comments } = expr in
      ignore @@ this#visit_jsx_opening_element expr_loc expr opening_element;
      Base.Option.iter closing_element ~f:(fun closing_element ->
          ignore @@ this#visit_jsx_closing_element expr_loc expr closing_element
      );
      ignore @@ this#jsx_children children;
      ignore @@ this#syntax_opt comments;
      expr

    method private visit_jsx_opening_element expr_loc expr elt =
      let open Flow_ast.JSX in
      let (_, Opening.{ name = component_name; targs = _; attributes; self_closing = _ }) = elt in
      let component_name_t =
        lazy (Inference.type_of_component_name_of_jsx_element cx expr_loc expr)
      in
      (match component_name with
      | Identifier (loc, { Identifier.name; comments = _ }) ->
        if this#covers_target loc then
          let type_ = Lazy.force component_name_t in
          this#find loc name (Ac_jsx_element { type_ })
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
                      Attribute.Identifier (loc, { Identifier.name = attribute_name; comments = _ });
                    value;
                  }
                ) ->
              let found' =
                match found with
                | Some _ -> found
                | None when this#covers_target loc ->
                  let has_value = Base.Option.is_some value in
                  Some (attribute_name, loc, Lazy.force component_name_t, has_value)
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

    method private visit_jsx_closing_element expr_loc expr elem =
      let open Flow_ast.JSX in
      let open Flow_ast.JSX.Closing in
      let (_, { name }) = elem in
      match name with
      | Identifier (loc, { Identifier.name; comments = _ }) ->
        if this#covers_target loc then begin
          let type_ = Inference.type_of_component_name_of_jsx_element cx expr_loc expr in
          this#find loc name (Ac_jsx_element { type_ })
        end;
        elem
      | NamespacedName _
      | MemberExpression _ ->
        (* TODO: these are rare so we haven't implemented them yet *)
        super#jsx_closing_element elem

    method! jsx_attribute_value value =
      let open Flow_ast.JSX.Attribute in
      match value with
      | StringLiteral (loc, { Flow_ast.StringLiteral.raw; _ }) when this#covers_target loc ->
        let lit_type = this#type_from_enclosing_node loc in
        this#find loc raw (Ac_literal { lit_type = Some lit_type })
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
        this#find loc raw (Ac_literal { lit_type = Some Type.(AnyT.at Untyped loc) })
      | _ -> super#pattern_object_property_key ?kind key

    method! class_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | Identifier (loc, { Flow_ast.Identifier.name; _ })
      | StringLiteral (loc, Flow_ast.StringLiteral.{ raw = name; _ })
        when this#covers_target loc ->
        this#find
          loc
          name
          (Ac_class_key
             { enclosing_class_t = Base.Option.map ~f:Lazy.force this#get_enclosing_class }
          )
      | PrivateName (loc, { Flow_ast.PrivateName.name; _ }) when this#covers_target loc ->
        this#find
          loc
          ("#" ^ name)
          (Ac_class_key
             { enclosing_class_t = Base.Option.map ~f:Lazy.force this#get_enclosing_class }
          )
      | _ -> super#class_key key

    method! object_key key =
      let open Flow_ast.Expression.Object.Property in
      match key with
      | StringLiteral (loc, Flow_ast.StringLiteral.{ raw; _ }) when this#covers_target loc ->
        let lit_type = this#type_from_enclosing_node loc in
        this#find loc raw (Ac_literal { lit_type = Some lit_type })
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
        | Unqualified (loc, { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          let t = this#type_from_enclosing_node loc in
          this#find loc name (this#default_ac_id t)
        | Qualified (expr_loc, { qualification; id = (loc, Flow_ast.Identifier.{ name; _ }) })
          when this#covers_target loc ->
          let obj_type = this#type_from_enclosing_node (loc_of_typeof qualification) in
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
        | Unqualified (loc, { Flow_ast.Identifier.name; _ }) when this#covers_target loc ->
          this#find loc name Ac_type
        | Qualified (_, { qualification; id = (loc, Flow_ast.Identifier.{ name; _ }) })
          when this#covers_target loc ->
          let qualification_type =
            this#type_from_enclosing_node (loc_of_qualification qualification)
          in
          this#find loc name (Ac_qualified_type qualification_type)
        | _ -> ()
      end;
      id

    method! statement (loc, stmt) =
      let open Flow_ast.Statement in
      match stmt with
      | ClassDeclaration ({ Flow_ast.Class.id = Some _; _ } as cls) ->
        let t = lazy (Inference.type_of_class_id cx loc cls) in
        this#with_enclosing_class_t t (lazy (super#statement (loc, stmt)))
      | _ -> super#statement (loc, stmt)

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | (loc, StringLiteral Flow_ast.StringLiteral.{ raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type = Some (Inference.type_of_expression cx expr) })
      | (loc, Member member) -> (this#on_type_annot loc, Member (this#member_with_annot loc member))
      | (loc, OptionalMember opt_member) ->
        (this#on_type_annot loc, OptionalMember (this#optional_member_with_loc loc opt_member))
      | (loc, Object obj) -> (this#on_type_annot loc, Object (this#object_with_annot loc obj))
      | (_, Class _) ->
        let class_t = lazy (Inference.type_of_expression cx expr) in
        this#with_enclosing_class_t class_t (lazy (super#expression expr))
      | _ -> super#expression expr

    method private object_spread properties =
      let open Flow_ast.Expression.Object in
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

    method private object_with_annot loc obj =
      let open Flow_ast.Expression.Object in
      let { properties; comments } = obj in
      let ac_key =
        lazy
          (match Statement.expression cx (loc, Flow_ast.Expression.Object obj) with
          | ((_, obj_type), Flow_ast.Expression.Object { properties; _ }) ->
            let (used_keys, spreads) = this#object_spread properties in
            Ac_key { used_keys; spreads; obj_type }
          | ((loc, _), _) ->
            (* impossible *)
            Ac_key { used_keys = SSet.empty; spreads = []; obj_type = Type.(AnyT.at Untyped loc) })
      in
      Base.Option.iter ~f:(fun syntax -> ignore (this#syntax syntax)) comments;
      Base.List.iter
        ~f:(fun prop -> ignore (this#object_property_or_spread_property_with_type ac_key prop))
        properties;
      obj

    method private object_property_or_spread_property_with_type ac_key prop =
      let open Flow_ast.Expression.Object in
      match prop with
      | Property p -> Property (this#object_property_with_type ac_key p)
      | SpreadProperty s -> SpreadProperty (this#spread_property s)

    method private object_property_with_type ac_key prop =
      let open Flow_ast.Expression.Object.Property in
      (match snd prop with
      | Init
          {
            key =
              ( Identifier (loc, Flow_ast.Identifier.{ name = token; _ })
              | StringLiteral (loc, Flow_ast.StringLiteral.{ raw = token; _ }) );
            _;
          }
        when this#covers_target loc ->
        this#find loc token (Lazy.force ac_key)
      | _ -> ());
      this#object_property prop

    method! template_literal_element elem =
      match elem with
      | (loc, Flow_ast.Expression.TemplateLiteral.Element.{ value = { raw; _ }; _ })
        when this#covers_target loc ->
        (* TODO: we shouldn't have to fabricate a type here! *)
        this#find loc raw (Ac_literal { lit_type = Some Type.(AnyT.at Untyped loc) })
      | _ -> super#template_literal_element elem

    method! import_declaration decl_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default = _; comments = _ } = decl in
      let (source_loc, Flow_ast.StringLiteral.{ raw = from; _ }) = source in
      if this#covers_target source_loc then
        this#find source_loc from Ac_module
      else if this#covers_target decl_loc then
        match specifiers with
        | Some (ImportNamedSpecifiers ns) ->
          let (found, used_keys) =
            Base.List.fold ns ~init:(None, SSet.empty) ~f:(fun (found, used_keys) specifier ->
                let { remote = (loc, Flow_ast.Identifier.{ name; _ }); kind; _ } = specifier in
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
            let module_type_opt =
              match Statement.statement cx (decl_loc, Ast.Statement.ImportDeclaration decl) with
              | ( _,
                  Ast.Statement.ImportDeclaration
                    { source = ((loc, _), { Ast.StringLiteral.value; _ }); _ }
                ) ->
                Flow_js_utils.ImportExportUtils.get_module_type_or_any
                  cx
                  ~import_kind_for_untyped_import_validation:None
                  (loc, Flow_import_specifier.userland value)
                |> Base.Result.ok
              | _ -> (* impossible *) None
            in
            this#find loc token (Ac_import_specifier { module_type_opt; used_keys; is_type }))
        | Some (ImportNamespaceSpecifier _)
        | None ->
          super#import_declaration decl_loc decl
      else
        decl

    method private indexed_access_type_with_annot expr_loc ia =
      let open Flow_ast in
      let { Type.IndexedAccess._object; index; _ } = ia in
      let (obj_loc, _) = _object in
      (match index with
      | ( index_loc,
          ( Type.StringLiteral { StringLiteral.raw = token; _ }
          | Type.Generic
              {
                Type.Generic.id =
                  Type.Generic.Identifier.Unqualified (_, { Identifier.name = token; _ });
                _;
              } )
        )
        when this#covers_target index_loc ->
        let index_type = this#type_from_enclosing_node index_loc in
        this#find
          index_loc
          token
          (Ac_member
             {
               obj_type = this#type_from_enclosing_node obj_loc;
               in_optional_chain = false;
               bracket_syntax = Some (this#default_bracket_syntax index_type);
               member_loc = Some (compute_member_loc ~expr_loc ~obj_loc);
               is_type_annotation = true;
               is_super = false;
             }
          )
      | _ -> ());
      super#indexed_access_type ia

    method private optional_indexed_access_type_with_annot expr_loc ia =
      let open Flow_ast in
      let {
        Type.OptionalIndexedAccess.indexed_access = { Type.IndexedAccess._object; index; comments };
        optional;
        _;
      } =
        ia
      in
      let (obj_loc, _) = _object in
      (match index with
      | ( index_loc,
          ( Type.StringLiteral { StringLiteral.raw = token; _ }
          | Type.Generic
              {
                Type.Generic.id =
                  Type.Generic.Identifier.Unqualified (_, { Identifier.name = token; _ });
                _;
              } )
        )
        when this#covers_target index_loc ->
        let index_type = this#type_from_enclosing_node index_loc in
        this#find
          index_loc
          token
          (Ac_member
             {
               obj_type = this#type_from_enclosing_node obj_loc;
               in_optional_chain = true;
               bracket_syntax = Some (this#default_bracket_syntax index_type);
               member_loc = Some (compute_member_loc ~expr_loc ~obj_loc);
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
      | (loc, StringLiteral { Flow_ast.StringLiteral.raw; _ }) when this#covers_target loc ->
        this#find loc raw (Ac_literal { lit_type = None })
      | (loc, IndexedAccess ia) ->
        (this#on_type_annot loc, IndexedAccess (this#indexed_access_type_with_annot loc ia))
      | (loc, OptionalIndexedAccess ia) ->
        ( this#on_type_annot loc,
          OptionalIndexedAccess (this#optional_indexed_access_type_with_annot loc ia)
        )
      | _ -> super#type_ t

    method! generic_type gt =
      let open Flow_ast.Type.Generic in
      let { id; targs; comments } = gt in
      let id' = this#generic_identifier_type id in
      let targs' = Base.Option.map ~f:this#type_args targs in
      let comments' = this#syntax_opt comments in
      { id = id'; targs = targs'; comments = comments' }

    (* Don't autocomplete type identifier bindings *)
    method! binding_type_identifier id =
      let (loc, Flow_ast.Identifier.{ name; _ }) = id in
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

    (* Don't autocomplete match object pattern property keys. *)
    method! match_object_pattern_property_key key =
      let open Ast.MatchPattern.ObjectPattern.Property in
      let (loc, token) =
        match key with
        | StringLiteral (loc, { Ast.StringLiteral.raw; _ }) -> (loc, raw)
        | NumberLiteral (loc, { Ast.NumberLiteral.raw; _ }) -> (loc, raw)
        | BigIntLiteral (loc, { Ast.BigIntLiteral.raw; _ }) -> (loc, raw)
        | Identifier (loc, { Ast.Identifier.name; _ }) -> (loc, name)
      in
      if this#covers_target loc then
        this#find loc token Ac_ignored
      else
        key

    method! match_member_pattern member_pattern =
      let open Ast.MatchPattern.MemberPattern in
      let (loc, { base; property; _ }) = member_pattern in
      let base_loc =
        match base with
        | BaseIdentifier (loc, _)
        | BaseMember (loc, _) ->
          loc
      in
      let member_loc = Some (compute_member_loc ~expr_loc:loc ~obj_loc:base_loc) in
      let obj_type () =
        match base with
        | BaseIdentifier (loc, id) ->
          Inference.type_of_identifier ~encl_ctx:Enclosing_context.NoContext cx loc id
        | BaseMember (loc, mem) -> Inference.type_of_match_member_pattern cx loc mem
      in
      (match property with
      | PropertyIdentifier (prop_loc, { Ast.Identifier.name; _ }) when this#covers_target prop_loc
        ->
        this#find
          prop_loc
          name
          (Ac_member
             {
               obj_type = obj_type ();
               in_optional_chain = false;
               bracket_syntax = None;
               member_loc;
               is_type_annotation = false;
               is_super = false;
             }
          )
      | PropertyString (prop_loc, { Ast.StringLiteral.raw = token; _ })
        when this#covers_target prop_loc ->
        let obj_type = obj_type () in
        this#find
          prop_loc
          token
          (Ac_member
             {
               obj_type;
               in_optional_chain = false;
               bracket_syntax = Some (this#default_bracket_syntax obj_type);
               member_loc;
               is_type_annotation = false;
               is_super = false;
             }
          )
      | _ -> ());
      super#match_member_pattern member_pattern
  end

let autocomplete_id ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_literal ~cursor _cx ac_loc = covers_target cursor ac_loc

let autocomplete_object_key ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let autocomplete_jsx ~cursor _cx _ac_name ac_loc = covers_target cursor ac_loc

let process_location cx ~trigger_character ~cursor aloc_ast =
  let from_trigger_character = trigger_character <> None in
  let searcher = new process_request_searcher cx ~from_trigger_character ~cursor in
  match searcher#program aloc_ast with
  | exception Found f -> Ok (Some f)
  | exception Internal_exn err -> Error err
  | _ -> Ok None

let autocomplete_set_hooks ~cursor =
  Type_inference_hooks_js.set_id_hook (autocomplete_id ~cursor);
  Type_inference_hooks_js.set_literal_hook (autocomplete_literal ~cursor);
  Type_inference_hooks_js.set_obj_prop_decl_hook (autocomplete_object_key ~cursor);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx ~cursor)

let autocomplete_unset_hooks = Type_inference_hooks_js.reset_hooks
