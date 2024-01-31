(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** stops walking the tree *)
exception Found

(** This type is distinct from the one raised by the searcher because
  it would never make sense for the searcher to raise LocNotFound *)
type 'loc result =
  | OwnDef of 'loc * (* name *) string
  | Request of ('loc, 'loc * (Type.t[@opaque])) Get_def_request.t
  | Empty of string
  | LocNotFound
[@@deriving show]

(* here lies the difference between "Go to Definition" and "Go to Type Definition":
   the former should stop on annot_loc (where the value was annotated), while the
   latter should jump to the def_loc (where the type was defined).

   for now, we only implement Go to Definition; if we want to do Go to Type
   Definition, it would ignore the annot loc. *)
let rec process_type_request cx =
  let open Type in
  function
  | OpenT _ as t ->
    (match Flow_js_utils.possible_types_of_type cx t with
    | [t'] -> process_type_request cx t'
    | [] -> Error "No possible types"
    | _ :: _ -> Error "More than one possible type")
  | t ->
    let r = TypeUtil.reason_of_t t in
    let aloc =
      match Reason.annot_loc_of_reason r with
      | Some aloc -> aloc
      | None -> Reason.def_loc_of_reason r
    in
    Ok aloc

(** Determines if the given expression is a [require()] call, or a member expression
  containing one, like [require('foo').bar]. *)
let rec is_require ~is_legit_require expr =
  let open Flow_ast.Expression in
  match expr with
  | (_, Member { Member._object; _ }) -> is_require ~is_legit_require _object
  | ( _,
      Call
        {
          Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
          arguments = (_, { ArgList.arguments = Expression (source_annot, _) :: _; _ });
          _;
        }
    )
    when is_legit_require source_annot ->
    true
  | _ -> false

let annot_of_jsx_name =
  let open Flow_ast.JSX in
  function
  | Identifier (annot, _)
  | NamespacedName (_, NamespacedName.{ name = (annot, _); _ })
  | MemberExpression (_, MemberExpression.{ property = (annot, _); _ }) ->
    annot

class searcher
  ~(is_legit_require : ALoc.t * Type.t -> bool) ~(covers_target : ALoc.t -> bool) ~purpose =
  let loc_of_annot (loc, _) = loc in
  let annot_covers_target annot = covers_target (loc_of_annot annot) in
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable in_require_declarator = false

    val mutable available_private_names : ALoc.t SMap.t = SMap.empty

    val mutable found_loc_ = LocNotFound

    method found_loc = found_loc_

    method with_in_require_declarator value f =
      let was_in_require_declarator = in_require_declarator in
      in_require_declarator <- value;
      let result = f () in
      in_require_declarator <- was_in_require_declarator;
      result

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    method own_def : 'a. ALoc.t -> string -> 'a =
      fun loc name ->
        found_loc_ <- OwnDef (loc, name);
        raise Found

    method found_empty : 'a. string -> 'a =
      fun x ->
        found_loc_ <- Empty x;
        raise Found

    method request : 'a. (ALoc.t, ALoc.t * Type.t) Get_def_request.t -> 'a =
      fun x ->
        found_loc_ <- Request x;
        raise Found

    method! variable_declarator
        ~kind ((_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init }) as x) =
      (* If a variable declarator's initializer contains `require()`, then we want to jump
         through it into the imported module. To do this, we set the `in_require_declarator`
         flag, which we use when we visit the id, in lieu of parent pointers. *)
      let (id_annot, _) = id in
      let has_require =
        match init with
        | Some init when is_require ~is_legit_require init && annot_covers_target id_annot -> true
        | _ -> false
      in
      this#with_in_require_declarator has_require (fun () -> super#variable_declarator ~kind x)

    method! import_source source_annot lit =
      if annot_covers_target source_annot then
        this#request (Get_def_request.Type { annot = source_annot; name = None });
      super#import_source source_annot lit

    method! import_named_specifier ~import_kind:_ decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let {
        local;
        remote = (remote_annot, { Flow_ast.Identifier.name; _ });
        remote_name_def_loc;
        kind = _;
      } =
        decl
      in
      ( if annot_covers_target remote_annot then
        match remote_name_def_loc with
        | Some l -> this#own_def l name
        | None -> this#own_def (fst remote_annot) "default"
      );
      Base.Option.iter local ~f:(fun (local_annot, _) ->
          if annot_covers_target local_annot then
            match remote_name_def_loc with
            | Some l -> this#own_def l name
            | None -> this#own_def (fst local_annot) "default"
      );
      decl

    method! import_declaration loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { default; specifiers; source; _ } = decl in
      Base.Option.iter default ~f:(fun { identifier = (annot, _); remote_default_name_def_loc } ->
          if annot_covers_target annot then
            match remote_default_name_def_loc with
            | Some l -> this#own_def l "default"
            | None -> this#own_def (fst annot) "default"
      );
      Base.Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers _ -> ()
          | ImportNamespaceSpecifier (l, ((name_loc, _), { Flow_ast.Identifier.name; _ })) ->
            if covers_target l then (
              match purpose with
              | Get_def_types.Purpose.GoToDefinition ->
                this#request (Get_def_request.Type { annot = fst source; name = Some name })
              | Get_def_types.Purpose.FindReferences -> ignore @@ this#own_def name_loc name
            )
          );
      super#import_declaration loc decl

    method! export_source source_annot lit =
      if annot_covers_target source_annot then
        this#request (Get_def_request.Type { annot = source_annot; name = None });
      super#export_source source_annot lit

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object; property; comments = _ } = expr in
      begin
        match property with
        | PropertyIdentifier (annot, { Flow_ast.Identifier.name; _ }) when annot_covers_target annot
          ->
          let (obj_annot, _) = _object in
          let force_instance = Flow_ast_utils.is_super_member_access expr in
          let result =
            Get_def_request.(Member { prop_name = name; object_type = obj_annot; force_instance })
          in
          this#request result
        | _ -> ()
      end;
      super#member expr

    method! indexed_access_type expr =
      let open Flow_ast.Type.IndexedAccess in
      let { _object; index; comments = _ } = expr in
      (match index with
      | (annot, Flow_ast.Type.StringLiteral { Flow_ast.StringLiteral.value; _ })
        when annot_covers_target annot ->
        let (obj_annot, _) = _object in
        let result =
          Get_def_request.(
            Member { prop_name = value; object_type = obj_annot; force_instance = false }
          )
        in
        this#request result
      | _ -> ());
      super#indexed_access_type expr

    method! t_identifier ((loc, { Flow_ast.Identifier.name; _ }) as id) =
      if annot_covers_target loc then this#request (Get_def_request.Identifier { name; loc });
      super#t_identifier id

    method! jsx_opening_element elt =
      let open Flow_ast.JSX in
      let (_, Opening.{ name = component_name; attributes; _ }) = elt in
      List.iter
        (function
          | Opening.Attribute
              ( _,
                {
                  Attribute.name =
                    Attribute.Identifier (annot, { Identifier.name = attribute_name; comments = _ });
                  _;
                }
              )
            when annot_covers_target annot ->
            let loc = loc_of_annot annot in
            this#request
              (Get_def_request.JsxAttribute
                 { component_t = annot_of_jsx_name component_name; name = attribute_name; loc }
              )
          | _ -> ())
        attributes;
      super#jsx_opening_element elt

    method! jsx_element_name_identifier
        ((annot, { Flow_ast.JSX.Identifier.name; comments = _ }) as id) =
      if annot_covers_target annot then
        this#request (Get_def_request.Identifier { name; loc = annot });
      super#jsx_element_name_identifier id

    method! jsx_element_name_namespaced ns =
      let (loc, _) = ns in
      (* TODO: this should be supported *)
      if covers_target loc then this#found_empty "jsx element (namespaced)";
      super#jsx_element_name_namespaced ns

    method! jsx_element_name_member_expression expr =
      let (loc, _) = expr in
      (* TODO: this should be supported *)
      if covers_target loc then this#found_empty "jsx element (member)";
      super#jsx_member_expression expr

    method! pattern ?kind ((pat_annot, p) as pat) =
      let open Flow_ast.Pattern in
      (* In const {foo: bar} = require('some_module'); foo and bar should jump to prop def of foo,
         while in other cases, bar should be its own definition. *)
      let is_id_pattern_of_obj_key_in_require_declarator = function
        | (id_annot, Identifier _) -> in_require_declarator && annot_covers_target id_annot
        | _ -> false
      in
      let () =
        match p with
        | Object { Object.properties; _ } ->
          List.iter
            Object.(
              function
              | Property (_, { Property.key; pattern; _ }) -> begin
                match key with
                | Property.StringLiteral (loc, { Flow_ast.StringLiteral.value = name; _ })
                  when covers_target loc || is_id_pattern_of_obj_key_in_require_declarator pattern
                  ->
                  this#request
                    Get_def_request.(
                      Member { prop_name = name; object_type = pat_annot; force_instance = false }
                    )
                | Property.Identifier (id_annot, { Flow_ast.Identifier.name; _ })
                  when annot_covers_target id_annot
                       || is_id_pattern_of_obj_key_in_require_declarator pattern ->
                  this#request
                    Get_def_request.(
                      Member { prop_name = name; object_type = pat_annot; force_instance = false }
                    )
                | _ -> ()
              end
              | _ -> ()
            )
            properties
        | _ -> ()
      in
      super#pattern ?kind pat

    method! pattern_identifier
        ?kind (annot, ({ Flow_ast.Identifier.name; comments = _ } as name_node)) =
      if kind != None && annot_covers_target annot then
        if in_require_declarator then
          this#request (Get_def_request.Type { annot; name = Some name })
        else
          this#own_def (loc_of_annot annot) name;
      super#pattern_identifier ?kind (annot, name_node)

    method! expression (annot, expr) =
      let open Flow_ast in
      let open Expression in
      if annot_covers_target annot then
        match expr with
        | StringLiteral _ -> this#found_empty "string"
        | NumberLiteral _ -> this#found_empty "number"
        | BigIntLiteral _ -> this#found_empty "bigint"
        | BooleanLiteral _ -> this#found_empty "boolean"
        | NullLiteral _ -> this#found_empty "null"
        | RegExpLiteral _ -> this#found_empty "regexp"
        | Call
            {
              Call.callee = (_, Identifier (_, { Identifier.name = "require"; _ }));
              arguments =
                ( _,
                  { ArgList.arguments = [Expression (source_annot, StringLiteral _)]; comments = _ }
                );
              _;
            }
          when is_legit_require source_annot ->
          this#request (Get_def_request.Type { annot; name = None })
        | _ -> super#expression (annot, expr)
      else
        (* it is tempting to not recurse here, but comments are not included in
           `annot`, so we have to dig into each child to visit their `comments`
           fields. *)
        super#expression (annot, expr)

    method! type_ (annot, t) =
      if annot_covers_target annot then
        let open! Flow_ast.Type in
        match t with
        | Any _
        | Mixed _
        | Empty _
        | Void _
        | Null _
        | Symbol _
        | Number _
        | NumberLiteral _
        | BigInt _
        | BigIntLiteral _
        | String _
        | StringLiteral _
        | Boolean _
        | BooleanLiteral _
        | Exists _
        | Unknown _
        | Never _
        | Undefined _ ->
          this#found_empty "type literal"
        | Nullable _
        | Array _
        | Conditional _
        | Infer _
        | Typeof _
        | Keyof _
        | ReadOnly _
        | Function _
        | Component _
        | Object _
        | Interface _
        | Generic _
        | IndexedAccess _
        | OptionalIndexedAccess _
        | Union _
        | Intersection _
        | Tuple _
        | Renders _ ->
          super#type_ (annot, t)
      else
        (* it is tempting to not recurse here, but comments are not included in
           `annot`, so we have to dig into each child to visit their `comments`
           fields. *)
        super#type_ (annot, t)

    method! type_param_identifier id =
      let (loc, { Flow_ast.Identifier.name; comments = _ }) = id in
      if covers_target loc then this#own_def loc name;
      id

    method! module_ref_literal mref =
      let { Flow_ast.ModuleRefLiteral.require_out; _ } = mref in
      if annot_covers_target require_out then
        this#request (Get_def_request.Type { annot = require_out; name = None })
      else
        super#module_ref_literal mref

    method! enum_member_identifier id =
      let (loc, { Flow_ast.Identifier.name; comments = _ }) = id in
      if covers_target loc then this#own_def loc name;
      super#enum_member_identifier id

    (* object keys would normally hit this#t_identifier; this circumvents that. *)
    method! object_key_identifier id =
      let (annot, { Flow_ast.Identifier.name; comments = _ }) = id in
      if annot_covers_target annot then this#own_def (loc_of_annot annot) name;
      id

    method! object_key_string_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    method! object_key_number_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    method! object_key_bigint_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    (* for object properties using the shorthand {variableName} syntax,
     * process the value before the key so that the explicit-non-find in this#object_key_identifier
     * doesn't make us miss the variable *)
    method! object_property prop =
      let open Flow_ast.Expression.Object.Property in
      (match prop with
      | (_, Init { shorthand = true; value; _ }) -> ignore (this#expression value)
      | _ -> ());
      super#object_property prop

    method! new_ expr =
      let { Flow_ast.Expression.New.callee = (_, callee); _ } = expr in
      begin
        match callee with
        | Flow_ast.Expression.Identifier (annot, _) when annot_covers_target annot ->
          this#request
            Get_def_request.(
              Member
                {
                  prop_name = "constructor";
                  object_type = annot;
                  (* In `new Foo()`, the type of Foo is ThisClassT(InstanceT).
                     We use force_instance to force the normalizer to inspect
                     the InstanceT instead of the static properties of the class *)
                  force_instance = true;
                }
            )
        | _ -> ()
      end;
      super#new_ expr

    method! comment c =
      let (loc, _) = c in
      if covers_target loc then this#found_empty "comment";
      c

    method! template_literal_element e =
      let (loc, _) = e in
      if covers_target loc then this#found_empty "template";
      e

    method! jsx_attribute_value_literal lit =
      let (annot, _) = lit in
      if annot_covers_target annot then this#found_empty "jsx attribute literal";
      lit

    method! jsx_attribute_name_namespaced name =
      let (loc, _) = name in
      (* TODO: this should be supported *)
      if covers_target loc then this#found_empty "jsx attribute (namespaced)";
      name

    method! jsx_child child =
      let (loc, c) = child in
      match c with
      | Flow_ast.JSX.Text _ when covers_target loc -> this#found_empty "jsx text"
      | _ -> super#jsx_child child

    method! class_body cls_body =
      let open Flow_ast.Class in
      let (_, { Body.body; comments = _ }) = cls_body in
      let new_available_private_names =
        let add_private_name (loc, { Flow_ast.PrivateName.name; _ }) = SMap.add name loc in
        Base.List.fold body ~init:available_private_names ~f:(fun acc -> function
          | Body.Method (_, { Method.key = Flow_ast.Expression.Object.Property.PrivateName name; _ })
          | Body.PrivateField (_, { PrivateField.key = name; _ }) ->
            add_private_name name acc
          | Body.Method _ -> acc
          | Body.Property _ -> acc
        )
      in
      let saved_available_private_names = available_private_names in
      available_private_names <- new_available_private_names;
      let result = Base.Result.try_with (fun () -> super#class_body cls_body) in
      available_private_names <- saved_available_private_names;
      Base.Result.ok_exn result

    method! private_name ((loc, { Flow_ast.PrivateName.name; comments = _ }) as pn) =
      if covers_target loc then
        match SMap.find_opt name available_private_names with
        | None -> this#found_empty "unbound private name"
        | Some l -> this#own_def l name
      else
        pn
  end

let process_location_in_typed_ast ~typed_ast ~is_legit_require ~purpose loc =
  let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
  let searcher = new searcher ~is_legit_require ~covers_target ~purpose in
  (try ignore (searcher#program typed_ast) with
  | Found -> ());
  searcher#found_loc
