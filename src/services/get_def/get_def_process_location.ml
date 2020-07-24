(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
  `Ok request` =
    the given loc is in a symbol for which it makes sense to ask for a definition,
    process `request` to get to the definition
  Error loc =
    the given loc is in a symbol which is its own definition,
    `loc` is the loc of the symbol
*)
exception Result of (Get_def_request.t, ALoc.t) result

(**
 * Determines if the given expression is a `require()` call, or a member expression
 * containing one, like `require('foo').bar`.
 *)
let rec is_require ~is_legit_require expr =
  let open Flow_ast.Expression in
  match expr with
  | (_, Member { Member._object; _ }) -> is_require ~is_legit_require _object
  | ( _,
      Call
        {
          Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
          arguments = (_, { ArgList.arguments = Expression ((source_loc, _), _) :: _; _ });
          _;
        } )
    when is_legit_require source_loc ->
    true
  | _ -> false

class searcher (target_loc : Loc.t) (is_legit_require : ALoc.t -> bool) =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable in_require_declarator = false

    method with_in_require_declarator value f =
      let was_in_require_declarator = in_require_declarator in
      in_require_declarator <- value;
      let result = f () in
      in_require_declarator <- was_in_require_declarator;
      result

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

    method own_def : 'a. ALoc.t -> 'a = (fun x -> raise (Result (Error x)))

    method request : 'a. Get_def_request.t -> 'a = (fun x -> raise (Result (Ok x)))

    method! variable_declarator
        ~kind ((_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init }) as x) =
      (* If a variable declarator's initializer contains `require()`, then we want to jump
         through it into the imported module. To do this, we set the `in_require_declarator`
         flag, which we use when we visit the id, in lieu of parent pointers. *)
      let ((id_loc, _), _) = id in
      let has_require =
        match init with
        | Some init when is_require ~is_legit_require init && this#covers_target id_loc -> true
        | _ -> false
      in
      this#with_in_require_declarator has_require (fun () -> super#variable_declarator ~kind x)

    method! import_declaration import_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { source = (source_loc, { Flow_ast.StringLiteral.value = module_name; _ }); _ } = decl in
      let res = super#import_declaration import_loc decl in
      if this#covers_target import_loc then
        this#request (Get_def_request.Require ((source_loc, module_name), import_loc));
      res

    method! import_named_specifier decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { local; remote = ((remote_loc, t), _); kind = _ } = decl in
      if
        this#covers_target remote_loc
        || Base.Option.exists local ~f:(fun ((local_loc, _), _) -> this#covers_target local_loc)
      then
        this#request (Get_def_request.Type t);
      decl

    method! import_default_specifier decl =
      let ((loc, t), _) = decl in
      if this#covers_target loc then this#request (Get_def_request.Type t);
      decl

    method! import_namespace_specifier id = id

    method! export_named_declaration export_loc decl =
      let open Flow_ast.Statement.ExportNamedDeclaration in
      match decl.source with
      | None -> super#export_named_declaration export_loc decl
      | Some (source_loc, { Flow_ast.StringLiteral.value = module_name; _ }) ->
        let { specifiers; _ } = decl in
        Base.Option.iter ~f:(this#export_specifier_with_loc ~source_loc) specifiers;
        if this#covers_target export_loc then
          this#request (Get_def_request.Require ((source_loc, module_name), export_loc));
        decl

    method export_specifier_with_loc ~source_loc specifier =
      let open Flow_ast.Statement.ExportNamedDeclaration in
      match specifier with
      | ExportSpecifiers named_specifiers ->
        Base.List.iter ~f:(this#export_named_specifier_with_loc ~source_loc) named_specifiers
      | ExportBatchSpecifier _ -> ()

    method export_named_specifier_with_loc ~source_loc specifier =
      let open Flow_ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (specifier_loc, { local; _ }) = specifier in
      let (_, { Flow_ast.Identifier.name = local_name; _ }) = local in
      let result =
        Get_def_request.(
          Member { prop_name = local_name; object_source = ObjectRequireLoc source_loc })
      in
      if this#covers_target specifier_loc then this#request result

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object; property; comments = _ } = expr in
      begin
        match property with
        | PropertyIdentifier ((loc, _), { Flow_ast.Identifier.name; _ }) when this#covers_target loc
          ->
          let ((_, t), _) = _object in
          let result =
            Get_def_request.(Member { prop_name = name; object_source = ObjectType t })
          in
          this#request result
        | _ -> ()
      end;
      super#member expr

    method! t_identifier (((loc, type_), { Flow_ast.Identifier.name; _ }) as id) =
      if this#covers_target loc then this#request (Get_def_request.Identifier { name; loc; type_ });
      super#t_identifier id

    method! jsx_identifier (((loc, type_), { Flow_ast.JSX.Identifier.name; comments = _ }) as id) =
      if this#covers_target loc then this#request (Get_def_request.Identifier { name; loc; type_ });
      super#jsx_identifier id

    method! pattern ?kind (((_, t), p) as pat) =
      let open Flow_ast.Pattern in
      begin
        match p with
        | Object { Object.properties; _ } ->
          List.iter
            Object.(
              function
              | Property (_, { Property.key; _ }) ->
                begin
                  match key with
                  | Property.Literal
                      (loc, { Flow_ast.Literal.value = Flow_ast.Literal.String name; _ })
                  | Property.Identifier ((loc, _), { Flow_ast.Identifier.name; _ })
                    when this#covers_target loc ->
                    this#request
                      Get_def_request.(Member { prop_name = name; object_source = ObjectType t })
                  | _ -> ()
                end
              | _ -> ())
            properties
        | _ -> ()
      end;
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind ((loc, t), name) =
      if kind != None && this#covers_target loc then
        if in_require_declarator then
          this#request (Get_def_request.Type t)
        else
          this#own_def loc;
      super#t_pattern_identifier ?kind ((loc, t), name)

    method! expression ((loc, t), expr) =
      match expr with
      | Flow_ast.Expression.(
          Call
            {
              Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
              arguments =
                ( _,
                  {
                    Flow_ast.Expression.ArgList.arguments =
                      [
                        Expression
                          ( (source_loc, _),
                            Literal Flow_ast.Literal.{ value = String module_name; _ } );
                      ];
                    comments = _;
                  } );
              _;
            })
        when this#covers_target loc && is_legit_require source_loc ->
        this#request (Get_def_request.Require ((source_loc, module_name), loc))
      | _ -> super#expression ((loc, t), expr)

    (* object keys would normally hit this#t_identifier; this circumvents that. *)
    method! object_key_identifier id =
      let ((loc, _), _) = id in
      if this#covers_target loc then this#own_def loc;
      id

    (* for object properties using the shorthand {variableName} syntax,
     * process the value before the key so that the explicit-non-find in this#object_key_identifier
     * doesn't make us miss the variable *)
    method! object_property prop =
      let open Flow_ast.Expression.Object.Property in
      (match prop with
      | (_, Init { shorthand = true; value; _ }) -> ignore (this#expression value)
      | _ -> ());
      super#object_property prop
  end

(*  This type is distinct from the one raised by the searcher because
    it would never make sense for the searcher to raise LocNotFound *)
type result =
  | OwnDef of ALoc.t
  | Request of Get_def_request.t
  | LocNotFound

let process_location ~typed_ast ~is_legit_require loc =
  let searcher = new searcher loc is_legit_require in
  try
    ignore (searcher#program typed_ast);
    LocNotFound
  with Result info ->
    (match info with
    | Ok request -> Request request
    | Error loc -> OwnDef loc)
