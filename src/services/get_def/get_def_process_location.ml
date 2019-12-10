(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type result =
  | Loc of ALoc.t
  | Chain of Get_def_request.t
  | No_loc

exception Result of result

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
          arguments = Expression ((source_loc, _), _) :: _;
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

    method find_loc : 'a. ALoc.t -> 'a = (fun x -> raise (Result (Loc x)))

    method chain : 'a. Get_def_request.t -> 'a = (fun x -> raise (Result (Chain x)))

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
      Flow_ast.Statement.ImportDeclaration.(
        let {
          importKind = _;
          source = (source_loc, { Flow_ast.StringLiteral.value = module_name; _ });
          specifiers;
          default;
        } =
          decl
        in
        Option.iter ~f:(this#import_specifier_with_loc ~source_loc) specifiers;
        Option.iter ~f:(this#import_default_specifier_with_loc ~source_loc) default;
        if this#covers_target import_loc then
          this#chain (Get_def_request.Require ((source_loc, module_name), import_loc));
        decl)

    method import_specifier_with_loc ~source_loc specifier =
      Flow_ast.Statement.ImportDeclaration.(
        match specifier with
        | ImportNamedSpecifiers named_specifiers ->
          Base.List.iter ~f:(this#import_named_specifier_with_loc ~source_loc) named_specifiers
        | ImportNamespaceSpecifier _ -> ())

    method import_named_specifier_with_loc ~source_loc specifier =
      Flow_ast.Statement.ImportDeclaration.(
        let { kind = _; local; remote } = specifier in
        let ((remote_name_loc, _), { Flow_ast.Identifier.name = remote_name; _ }) = remote in
        let result =
          Get_def_request.(
            Member { prop_name = remote_name; object_source = ObjectRequireLoc source_loc })
        in
        if this#covers_target remote_name_loc then this#chain result;
        Option.iter
          ~f:(fun local ->
            let ((local_name_loc, _), _) = local in
            if this#covers_target local_name_loc then
              let result =
                Get_def_request.(
                  Member { prop_name = remote_name; object_source = ObjectRequireLoc source_loc })
              in
              this#chain result)
          local)

    method! member expr =
      Flow_ast.Expression.Member.(
        let { _object; property } = expr in
        begin
          match property with
          | PropertyIdentifier ((loc, _), { Flow_ast.Identifier.name; _ })
            when this#covers_target loc ->
            let ((_, t), _) = _object in
            let result =
              Get_def_request.(Member { prop_name = name; object_source = ObjectType t })
            in
            this#chain result
          | _ -> ()
        end;
        super#member expr)

    method import_default_specifier_with_loc ~source_loc default =
      let ((remote_name_loc, _), _) = default in
      if this#covers_target remote_name_loc then
        let result =
          Get_def_request.(
            Member
              {
                prop_name = "default";
                (* see members.ml *)
                object_source = ObjectRequireLoc source_loc;
              })
        in
        this#chain result

    method! t_identifier (((loc, type_), { Flow_ast.Identifier.name; _ }) as id) =
      if this#covers_target loc then this#chain (Get_def_request.Identifier { name; loc; type_ });
      super#t_identifier id

    method! jsx_identifier (((loc, type_), { Flow_ast.JSX.Identifier.name }) as id) =
      if this#covers_target loc then this#chain (Get_def_request.Identifier { name; loc; type_ });
      super#jsx_identifier id

    method! pattern ?kind (((_, t), p) as pat) =
      Flow_ast.Pattern.(
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
                      this#chain
                        Get_def_request.(Member { prop_name = name; object_source = ObjectType t })
                    | _ -> ()
                  end
                | _ -> ())
              properties
          | _ -> ()
        end;
        super#pattern ?kind pat)

    method! t_pattern_identifier ?kind ((loc, t), name) =
      if kind != None && this#covers_target loc then
        if in_require_declarator then
          this#chain (Get_def_request.Type t)
        else
          this#find_loc loc;
      super#t_pattern_identifier ?kind ((loc, t), name)

    method! expression ((loc, t), expr) =
      match expr with
      | Flow_ast.Expression.(
          Call
            {
              Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
              arguments =
                [
                  Expression
                    ((source_loc, _), Literal Flow_ast.Literal.{ value = String module_name; _ });
                ];
              _;
            })
        when this#covers_target loc && is_legit_require source_loc ->
        this#chain (Get_def_request.Require ((source_loc, module_name), loc))
      | _ -> super#expression ((loc, t), expr)
  end

let process_location ~typed_ast ~is_legit_require loc =
  let searcher = new searcher loc is_legit_require in
  try
    ignore (searcher#program typed_ast);
    No_loc
  with Result info -> info
