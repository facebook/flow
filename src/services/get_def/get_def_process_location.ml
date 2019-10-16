(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of Get_def_request.t

class searcher (target_loc : Loc.t) =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

    method find_loc x = raise (Found x)

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
          this#find_loc (Get_def_request.Require ((source_loc, module_name), import_loc));
        decl)

    method import_specifier_with_loc ~source_loc specifier =
      Flow_ast.Statement.ImportDeclaration.(
        match specifier with
        | ImportNamedSpecifiers named_specifiers ->
          Core_list.iter ~f:(this#import_named_specifier_with_loc ~source_loc) named_specifiers
        | ImportNamespaceSpecifier _ -> ())

    method import_named_specifier_with_loc ~source_loc specifier =
      Flow_ast.Statement.ImportDeclaration.(
        let { kind = _; local; remote } = specifier in
        let ((remote_name_loc, _), { Flow_ast.Identifier.name = remote_name; _ }) = remote in
        let result =
          Get_def_request.(
            Member { prop_name = remote_name; object_source = ObjectRequireLoc source_loc })
        in
        if this#covers_target remote_name_loc then this#find_loc result;
        Option.iter
          ~f:(fun local ->
            let ((local_name_loc, _), _) = local in
            if this#covers_target local_name_loc then
              let result =
                Get_def_request.(
                  Member { prop_name = remote_name; object_source = ObjectRequireLoc source_loc })
              in
              this#find_loc result)
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
            this#find_loc result
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
        this#find_loc result

    method! t_identifier (((loc, _t), { Flow_ast.Identifier.name; _ }) as id) =
      if this#covers_target loc then this#find_loc (Get_def_request.Identifier (name, loc));
      super#t_identifier id

    method! jsx_identifier (((loc, _t), { Flow_ast.JSX.Identifier.name }) as id) =
      if this#covers_target loc then this#find_loc (Get_def_request.Identifier (name, loc));
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
                      this#find_loc
                        Get_def_request.(Member { prop_name = name; object_source = ObjectType t })
                    | _ -> ()
                  end
                | _ -> ())
              properties
          | _ -> ()
        end;
        super#pattern ?kind pat)

    method! t_pattern_identifier ?kind ((loc, t), name) =
      if kind != None && this#covers_target loc then this#find_loc (Get_def_request.Type t);
      super#t_pattern_identifier ?kind ((loc, t), name)

    method! expression ((loc, t), expr) =
      ( if this#covers_target loc then
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
              }) ->
          this#find_loc (Get_def_request.Require ((source_loc, module_name), loc))
        | _ -> () );
      super#expression ((loc, t), expr)
  end

let process_location ~typed_ast loc =
  let searcher = new searcher loc in
  try
    ignore (searcher#program typed_ast);
    None
  with Found info -> Some info
