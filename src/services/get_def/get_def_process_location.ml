(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* stops walking the tree *)
exception Found

(*  This type is distinct from the one raised by the searcher because
   it would never make sense for the searcher to raise LocNotFound *)
type ('M, 'T) result =
  | OwnDef of 'M
  | Request of ('M, 'T) Get_def_request.t
  | LocNotFound

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
          arguments = (_, { ArgList.arguments = Expression (source_annot, _) :: _; _ });
          _;
        } )
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

class ['M, 'T] searcher
  ~(is_legit_require : 'T -> bool)
  ~(module_ref_prefix : string option)
  ~(covers_target : 'M -> bool)
  ~(loc_of_annot : 'T -> 'M) =
  let annot_covers_target annot = covers_target (loc_of_annot annot) in
  object (this)
    inherit ['M, 'T, 'M, 'T] Flow_polymorphic_ast_mapper.mapper as super

    val mutable in_require_declarator = false

    val mutable found_loc_ = LocNotFound

    method found_loc = found_loc_

    method with_in_require_declarator value f =
      let was_in_require_declarator = in_require_declarator in
      in_require_declarator <- value;
      let result = f () in
      in_require_declarator <- was_in_require_declarator;
      result

    method on_loc_annot (x : 'M) = x

    method on_type_annot (x : 'T) = x

    method own_def : 'a. 'M -> 'a =
      fun x ->
        found_loc_ <- OwnDef x;
        raise Found

    method request : 'a. ('M, 'T) Get_def_request.t -> 'a =
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

    method! import_declaration import_loc decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { source = (source_loc, { Flow_ast.StringLiteral.value = module_name; _ }); _ } = decl in
      let res = super#import_declaration import_loc decl in
      if covers_target import_loc then
        this#request (Get_def_request.Require ((source_loc, module_name), import_loc));
      res

    method! import_named_specifier ~import_kind decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { local; remote = (remote_annot, _); kind } = decl in
      (if
       annot_covers_target remote_annot
       || Base.Option.exists local ~f:(fun (local_annot, _) -> annot_covers_target local_annot)
      then
        match (kind, import_kind) with
        | (Some ImportTypeof, _)
        | (_, ImportTypeof) ->
          this#request (Get_def_request.Typeof remote_annot)
        | _ -> this#request (Get_def_request.Type remote_annot));
      decl

    method! import_default_specifier decl =
      let (annot, _) = decl in
      if annot_covers_target annot then this#request (Get_def_request.Type annot);
      decl

    method! import_namespace_specifier id = id

    method! export_named_declaration export_loc decl =
      let open Flow_ast.Statement.ExportNamedDeclaration in
      match decl.source with
      | None -> super#export_named_declaration export_loc decl
      | Some (source_loc, { Flow_ast.StringLiteral.value = module_name; _ }) ->
        let { specifiers; _ } = decl in
        Base.Option.iter ~f:(this#export_specifier_with_loc ~source_loc) specifiers;
        if covers_target export_loc then
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
      if covers_target specifier_loc then this#request result

    method! member expr =
      let open Flow_ast.Expression.Member in
      let { _object; property; comments = _ } = expr in
      begin
        match property with
        | PropertyIdentifier (annot, { Flow_ast.Identifier.name; _ }) when annot_covers_target annot
          ->
          let (obj_annot, _) = _object in
          let result =
            Get_def_request.(Member { prop_name = name; object_source = ObjectType obj_annot })
          in
          this#request result
        | _ -> ()
      end;
      super#member expr

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
                } )
            when annot_covers_target annot ->
            let loc = loc_of_annot annot in
            this#request
              (Get_def_request.JsxAttribute
                 { component_t = annot_of_jsx_name component_name; name = attribute_name; loc })
          | _ -> ())
        attributes;
      super#jsx_opening_element elt

    method! jsx_element_name_identifier
        ((annot, { Flow_ast.JSX.Identifier.name; comments = _ }) as id) =
      if annot_covers_target annot then
        this#request (Get_def_request.Identifier { name; loc = annot });
      super#jsx_element_name_identifier id

    method! pattern ?kind ((pat_annot, p) as pat) =
      let open Flow_ast.Pattern in
      (if not in_require_declarator then
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
                    when covers_target loc ->
                    this#request
                      Get_def_request.(
                        Member { prop_name = name; object_source = ObjectType pat_annot })
                  | Property.Identifier (id_annot, { Flow_ast.Identifier.name; _ })
                    when annot_covers_target id_annot ->
                    this#request
                      Get_def_request.(
                        Member { prop_name = name; object_source = ObjectType pat_annot })
                  | _ -> ()
                end
              | _ -> ())
            properties
        | _ -> ());
      super#pattern ?kind pat

    method! t_pattern_identifier ?kind (annot, name) =
      if kind != None && annot_covers_target annot then
        if in_require_declarator then
          this#request (Get_def_request.Type annot)
        else
          this#own_def (loc_of_annot annot);
      super#t_pattern_identifier ?kind (annot, name)

    method! expression (annot, expr) =
      match (expr, module_ref_prefix) with
      | ( Flow_ast.Expression.(
            Call
              {
                Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
                arguments =
                  ( _,
                    {
                      Flow_ast.Expression.ArgList.arguments =
                        [
                          Expression
                            ( source_annot,
                              Literal Flow_ast.Literal.{ value = String module_name; _ } );
                        ];
                      comments = _;
                    } );
                _;
              }),
          _ )
        when annot_covers_target annot && is_legit_require source_annot ->
        let loc = loc_of_annot annot in
        let source_loc = loc_of_annot source_annot in
        this#request (Get_def_request.Require ((source_loc, module_name), loc))
      | (Flow_ast.Expression.Literal Flow_ast.Literal.{ value = String str; _ }, Some prefix)
        when annot_covers_target annot && Base.String.is_prefix str ~prefix ->
        let loc = loc_of_annot annot in
        this#request (Get_def_request.Require ((loc, str), loc))
      | _ -> super#expression (annot, expr)

    (* object keys would normally hit this#t_identifier; this circumvents that. *)
    method! object_key_identifier id =
      let (annot, _) = id in
      if annot_covers_target annot then this#own_def (loc_of_annot annot);
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

let process_location ~loc_of_annot ~ast ~is_legit_require ~module_ref_prefix ~covers_target =
  let searcher = new searcher ~loc_of_annot ~is_legit_require ~module_ref_prefix ~covers_target in
  (try ignore (searcher#program ast) with
  | Found -> ());
  searcher#found_loc

let process_location_in_ast ~ast ~is_legit_require ~module_ref_prefix loc =
  let loc_of_annot loc = loc in
  let covers_target test_loc = Reason.in_range loc test_loc in
  process_location ~loc_of_annot ~is_legit_require ~ast ~module_ref_prefix ~covers_target

let process_location_in_typed_ast ~typed_ast ~is_legit_require ~module_ref_prefix loc =
  let loc_of_annot (loc, _) = loc in
  let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
  process_location ~loc_of_annot ~is_legit_require ~ast:typed_ast ~module_ref_prefix ~covers_target
