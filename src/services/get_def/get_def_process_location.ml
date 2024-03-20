(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Typed_ast_finder
module Ast = Flow_ast

(** stops walking the tree *)
exception Found

type internal_error =
  | Enclosing_node_error
  | On_demand_tast_error
[@@deriving show]

(** This type is distinct from the one raised by the searcher because
  it would never make sense for the searcher to raise LocNotFound *)
type 'loc result =
  | OwnDef of 'loc * (* name *) string
  | Request of ('loc, 'loc * (Type.t[@opaque])) Get_def_request.t
  | Empty of string
  | LocNotFound
  | InternalError of internal_error
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
  let open Ast.Expression in
  match expr with
  | (_, Member { Member._object; _ }) -> is_require ~is_legit_require _object
  | ( _,
      Call
        {
          Call.callee = (_, Identifier (_, { Ast.Identifier.name = "require"; _ }));
          arguments = (_, { ArgList.arguments = Expression (source_annot, _) :: _; _ });
          _;
        }
    )
    when is_legit_require source_annot ->
    true
  | _ -> false

let annot_of_jsx_name =
  let open Ast.JSX in
  function
  | Identifier (annot, _)
  | NamespacedName (_, NamespacedName.{ name = (annot, _); _ })
  | MemberExpression (_, MemberExpression.{ property = (annot, _); _ }) ->
    annot

class virtual ['T] searcher _cx ~is_legit_require ~covers_target ~purpose =
  object (this)
    inherit [ALoc.t, 'T, ALoc.t, 'T] enclosing_node_mapper as super

    method virtual loc_of_annot : 'T -> ALoc.t

    method private annot_covers_target annot = covers_target (this#loc_of_annot annot)

    method private is_legit_require annot = is_legit_require (this#loc_of_annot annot)

    val mutable in_require_declarator = false

    val mutable available_private_names : ALoc.t SMap.t = SMap.empty

    val mutable found_loc_ = LocNotFound

    method found_loc = found_loc_

    method private with_in_require_declarator value f =
      let was_in_require_declarator = in_require_declarator in
      in_require_declarator <- value;
      let result = f () in
      in_require_declarator <- was_in_require_declarator;
      result

    method private on_loc_annot x = x

    method private own_def : 'a. ALoc.t -> string -> 'a =
      fun loc name ->
        found_loc_ <- OwnDef (loc, name);
        raise Found

    method private found_empty : 'a. string -> 'a =
      fun x ->
        found_loc_ <- Empty x;
        raise Found

    method private request : 'a. (ALoc.t, ALoc.t * Type.t) Get_def_request.t -> 'a =
      fun x ->
        found_loc_ <- Request x;
        raise Found

    method virtual private type_from_enclosing_node : 'T -> Type.t

    method virtual private get_module_t : 'T -> ALoc.t Ast.StringLiteral.t -> Type.t

    method virtual private remote_name_def_loc_of_import_named_specifier
        : (ALoc.t, 'T) Ast.Statement.ImportDeclaration.named_specifier -> ALoc.t option

    method virtual private remote_default_name_def_loc_of_import_declaration
        : ALoc.t * (ALoc.t, 'T) Ast.Statement.ImportDeclaration.t -> ALoc.t option

    method virtual private component_name_of_jsx_element
        : 'T -> (ALoc.t, 'T) Ast.JSX.element -> ALoc.t * Type.t

    method! variable_declarator
        ~kind ((_, { Ast.Statement.VariableDeclaration.Declarator.id; init }) as x) =
      (* If a variable declarator's initializer contains `require()`, then we want to jump
         through it into the imported module. To do this, we set the `in_require_declarator`
         flag, which we use when we visit the id, in lieu of parent pointers. *)
      let (id_annot, _) = id in
      let has_require =
        match init with
        | Some init
          when is_require ~is_legit_require:this#is_legit_require init
               && this#annot_covers_target id_annot ->
          true
        | _ -> false
      in
      this#with_in_require_declarator has_require (fun () -> super#variable_declarator ~kind x)

    method! import_source source_annot lit =
      if this#annot_covers_target source_annot then begin
        let source_annot = (this#loc_of_annot source_annot, this#get_module_t source_annot lit) in
        this#request (Get_def_request.Type { annot = source_annot; name = None })
      end;
      super#import_source source_annot lit

    method! import_named_specifier ~import_kind:_ decl =
      let open Ast.Statement.ImportDeclaration in
      let {
        local;
        remote = (remote_annot, { Ast.Identifier.name; _ });
        remote_name_def_loc = _;
        kind = _;
      } =
        decl
      in
      ( if this#annot_covers_target remote_annot then
        match this#remote_name_def_loc_of_import_named_specifier decl with
        | Some l -> this#own_def l name
        | None -> this#own_def (this#loc_of_annot remote_annot) "default"
      );
      Base.Option.iter local ~f:(fun (local_annot, _) ->
          if this#annot_covers_target local_annot then
            match this#remote_name_def_loc_of_import_named_specifier decl with
            | Some l -> this#own_def l name
            | None -> this#own_def (this#loc_of_annot local_annot) "default"
      );
      decl

    method! import_declaration loc decl =
      let open Ast.Statement.ImportDeclaration in
      let { default; specifiers; source = (source_annot, _); _ } = decl in
      Base.Option.iter default ~f:(fun { identifier = (annot, _); _ } ->
          if this#annot_covers_target annot then
            match this#remote_default_name_def_loc_of_import_declaration (loc, decl) with
            | Some l -> this#own_def l "default"
            | None -> this#own_def (this#loc_of_annot annot) "default"
      );
      Base.Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers _ -> ()
          | ImportNamespaceSpecifier (l, (name_annot, { Ast.Identifier.name; _ })) ->
            if covers_target l then (
              match purpose with
              | Get_def_types.Purpose.GoToDefinition ->
                let t = this#type_from_enclosing_node source_annot in
                this#request
                  (Get_def_request.Type
                     { annot = (this#loc_of_annot source_annot, t); name = Some name }
                  )
              | Get_def_types.Purpose.FindReferences ->
                ignore @@ this#own_def (this#loc_of_annot name_annot) name
            )
          );
      super#import_declaration loc decl

    method! export_source source_annot lit =
      if this#annot_covers_target source_annot then begin
        let source_annot = (this#loc_of_annot source_annot, this#get_module_t source_annot lit) in
        this#request (Get_def_request.Type { annot = source_annot; name = None })
      end;
      super#export_source source_annot lit

    method! member loc expr =
      let open Ast.Expression.Member in
      let { _object; property; comments = _ } = expr in
      begin
        match property with
        | PropertyIdentifier (annot, { Ast.Identifier.name; _ }) when this#annot_covers_target annot
          ->
          let (obj_annot, _) = _object in
          let obj_annot = (this#loc_of_annot obj_annot, this#type_from_enclosing_node obj_annot) in
          let force_instance = Flow_ast_utils.is_super_member_access expr in
          let result =
            Get_def_request.(Member { prop_name = name; object_type = obj_annot; force_instance })
          in
          this#request result
        | _ -> ()
      end;
      super#member loc expr

    method! indexed_access_type expr =
      let open Ast.Type.IndexedAccess in
      let { _object; index; comments = _ } = expr in
      (match index with
      | (annot, Ast.Type.StringLiteral { Ast.StringLiteral.value; _ })
        when this#annot_covers_target annot ->
        let (obj_annot, _) = _object in
        let obj_annot = (this#loc_of_annot obj_annot, this#type_from_enclosing_node obj_annot) in
        let result =
          Get_def_request.(
            Member { prop_name = value; object_type = obj_annot; force_instance = false }
          )
        in
        this#request result
      | _ -> ());
      super#indexed_access_type expr

    method! t_identifier ((loc, { Ast.Identifier.name; _ }) as id) =
      if this#annot_covers_target loc then begin
        let t = this#type_from_enclosing_node loc in
        this#request (Get_def_request.Identifier { name; loc = (this#loc_of_annot loc, t) })
      end;
      super#t_identifier id

    method! jsx_element expr_loc expr =
      let open Ast.JSX in
      let { opening_element; closing_element = _; children = _; comments = _ } = expr in
      let (_, Opening.{ name = _component_name; attributes; _ }) = opening_element in
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
            when this#annot_covers_target annot ->
            let loc = this#loc_of_annot annot in
            let component_t = this#component_name_of_jsx_element expr_loc expr in
            this#request (Get_def_request.JsxAttribute { component_t; name = attribute_name; loc })
          | _ -> ())
        attributes;
      super#jsx_element expr_loc expr

    method! jsx_element_name_identifier ((annot, { Ast.JSX.Identifier.name; comments = _ }) as id) =
      if this#annot_covers_target annot then begin
        let annot = (this#loc_of_annot annot, this#type_from_enclosing_node annot) in
        this#request (Get_def_request.Identifier { name; loc = annot })
      end;
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
      let open Ast.Pattern in
      (* In const {foo: bar} = require('some_module'); foo and bar should jump to prop def of foo,
         while in other cases, bar should be its own definition. *)
      let is_id_pattern_of_obj_key_in_require_declarator = function
        | (id_annot, Identifier _) -> in_require_declarator && this#annot_covers_target id_annot
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
                | Property.StringLiteral (loc, { Ast.StringLiteral.value = name; _ })
                  when covers_target loc || is_id_pattern_of_obj_key_in_require_declarator pattern
                  ->
                  let pat_annot =
                    (this#loc_of_annot pat_annot, this#type_from_enclosing_node pat_annot)
                  in
                  this#request
                    Get_def_request.(
                      Member { prop_name = name; object_type = pat_annot; force_instance = false }
                    )
                | Property.Identifier (id_annot, { Ast.Identifier.name; _ })
                  when this#annot_covers_target id_annot
                       || is_id_pattern_of_obj_key_in_require_declarator pattern ->
                  let pat_annot =
                    (this#loc_of_annot pat_annot, this#type_from_enclosing_node pat_annot)
                  in
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

    method! pattern_identifier ?kind (annot, ({ Ast.Identifier.name; comments = _ } as name_node)) =
      if kind != None && this#annot_covers_target annot then
        if in_require_declarator then
          let annot = (this#loc_of_annot annot, this#type_from_enclosing_node annot) in
          this#request (Get_def_request.Type { annot; name = Some name })
        else
          this#own_def (this#loc_of_annot annot) name;
      super#pattern_identifier ?kind (annot, name_node)

    method! expression (annot, expr) =
      let open Ast in
      let open Expression in
      if this#annot_covers_target annot then
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
          when this#is_legit_require source_annot ->
          let annot = (this#loc_of_annot annot, this#type_from_enclosing_node annot) in
          this#request (Get_def_request.Type { annot; name = None })
        | _ -> super#expression (annot, expr)
      else
        (* it is tempting to not recurse here, but comments are not included in
           `annot`, so we have to dig into each child to visit their `comments`
           fields. *)
        super#expression (annot, expr)

    method! type_ (annot, t) =
      if this#annot_covers_target annot then
        let open! Ast.Type in
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
      let (loc, { Ast.Identifier.name; comments = _ }) = id in
      if covers_target loc then this#own_def loc name;
      id

    method! module_ref_literal mref =
      let { Ast.ModuleRefLiteral.require_out; _ } = mref in
      if this#annot_covers_target require_out then
        let require_out =
          (this#loc_of_annot require_out, this#type_from_enclosing_node require_out)
        in
        this#request (Get_def_request.Type { annot = require_out; name = None })
      else
        super#module_ref_literal mref

    method! enum_member_identifier id =
      let (loc, { Ast.Identifier.name; comments = _ }) = id in
      if covers_target loc then this#own_def loc name;
      super#enum_member_identifier id

    (* object keys would normally hit this#t_identifier; this circumvents that. *)
    method! object_key_identifier id =
      let (annot, { Ast.Identifier.name; comments = _ }) = id in
      if this#annot_covers_target annot then this#own_def (this#loc_of_annot annot) name;
      id

    method! object_key_string_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if this#annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    method! object_key_number_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if this#annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    method! object_key_bigint_literal literal =
      let (annot, _) = literal in
      (* TODO: this should be supported *)
      if this#annot_covers_target annot then this#found_empty "object key (literal)";
      literal

    (* for object properties using the shorthand {variableName} syntax,
     * process the value before the key so that the explicit-non-find in this#object_key_identifier
     * doesn't make us miss the variable *)
    method! object_property prop =
      let open Ast.Expression.Object.Property in
      (match prop with
      | (_, Init { shorthand = true; value; _ }) -> ignore (this#expression value)
      | _ -> ());
      super#object_property prop

    method! new_ expr =
      let { Ast.Expression.New.callee = (_, callee); _ } = expr in
      begin
        match callee with
        | Ast.Expression.Identifier (annot, _) when this#annot_covers_target annot ->
          let annot = (this#loc_of_annot annot, this#type_from_enclosing_node annot) in
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
      if this#annot_covers_target annot then this#found_empty "jsx attribute literal";
      lit

    method! jsx_attribute_name_namespaced name =
      let (loc, _) = name in
      (* TODO: this should be supported *)
      if covers_target loc then this#found_empty "jsx attribute (namespaced)";
      name

    method! jsx_child child =
      let (loc, c) = child in
      match c with
      | Ast.JSX.Text _ when this#annot_covers_target loc -> this#found_empty "jsx text"
      | _ -> super#jsx_child child

    method! class_body cls_body =
      let open Ast.Class in
      let (_, { Body.body; comments = _ }) = cls_body in
      let new_available_private_names =
        let add_private_name (loc, { Ast.PrivateName.name; _ }) = SMap.add name loc in
        Base.List.fold body ~init:available_private_names ~f:(fun acc -> function
          | Body.Method (_, { Method.key = Ast.Expression.Object.Property.PrivateName name; _ })
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

    method! private_name ((loc, { Ast.PrivateName.name; comments = _ }) as pn) =
      if covers_target loc then
        match SMap.find_opt name available_private_names with
        | None -> this#found_empty "unbound private name"
        | Some l -> this#own_def l name
      else
        pn
  end

class typed_ast_searcher _cx ~typed_ast:_ ~is_legit_require ~covers_target ~purpose =
  object
    inherit [ALoc.t * Type.t] searcher _cx ~is_legit_require ~covers_target ~purpose

    method private loc_of_annot (loc, _) = loc

    method on_type_annot (x, y) = (x, y)

    method private remote_name_def_loc_of_import_named_specifier decl =
      let { Ast.Statement.ImportDeclaration.remote_name_def_loc; _ } = decl in
      remote_name_def_loc

    method private remote_default_name_def_loc_of_import_declaration (_loc, decl) =
      let open Ast.Statement.ImportDeclaration in
      let { default; _ } = decl in
      match default with
      | None -> None
      | Some { remote_default_name_def_loc = loc; _ } -> loc

    method private get_module_t (_, t) _ = t

    method private component_name_of_jsx_element _ expr =
      let open Ast.JSX in
      let { opening_element = (_, Opening.{ name; _ }); _ } = expr in
      annot_of_jsx_name name

    method private type_from_enclosing_node (_, t) = t
  end

module Statement = Fix_statement.Statement_

let find_remote_name_def_loc_in_node loc node =
  let exception Found of ALoc.t option in
  let visitor =
    object (_this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot loc = loc

      method on_type_annot loc = loc

      method! import_named_specifier ~import_kind specifier =
        let open Ast.Statement.ImportDeclaration in
        let { remote = ((loc', _), _); remote_name_def_loc; _ } = specifier in
        if loc' = loc then raise (Found remote_name_def_loc);
        super#import_named_specifier ~import_kind specifier
    end
  in
  try
    begin
      match node with
      | EnclosingProgram prog -> ignore (visitor#program prog)
      | EnclosingStatement stmt -> ignore (visitor#statement stmt)
      | EnclosingExpression expr -> ignore (visitor#expression expr)
    end;
    None
  with
  | Found t -> Some t

exception Internal_error_exn of internal_error

class on_demand_searcher cx ~is_legit_require ~covers_target ~purpose =
  object (this)
    inherit [ALoc.t] searcher cx ~is_legit_require ~covers_target ~purpose

    method on_type_annot x = x

    method loc_of_annot x = x

    method private remote_name_def_loc_of_import_named_specifier decl =
      let open Flow_ast.Statement.ImportDeclaration in
      let { remote = (remote_loc, _); _ } = decl in
      let node = this#enclosing_node in
      let typed_node = Typed_ast_finder.infer_node cx node in
      match find_remote_name_def_loc_in_node remote_loc typed_node with
      | None -> raise (Internal_error_exn Enclosing_node_error)
      | Some t -> t

    method private remote_default_name_def_loc_of_import_declaration (loc, decl) =
      let open Ast.Statement.ImportDeclaration in
      let stmt' = Statement.statement cx (loc, Flow_ast.Statement.ImportDeclaration decl) in
      (* NOTE: remote_default_name_def_loc field gets updated during inference *)
      match stmt' with
      | ( _,
          Ast.Statement.ImportDeclaration
            { default = Some { remote_default_name_def_loc = Some l; _ }; _ }
        ) ->
        Some l
      | _ -> None

    method private get_module_t loc source =
      let { Flow_ast.StringLiteral.value = module_name; _ } = source in
      Type_operation_utils.Import_export.get_module_t
        cx
        (loc, module_name)
        ~perform_platform_validation:false

    method private component_name_of_jsx_element loc expr =
      let open Ast.JSX in
      let typed_expr = Statement.expression cx (loc, Flow_ast.Expression.JSXElement expr) in
      match typed_expr with
      | (_, Flow_ast.Expression.JSXElement expr) ->
        let { opening_element = (_, Opening.{ name = component_name; _ }); _ } = expr in
        annot_of_jsx_name component_name
      | _ -> raise (Internal_error_exn On_demand_tast_error)

    method private type_from_enclosing_node loc =
      let node = this#enclosing_node in
      let typed_node = Typed_ast_finder.infer_node cx node in
      match find_type_annot_in_node loc typed_node with
      | None -> raise (Internal_error_exn Enclosing_node_error)
      | Some t -> t
  end

let search ~searcher ast =
  match searcher#program ast with
  | exception Found -> searcher#found_loc
  | exception Internal_error_exn err -> InternalError err
  | _ -> searcher#found_loc

let process_location cx ~ast ~typed_ast_opt ~is_legit_require ~purpose loc =
  match typed_ast_opt with
  | Some typed_ast ->
    let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
    let searcher = new typed_ast_searcher () ~typed_ast ~is_legit_require ~covers_target ~purpose in
    search ~searcher typed_ast
  | None ->
    (* Computing the aloc_ast here is temporary. When on-demand autocomplete is ready,
     * we can use the same aloc_ast used when computing the typed environment. *)
    let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
    let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
    let searcher = new on_demand_searcher cx ~is_legit_require ~covers_target ~purpose in
    search ~searcher aloc_ast
