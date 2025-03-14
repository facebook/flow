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
  | OwnNamedDef of 'loc * (* name *) string
  | OwnUnnamedDef of 'loc
  | ModuleDef of 'loc
  | ModuleTypeDef of (Type.t[@opaque])
  | Request of ('loc, 'loc * (Type.t[@opaque])) Get_def_request.t
  | Empty of string
  | LocNotFound
  | InternalError of internal_error
[@@deriving show]

let def_loc_of_reason r =
  match Reason.annot_loc_of_reason r with
  | Some aloc -> aloc
  | None -> Reason.def_loc_of_reason r

let def_loc_of_t t =
  let r = TypeUtil.reason_of_t t in
  def_loc_of_reason r

(* here lies the difference between "Go to Definition" and "Go to Type Definition":
   the former should stop on annot_loc (where the value was annotated), while the
   latter should jump to the def_loc (where the type was defined).

   for now, we only implement Go to Definition; if we want to do Go to Type
   Definition, it would ignore the annot loc. *)
let process_type_request =
  let rec loop cx seen =
    let open Type in
    function
    | OpenT (_, id) as t ->
      let root = Context.find_root_id cx id in
      if ISet.mem root seen then
        Ok (def_loc_of_t t)
      else (
        match Flow_js_utils.possible_types_of_type cx t with
        | [t'] -> loop cx (ISet.add root seen) t'
        | [] -> Error "No possible types"
        | _ :: _ -> Error "More than one possible type"
      )
    | t -> Ok (def_loc_of_t t)
  in
  (fun cx t -> loop cx ISet.empty t)

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

type 't require_declarator_info = {
  toplevel_pattern_annot: 't;
  require_t: 't;
}

class virtual ['T] searcher _cx ~is_local_use ~is_legit_require ~covers_target ~purpose =
  object (this)
    inherit [ALoc.t, 'T, ALoc.t, 'T] enclosing_node_mapper as super

    method virtual loc_of_annot : 'T -> ALoc.t

    method private annot_covers_target annot = covers_target (this#loc_of_annot annot)

    method private is_legit_require annot = is_legit_require (this#loc_of_annot annot)

    val mutable require_declarator_info = None

    val mutable available_private_names : ALoc.t SMap.t = SMap.empty

    val mutable found_loc_ = LocNotFound

    method found_loc = found_loc_

    method private with_require_toplevel_pattern_info ~info f =
      let saved_require_declarator_info = require_declarator_info in
      require_declarator_info <- info;
      let result = f () in
      require_declarator_info <- saved_require_declarator_info;
      result

    method private on_loc_annot x = x

    method private own_named_def : 'a. ALoc.t -> string -> 'a =
      fun loc name ->
        found_loc_ <- OwnNamedDef (loc, name);
        raise Found

    method private own_unnamed_def : 'a. ALoc.t -> 'a =
      fun loc ->
        found_loc_ <- OwnUnnamedDef loc;
        raise Found

    method private module_def : 'a. ALoc.t -> 'a =
      fun l ->
        found_loc_ <- ModuleDef l;
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

    method virtual private get_module_def_loc : 'T -> string -> ALoc.t

    method virtual private remote_name_def_loc_of_import_named_specifier
        : (ALoc.t, 'T) Ast.Statement.ImportDeclaration.named_specifier -> ALoc.t option

    method virtual private imported_name_def_loc_of_export_named_declaration_specifier
        : (ALoc.t, 'T) Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t -> ALoc.t option

    method virtual private remote_default_name_def_loc_of_import_declaration
        : ALoc.t * (ALoc.t, 'T) Ast.Statement.ImportDeclaration.t -> ALoc.t option

    method virtual private component_name_of_jsx_element
        : 'T -> (ALoc.t, 'T) Ast.JSX.element -> ALoc.t * Type.t

    method private module_def_for_entire_module_related_id ~module_def_f ~id =
      let (name_annot, { Ast.Identifier.name; _ }) = id in
      if this#annot_covers_target name_annot then
        match purpose with
        | Get_def_types.Purpose.GoToDefinition
        | Get_def_types.Purpose.JSDoc ->
          module_def_f ()
        | Get_def_types.Purpose.FindReferences ->
          this#own_named_def (this#loc_of_annot name_annot) name

    method! variable_declarator
        ~kind ((_, { Ast.Statement.VariableDeclaration.Declarator.id; init }) as x) =
      (* If a variable declarator's initializer contains `require()`, then we want to jump
         through it into the imported module. To do this, we set the `in_require_declarator`
         flag, which we use when we visit the id, in lieu of parent pointers. *)
      let (id_annot, _) = id in
      let info =
        match init with
        | Some init
          when is_require ~is_legit_require:this#is_legit_require init
               && this#annot_covers_target id_annot ->
          Some { toplevel_pattern_annot = id_annot; require_t = fst init }
        | _ -> None
      in
      this#with_require_toplevel_pattern_info ~info (fun () -> super#variable_declarator ~kind x)

    method! import_source source_annot ({ Ast.StringLiteral.value = module_name; _ } as lit) =
      if this#annot_covers_target source_annot then begin
        this#module_def (this#get_module_def_loc source_annot module_name)
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
        | Some l -> this#own_named_def l name
        | None -> this#own_named_def (this#loc_of_annot remote_annot) "default"
      );
      Base.Option.iter local ~f:(fun (local_annot, _) ->
          if this#annot_covers_target local_annot then
            match this#remote_name_def_loc_of_import_named_specifier decl with
            | Some l -> this#own_named_def l name
            | None -> this#own_named_def (this#loc_of_annot local_annot) "default"
      );
      decl

    method! export_named_declaration loc decl =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind = _; source; specifiers; declaration = _; comments = _ } = decl in
      (match (source, specifiers) with
      | ( Some (source_annot, { Ast.StringLiteral.value = module_name; _ }),
          Some (ExportBatchSpecifier (_, Some id))
        ) ->
        this#module_def_for_entire_module_related_id
          ~module_def_f:(fun () ->
            this#module_def (this#get_module_def_loc source_annot module_name))
          ~id
      | _ -> ());
      super#export_named_declaration loc decl

    method! export_named_declaration_specifier spec =
      let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (_, { local; exported; from_remote; imported_name_def_loc = _ }) = spec in
      Base.Option.iter exported ~f:(fun (annot, { Ast.Identifier.name; _ }) ->
          if this#annot_covers_target annot then
            (* Either `export {foo as bar}` or `export {foo as bar} from '...'`
             * In both case, get-def on bar should jump to itself *)
            this#own_named_def (this#loc_of_annot annot) name
      );
      let (local_annot, { Ast.Identifier.name = local_name; _ }) = local in
      if this#annot_covers_target local_annot then
        if from_remote then
          match this#imported_name_def_loc_of_export_named_declaration_specifier spec with
          | Some l ->
            (* When we have imported_name_def_loc, we must be in the case of
             * `export {foo [as bar]?} from '...'.
             * In this case we should jump to the remote def loc stored in typed AST *)
            this#own_named_def l local_name
          | None ->
            (* When we don't have have imported_name_def_loc, then there is a type error.
             * Similar to how we handle imported names with type error, we make it jump to itself. *)
            this#own_named_def (this#loc_of_annot local_annot) local_name
        else
          (* Given `export {foo}`, we should use the usual
           * use-def analysis result from scope builder. *)
          ignore
          @@ this#request
               (Get_def_request.Identifier
                  { name = local_name; loc = this#loc_of_annot local_annot }
               );
      spec

    method! import_declaration loc decl =
      let open Ast.Statement.ImportDeclaration in
      let {
        default;
        specifiers;
        source = (source_annot, { Ast.StringLiteral.value = module_name; _ });
        _;
      } =
        decl
      in
      Base.Option.iter default ~f:(fun { identifier = (annot, _); _ } ->
          if this#annot_covers_target annot then
            match this#remote_default_name_def_loc_of_import_declaration (loc, decl) with
            | Some l -> this#own_named_def l "default"
            | None -> this#own_named_def (this#loc_of_annot annot) "default"
      );
      Base.Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers _ -> ()
          | ImportNamespaceSpecifier (_, id) ->
            this#module_def_for_entire_module_related_id
              ~module_def_f:(fun () ->
                this#module_def (this#get_module_def_loc source_annot module_name))
              ~id
          );
      super#import_declaration loc decl

    method! export_source source_annot ({ Ast.StringLiteral.value = module_name; _ } as lit) =
      if this#annot_covers_target source_annot then begin
        this#module_def (this#get_module_def_loc source_annot module_name)
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

    method! match_member_pattern member_pattern =
      let open Flow_ast.MatchPattern.MemberPattern in
      let (_, { base; property; _ }) = member_pattern in
      begin
        match property with
        | PropertyIdentifier (annot, { Ast.Identifier.name; _ }) when this#annot_covers_target annot
          ->
          let base_annot =
            match base with
            | BaseIdentifier (annot, _)
            | BaseMember (annot, _) ->
              annot
          in
          let base_annot =
            (this#loc_of_annot base_annot, this#type_from_enclosing_node base_annot)
          in
          let result =
            Get_def_request.(
              Member { prop_name = name; object_type = base_annot; force_instance = false }
            )
          in
          this#request result
        | _ -> ()
      end;
      super#match_member_pattern member_pattern

    method! generic_type expr =
      let open Ast.Type.Generic in
      let { id; targs; comments = _ } = expr in
      begin
        match (id, targs) with
        | ( Identifier.Unqualified (pick_annot, { Ast.Identifier.name = "Pick"; comments = _ }),
            Some (_, { Ast.Type.TypeArgs.arguments = [(obj_annot, _); keys]; _ })
          )
          when not (is_local_use (this#loc_of_annot pick_annot)) ->
          let request annot prop_name =
            if this#annot_covers_target annot then
              let obj_annot =
                (this#loc_of_annot obj_annot, this#type_from_enclosing_node obj_annot)
              in
              let result =
                Get_def_request.(
                  Member { prop_name; object_type = obj_annot; force_instance = false }
                )
              in
              this#request result
          in
          (match keys with
          | (annot, Ast.Type.StringLiteral { Ast.StringLiteral.value; _ }) -> request annot value
          | (_, Ast.Type.Union { Ast.Type.Union.types = (t1, t2, ts); _ }) ->
            Base.List.iter (t1 :: t2 :: ts) ~f:(function
                | (annot, Ast.Type.StringLiteral { Ast.StringLiteral.value; _ }) ->
                  request annot value
                | _ -> ()
                )
          | _ -> ())
        | _ -> ()
      end;
      super#generic_type expr

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
        this#request (Get_def_request.Identifier { name; loc = this#loc_of_annot loc })
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
        if name = String.capitalize_ascii name then
          this#request (Get_def_request.Identifier { name; loc = this#loc_of_annot annot })
        else
          this#found_empty "jsx intrinsic element"
      end;
      super#jsx_element_name_identifier id

    method! jsx_element_name_namespaced ns =
      let (loc, _) = ns in
      (* TODO: this should be supported *)
      if covers_target loc then this#found_empty "jsx element (namespaced)";
      super#jsx_element_name_namespaced ns

    method! jsx_member_expression_identifier
        ((annot, { Ast.JSX.Identifier.name; comments = _ }) as id) =
      if this#annot_covers_target annot then begin
        this#request (Get_def_request.Identifier { name; loc = this#loc_of_annot annot })
      end;
      super#jsx_member_expression_identifier id

    method! jsx_member_expression expr =
      let ( _,
            {
              Ast.JSX.MemberExpression._object;
              property = (property_annot, { Ast.JSX.Identifier.name = prop_name; _ });
            }
          ) =
        expr
      in
      ( if this#annot_covers_target property_annot then
        match _object with
        | Ast.JSX.MemberExpression.Identifier (obj_annot, _)
        | Ast.JSX.MemberExpression.MemberExpression
            (_, { Ast.JSX.MemberExpression.property = (obj_annot, _); _ }) ->
          let obj_annot = (this#loc_of_annot obj_annot, this#type_from_enclosing_node obj_annot) in
          let result =
            Get_def_request.(Member { prop_name; object_type = obj_annot; force_instance = false })
          in
          this#request result
      );
      super#jsx_member_expression expr

    method! pattern ?kind ((pat_annot, p) as pat) =
      let open Ast.Pattern in
      (* In const {foo: bar} = require('some_module'); foo and bar should jump to prop def of foo,
         while in other cases, bar should be its own definition. *)
      let is_id_pattern_of_obj_key_in_require_declarator = function
        | (id_annot, Identifier _) ->
          Base.Option.is_some require_declarator_info && this#annot_covers_target id_annot
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
        | Identifier { Identifier.name = id; _ } ->
          (match require_declarator_info with
          | Some { toplevel_pattern_annot; require_t } when toplevel_pattern_annot = pat_annot ->
            this#module_def_for_entire_module_related_id
              ~module_def_f:(fun () ->
                found_loc_ <- ModuleTypeDef (this#type_from_enclosing_node require_t);
                raise Found)
              ~id
          | _ -> ())
        | _ -> ()
      in
      super#pattern ?kind pat

    method! pattern_identifier ?kind (annot, ({ Ast.Identifier.name; comments = _ } as name_node)) =
      if kind != None && this#annot_covers_target annot then
        this#own_named_def (this#loc_of_annot annot) name;
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
                  {
                    ArgList.arguments =
                      [Expression (source_annot, (StringLiteral _ | TemplateLiteral _))];
                    comments = _;
                  }
                );
              _;
            }
          when this#is_legit_require source_annot ->
          found_loc_ <- ModuleTypeDef (this#type_from_enclosing_node source_annot);
          raise Found
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
      if covers_target loc then this#own_named_def loc name;
      id

    method! module_ref_literal mref =
      let { Ast.ModuleRefLiteral.require_loc; def_loc_opt; _ } = mref in
      if covers_target require_loc then
        this#own_unnamed_def (Base.Option.value ~default:require_loc def_loc_opt)
      else
        super#module_ref_literal mref

    method! enum_member_identifier id =
      let (loc, { Ast.Identifier.name; comments = _ }) = id in
      if covers_target loc then this#own_named_def loc name;
      super#enum_member_identifier id

    (* object keys would normally hit this#t_identifier; this circumvents that. *)
    method! object_key_identifier id =
      let (annot, { Ast.Identifier.name; comments = _ }) = id in
      if this#annot_covers_target annot then this#own_named_def (this#loc_of_annot annot) name;
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

    method! match_object_pattern_property_key key =
      let open Ast.MatchPattern.ObjectPattern.Property in
      (match key with
      | StringLiteral (loc, _)
      | NumberLiteral (loc, _)
      | BigIntLiteral (loc, _) ->
        if covers_target loc then this#found_empty "match pattern object key (literal)"
      | Identifier (annot, { Ast.Identifier.name; comments = _ }) ->
        if this#annot_covers_target annot then this#own_named_def (this#loc_of_annot annot) name);
      super#match_object_pattern_property_key key

    (* for object properties using the shorthand {variableName} syntax,
     * process the value before the key so that the explicit-non-find in this#object_key_identifier
     * doesn't make us miss the variable *)
    method! object_property prop =
      let open Ast.Expression.Object.Property in
      (match prop with
      | (_, Init { shorthand = true; value; _ }) -> ignore (this#expression value)
      | _ -> ());
      super#object_property prop

    method! new_ annot expr =
      let { Ast.Expression.New.callee = (_, callee); _ } = expr in
      begin
        match callee with
        | Ast.Expression.Identifier (annot, _)
          when this#annot_covers_target annot && purpose = Get_def_types.Purpose.JSDoc ->
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
      super#new_ annot expr

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
        | Some l -> this#own_named_def l name
      else
        pn

    (* If shorthand syntax (e.g. `const foo`), don't map over the 'key' as its
       location also covers the binding name. *)
    method! match_object_pattern_property prop =
      let open Ast.MatchPattern.ObjectPattern.Property in
      match prop with
      | (loc, Valid { key; pattern; shorthand; comments }) ->
        let key =
          if shorthand then
            key
          else
            this#match_object_pattern_property_key key
        in
        let pattern = this#match_pattern pattern in
        let comments = this#syntax_opt comments in
        (this#on_loc_annot loc, Valid { key; pattern; shorthand; comments })
      | (_, InvalidShorthand _) -> prop
  end

class typed_ast_searcher cx ~typed_ast:_ ~is_local_use ~is_legit_require ~covers_target ~purpose =
  object
    inherit [ALoc.t * Type.t] searcher cx ~is_local_use ~is_legit_require ~covers_target ~purpose

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

    method private imported_name_def_loc_of_export_named_declaration_specifier
        (_, { Ast.Statement.ExportNamedDeclaration.ExportSpecifier.imported_name_def_loc; _ }) =
      imported_name_def_loc

    method private get_module_def_loc (loc, _) module_name =
      match
        Flow_js_utils.ImportExportUtils.get_module_type_or_any
          cx
          (loc, module_name)
          ~perform_platform_validation:false
          ~import_kind_for_untyped_import_validation:None
      with
      | Ok m -> def_loc_of_reason m.Type.module_reason
      | Error t -> def_loc_of_t t

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

let find_imported_name_def_loc_in_node local_loc node =
  let exception Found of ALoc.t option in
  let visitor =
    object (_this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot loc = loc

      method on_type_annot loc = loc

      method! export_named_declaration_specifier spec =
        let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
        let ( _,
              { local = ((local_loc', _), _); exported = _; from_remote = _; imported_name_def_loc }
            ) =
          spec
        in
        if local_loc = local_loc' then raise (Found imported_name_def_loc);
        super#export_named_declaration_specifier spec
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

class on_demand_searcher cx ~is_local_use ~is_legit_require ~covers_target ~purpose =
  object (this)
    inherit [ALoc.t] searcher cx ~is_local_use ~is_legit_require ~covers_target ~purpose

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

    method private imported_name_def_loc_of_export_named_declaration_specifier spec =
      let (_, { Ast.Statement.ExportNamedDeclaration.ExportSpecifier.local = (local_loc, _); _ }) =
        spec
      in
      let node = this#enclosing_node in
      let typed_node = Typed_ast_finder.infer_node cx node in
      match find_imported_name_def_loc_in_node local_loc typed_node with
      | None -> raise (Internal_error_exn Enclosing_node_error)
      | Some t -> t

    method private get_module_def_loc loc module_name =
      match
        Flow_js_utils.ImportExportUtils.get_module_type_or_any
          cx
          (loc, module_name)
          ~perform_platform_validation:false
          ~import_kind_for_untyped_import_validation:None
      with
      | Ok m -> def_loc_of_reason m.Type.module_reason
      | Error t -> def_loc_of_t t

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

let process_location cx ~available_ast ~is_local_use ~is_legit_require ~purpose loc =
  match available_ast with
  | Typed_ast_utils.Typed_ast typed_ast ->
    let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
    let searcher =
      new typed_ast_searcher cx ~typed_ast ~is_local_use ~is_legit_require ~covers_target ~purpose
    in
    search ~searcher typed_ast
  | Typed_ast_utils.ALoc_ast aloc_ast ->
    let covers_target test_loc = Reason.in_range loc (ALoc.to_loc_exn test_loc) in
    let searcher =
      new on_demand_searcher cx ~is_local_use ~is_legit_require ~covers_target ~purpose
    in
    search ~searcher aloc_ast
