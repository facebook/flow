(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_utils

let loc_of_object_key =
  let open Ast.Expression.Object.Property in
  function
  | Identifier (loc, _)
  | StringLiteral (loc, _)
  | NumberLiteral (loc, _)
  | BigIntLiteral (loc, _)
  | Computed (_, Ast.ComputedKey.{ expression = (loc, _); _ })
  | PrivateName (loc, _) ->
    loc

let comments_of_variance =
  let open Ast.Variance in
  Base.Option.bind ~f:(fun (_, { comments; _ }) -> comments)

let comments_of_object_key =
  let open Ast.Expression.Object.Property in
  function
  | Identifier (_, Ast.Identifier.{ comments; _ })
  | StringLiteral (_, Ast.StringLiteral.{ comments; _ })
  | NumberLiteral (_, Ast.NumberLiteral.{ comments; _ })
  | BigIntLiteral (_, Ast.BigIntLiteral.{ comments; _ })
  | Computed (_, Ast.ComputedKey.{ comments; _ }) ->
    comments
  | PrivateName _ -> None

(* used to forward the comments on an export statement to the declaration
   contained in the export statement. That's why we don't bother with all
   cases; only statements that can appear in export declarations. *)
let replace_comments_of_statement ~comments =
  let open Ast.Statement in
  Utils_js.map_snd (function
      | TypeAlias x -> TypeAlias TypeAlias.{ x with comments }
      | OpaqueType x -> OpaqueType OpaqueType.{ x with comments }
      | InterfaceDeclaration x -> InterfaceDeclaration Interface.{ x with comments }
      | VariableDeclaration x -> VariableDeclaration VariableDeclaration.{ x with comments }
      | ComponentDeclaration x ->
        ComponentDeclaration Ast.Statement.ComponentDeclaration.{ x with comments }
      | ClassDeclaration x -> ClassDeclaration Ast.Class.{ x with comments }
      | FunctionDeclaration x -> FunctionDeclaration Ast.Function.{ x with comments }
      | EnumDeclaration x -> EnumDeclaration EnumDeclaration.{ x with comments }
      | DeclareVariable x -> DeclareVariable DeclareVariable.{ x with comments }
      | DeclareFunction x -> DeclareFunction DeclareFunction.{ x with comments }
      | DeclareClass x -> DeclareClass DeclareClass.{ x with comments }
      | DeclareComponent x -> DeclareComponent DeclareComponent.{ x with comments }
      | DeclareTypeAlias x -> DeclareTypeAlias TypeAlias.{ x with comments }
      | DeclareOpaqueType x -> DeclareOpaqueType OpaqueType.{ x with comments }
      | DeclareInterface x -> DeclareInterface Interface.{ x with comments }
      | DeclareEnum x -> DeclareEnum EnumDeclaration.{ x with comments }
      | ( Block _ | Break _ | Continue _ | Debugger _ | DeclareExportDeclaration _ | DeclareModule _
        | DeclareModuleExports _ | DeclareNamespace _ | DoWhile _ | Empty _
        | ExportDefaultDeclaration _ | ExportNamedDeclaration _ | Expression _ | For _ | ForIn _
        | ForOf _ | If _ | ImportDeclaration _ | Labeled _ | Return _ | Switch _ | Throw _ | Try _
        | While _ | With _ ) as x ->
        x
      )

class jsdoc_documentation_searcher find =
  object (this)
    inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

    method! variable_declaration stmt_loc decl =
      let open Ast.Statement.VariableDeclaration in
      let { declarations; comments; _ } = decl in
      Base.List.iter declarations ~f:(function
          | ( _,
              Declarator.
                {
                  id = (_, Ast.Pattern.(Identifier Identifier.{ name = (id_loc, _); annot; _ }));
                  init;
                  _;
                }
            ) ->
            find id_loc comments;
            find (loc_of_annotation_or_hint annot) comments;
            Base.Option.iter init ~f:(fun (init_loc, _) -> find init_loc comments)
          | _ -> ()
          );
      super#variable_declaration stmt_loc decl

    method! class_ stmt_loc cls =
      let open Ast.Class in
      let { id; comments; _ } = cls in
      Base.Option.iter id ~f:(fun (loc, _) -> find loc comments);
      super#class_ stmt_loc cls

    method! function_ loc func =
      let open Ast.Function in
      let { comments; id; sig_loc; _ } = func in
      find loc comments;
      find sig_loc comments;
      Base.Option.iter id ~f:(fun (id_loc, _) -> find id_loc comments);
      super#function_ loc func

    method! component_declaration loc c =
      let open Ast.Statement.ComponentDeclaration in
      let { comments; id = (id_loc, _); sig_loc; _ } = c in
      find loc comments;
      find sig_loc comments;
      find id_loc comments;
      super#component_declaration loc c

    method! declare_variable stmt_loc decl =
      let open Ast.Statement.DeclareVariable in
      let { id = (loc, _); comments; _ } = decl in
      find loc comments;
      super#declare_variable stmt_loc decl

    method! declare_class stmt_loc decl =
      let open Ast.Statement.DeclareClass in
      let { id = (loc, _); comments; _ } = decl in
      find loc comments;
      super#declare_class stmt_loc decl

    method! declare_function stmt_loc decl =
      let open Ast.Statement.DeclareFunction in
      let { id = (id_loc, _); annot = (annot_loc, _); comments; _ } = decl in
      find id_loc comments;
      find annot_loc comments;
      super#declare_function stmt_loc decl

    method! declare_export_declaration loc decl =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default; source = _; specifiers = _; declaration; comments } = decl in
      Base.Option.iter default ~f:(fun l -> find l comments);
      let () =
        let open Utils_js in
        Base.Option.iter declaration ~f:(function
            | Variable (loc, d) ->
              (loc, Ast.Statement.DeclareVariable d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | Function (loc, d) ->
              (loc, Ast.Statement.DeclareFunction d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | Class (loc, d) ->
              (loc, Ast.Statement.DeclareClass d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | Component (loc, d) ->
              (loc, Ast.Statement.DeclareComponent d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | DefaultType ((loc, _) as t) ->
              find loc comments;
              ignore @@ this#type_ t
            | NamedType (loc, d) ->
              (loc, Ast.Statement.DeclareTypeAlias d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | NamedOpaqueType (loc, d) ->
              (loc, Ast.Statement.DeclareOpaqueType d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | Interface (loc, d) ->
              (loc, Ast.Statement.DeclareInterface d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            | Enum (loc, d) ->
              (loc, Ast.Statement.DeclareEnum d)
              |> replace_comments_of_statement ~comments %> this#statement %> ignore
            )
      in
      super#declare_export_declaration loc decl

    method! object_property_type prop_type =
      let open Ast.Type.Object.Property in
      let (_, { key; value; comments; variance; _ }) = prop_type in
      let value_loc =
        match value with
        | Init (value_loc, _)
        | Get (_, Ast.Type.Function.{ return = TypeAnnotation (value_loc, _); _ })
        | Get (_, Ast.Type.Function.{ return = TypeGuard (value_loc, _); _ })
        | Set (value_loc, _) ->
          value_loc
      in
      let key_loc = loc_of_object_key key in
      find key_loc comments;
      find value_loc comments;
      let variance_comments = comments_of_variance variance in
      find key_loc variance_comments;
      find value_loc variance_comments;
      let key_comments = comments_of_object_key key in
      find key_loc key_comments;
      find value_loc key_comments;
      super#object_property_type prop_type

    method! class_method method_loc meth =
      let open Ast.Class.Method in
      let { key; comments; _ } = meth in
      let key_loc = loc_of_object_key key in
      find key_loc comments;
      find key_loc (comments_of_object_key key);
      super#class_method method_loc meth

    method! class_property prop_loc prop =
      let open Ast.Class.Property in
      let { key; variance; comments; _ } = prop in
      let key_loc = loc_of_object_key key in
      find key_loc comments;
      find key_loc (comments_of_variance variance);
      find key_loc (comments_of_object_key key);
      super#class_property prop_loc prop

    method! object_property prop =
      let open Ast.Expression.Object.Property in
      let (locs, comments) =
        match prop with
        | (_, Init { key; value = (value_loc, _); _ }) ->
          ([loc_of_object_key key; value_loc], [comments_of_object_key key])
        | (prop_loc, Method { key; value = (_, Ast.Function.{ comments; _ }) }) ->
          ([prop_loc; loc_of_object_key key], [comments_of_object_key key; comments])
        | (_, Get { key; value = (_, Ast.Function.{ return; _ }); comments }) ->
          ([loc_of_object_key key; loc_of_return_annot return], [comments])
        | (_, Set _) -> ([], [])
      in
      Base.List.iter locs ~f:(fun loc ->
          Base.List.iter comments ~f:(fun comment -> find loc comment)
      );
      super#object_property prop

    method! enum_declaration loc enum =
      let open Ast.Statement.EnumDeclaration in
      let { comments; id = (id_loc, _); _ } = enum in
      find loc comments;
      find id_loc comments;
      super#enum_declaration loc enum

    method! enum_defaulted_member member =
      let open Ast.Statement.EnumDeclaration.DefaultedMember in
      let (loc, { id = (id_loc, Ast.Identifier.{ comments; _ }) }) = member in
      find loc comments;
      find id_loc comments;
      member

    method enum_initialized_member
        : 'a.
          ('a, Loc.t) Ast.Statement.EnumDeclaration.InitializedMember.t ->
          ('a, Loc.t) Ast.Statement.EnumDeclaration.InitializedMember.t =
      fun member ->
        let open Ast.Statement.EnumDeclaration.InitializedMember in
        let (loc, { id = (id_loc, Ast.Identifier.{ comments; _ }); _ }) = member in
        find loc comments;
        find id_loc comments;
        member

    method! enum_boolean_member member = this#enum_initialized_member member

    method! enum_number_member member = this#enum_initialized_member member

    method! enum_string_member member = this#enum_initialized_member member

    method! export_named_declaration loc decl =
      let open Ast.Statement.ExportNamedDeclaration in
      let { declaration; comments; _ } = decl in
      find loc comments;
      Base.Option.iter
        declaration
        ~f:Utils_js.(replace_comments_of_statement ~comments %> this#statement %> ignore);
      super#export_named_declaration loc decl

    method! export_default_declaration loc decl =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; declaration; comments; _ } = decl in
      find default comments;
      (let open Ast.Expression in
      match declaration with
      | Declaration stmt ->
        stmt |> replace_comments_of_statement ~comments |> this#statement |> ignore
      | Expression
          ( _,
            ( TypeCast TypeCast.{ annot = (_, (loc, _)); _ }
            | AsExpression AsExpression.{ annot = (_, (loc, _)); _ } )
          )
      | Expression (loc, _) ->
        find loc comments
      );
      super#export_default_declaration loc decl

    method! type_alias loc type_alias =
      let open Ast.Statement.TypeAlias in
      let { id = (id_loc, _); comments; _ } = type_alias in
      find loc comments;
      find id_loc comments;
      super#type_alias loc type_alias

    method! opaque_type loc opaque_type =
      let open Ast.Statement.OpaqueType in
      let { id = (id_loc, _); comments; _ } = opaque_type in
      find id_loc comments;
      super#opaque_type loc opaque_type

    method! interface loc interface =
      let open Ast.Statement.Interface in
      let { id = (id_loc, _); comments; _ } = interface in
      find loc comments;
      find id_loc comments;
      super#interface loc interface
  end

exception FoundJsdoc of Jsdoc.t

let find_jsdoc target_loc found_loc comments =
  if Loc.equal target_loc found_loc then
    Base.Option.iter (Jsdoc.of_comments comments) ~f:(fun jsdoc -> raise (FoundJsdoc jsdoc))

let search_jsdoc def_loc ast =
  let searcher = new jsdoc_documentation_searcher (find_jsdoc def_loc) in
  try
    ignore (searcher#program ast);
    None
  with
  | FoundJsdoc documentation -> Some documentation

let jsdoc_of_getdef_loc ~ast ~reader def_loc =
  let open Base.Option.Let_syntax in
  let%bind source = Loc.source def_loc in
  let current_ast_if_should_use =
    let (current_file_loc, _) = ast in
    let%bind current_file_source = Loc.source current_file_loc in
    if source = current_file_source then
      Some ast
    else
      None
  in
  let%bind ast =
    match current_ast_if_should_use with
    | Some _ as some_ast -> some_ast
    | None -> Parsing_heaps.Reader.get_ast ~reader source
  in
  search_jsdoc def_loc ast

let documentation_of_jsdoc jsdoc =
  let documentation_of_unrecognized_tag (tag_name, tag_description) =
    let tag_name_documentation = Printf.sprintf "**@%s**" tag_name in
    match tag_description with
    | None -> tag_name_documentation
    | Some tag_description -> Printf.sprintf "%s %s" tag_name_documentation tag_description
  in
  let documentation_strings =
    Base.Option.fold
      (Jsdoc.description jsdoc)
      ~f:(fun unrecognized_tag_documentations description ->
        description :: unrecognized_tag_documentations)
      ~init:(Base.List.map ~f:documentation_of_unrecognized_tag (Jsdoc.unrecognized_tags jsdoc))
  in
  let documentation_strings =
    Base.Option.fold
      (Jsdoc.deprecated jsdoc)
      ~f:(fun acc description ->
        documentation_of_unrecognized_tag ("deprecated", Some description) :: acc)
      ~init:documentation_strings
  in
  match documentation_strings with
  | [] -> None
  | _ -> Some (String.concat "\n\n" documentation_strings)

class hardcoded_documentation_searcher find =
  object (_this)
    inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

    method! render_type t =
      let open Ast.Type.Renders in
      let { operator_loc; variant; _ } = t in
      let doc =
        match variant with
        | Normal ->
          "`renders A` means that it will eventually render exactly one React element `A`."
        | Maybe ->
          "`renders? A` means that it will eventually render zero or one React element `A`."
        | Star -> "`renders* A` means that it will eventually render any amount of `A`."
      in
      find operator_loc doc;
      super#render_type t
  end

exception FoundHardcodedDocumentation of string

let hardcoded_documentation_at_loc ast target_loc =
  let find loc doc = if Loc.contains loc target_loc then raise (FoundHardcodedDocumentation doc) in
  let searcher = new hardcoded_documentation_searcher find in
  try
    ignore (searcher#program ast);
    None
  with
  | FoundHardcodedDocumentation doc -> Some doc

let def_loc_to_comment_loc_map ast =
  let map_ref = ref Loc_sig.LocS.LMap.empty in
  let add_to_map def_loc =
    Base.Option.iter ~f:(fun Ast.Syntax.{ leading; _ } ->
        Base.Option.iter (Base.List.last leading) ~f:(fun (comment_loc, _) ->
            map_ref := Loc_sig.LocS.LMap.add ~combine:Base.Fn.const def_loc comment_loc !map_ref
        )
    )
  in
  let searcher = new jsdoc_documentation_searcher add_to_map in
  ignore (searcher#program ast);
  !map_ref

let module_doc_loc (_, Ast.Program.{ comments; _ }) : Loc.t option =
  match comments with
  | Some Ast.Syntax.{ leading = (comment_loc, _) :: _; _ } -> Some comment_loc
  | _ -> None
