(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Kind = Signature_builder_kind

type t = (Loc.t, Loc.t) Ast.Identifier.t * Kind.t

let rec pattern loc ?annot_path ?init_path (p : (Loc.t, Loc.t) Ast.Pattern.t) =
  let open Ast.Pattern in
  match p with
  | (_, Identifier { Identifier.name; annot; _ }) ->
    [
      ( name,
        ( loc,
          Kind.VariableDef
            { id = name; annot = Kind.Annot_path.mk_annot ?annot_path annot; init = init_path } ) );
    ]
  | (_, Object { Object.properties; annot }) ->
    Object.(
      let annot_path = Kind.Annot_path.mk_annot ?annot_path annot in
      List.fold_left
        (fun acc -> function
          | Property (prop_loc, { Property.key; pattern = p; _ }) ->
            begin
              match key with
              | Property.Identifier (key_loc, { Ast.Identifier.name = x; comments = _ }) ->
                let annot_path = Kind.Annot_path.mk_object prop_loc ?annot_path (key_loc, x) in
                let init_path = Kind.Init_path.mk_object prop_loc ?init_path (key_loc, x) in
                acc @ pattern loc ?annot_path ?init_path p
              | Property.Literal (key_loc, { Ast.Literal.raw; _ }) ->
                let annot_path = Kind.Annot_path.mk_object prop_loc ?annot_path (key_loc, raw) in
                let init_path = Kind.Init_path.mk_object prop_loc ?init_path (key_loc, raw) in
                acc @ pattern loc ?annot_path ?init_path p
              | Property.Computed _ -> acc @ pattern loc p
            end
          | RestElement (_, { RestElement.argument = p; comments = _ }) -> acc @ pattern loc p)
        []
        properties)
  | (_, Array { Array.elements; annot = _; comments = _ }) ->
    Array.(
      List.fold_left
        (fun acc -> function
          | None -> acc
          | Some (Element (_, { Element.argument = p; default = _ })) -> acc @ pattern loc p
          | Some (RestElement (_, { RestElement.argument = p; comments = _ })) ->
            acc @ pattern loc p)
        []
        elements)
  | (_, Expression _) -> []

(* TODO *)

let variable_declaration loc (decl : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
  let open Ast.Statement.VariableDeclaration in
  let { declarations; kind; comments = _ } = decl in
  List.fold_left
    (fun acc (_, { Declarator.id; init }) ->
      let init =
        match (kind, init) with
        | (Const, Some _) -> init
        | _ -> None
      in
      acc @ pattern loc ?init_path:(Kind.Init_path.mk_init init) id)
    []
    declarations

let function_declaration
    loc { Ast.Function.id; generator; async; tparams; params; return; body; predicate; _ } =
  ( Base.Option.value_exn id,
    (loc, Kind.FunctionDef { generator; async; tparams; params; return; body; predicate }) )

let function_expression
    loc { Ast.Function.id; generator; async; tparams; params; return; body; predicate; _ } =
  ( Base.Option.value_exn id,
    (loc, Kind.FunctionDef { generator; async; tparams; params; return; body; predicate }) )

let class_ loc class_ =
  let open Ast.Class in
  let { id; tparams; body; extends; implements; classDecorators = _; comments = _ } = class_ in
  let (super, super_targs) =
    match extends with
    | None -> (None, None)
    | Some (_, { Extends.expr; targs }) -> (Some expr, targs)
  in
  (Base.Option.value_exn id, (loc, Kind.ClassDef { tparams; body; super; super_targs; implements }))

let enum loc enum =
  let open Ast.Statement.EnumDeclaration in
  let { id; body } = enum in
  (id, (loc, Kind.EnumDef { body }))

let declare_variable loc declare_variable =
  let open Ast.Statement.DeclareVariable in
  let { id; annot } = declare_variable in
  (id, (loc, Kind.VariableDef { id; annot = Kind.Annot_path.mk_annot annot; init = None }))

let declare_function loc declare_function =
  let open Ast.Statement.DeclareFunction in
  let { id; annot; predicate; _ } = declare_function in
  (id, (loc, Kind.DeclareFunctionDef { annot; predicate }))

let declare_class loc declare_class =
  let open Ast.Statement.DeclareClass in
  let { id; tparams; body; extends; mixins; implements } = declare_class in
  (id, (loc, Kind.DeclareClassDef { tparams; body; extends; mixins; implements }))

let type_alias loc type_alias =
  let open Ast.Statement.TypeAlias in
  let { id; right; tparams; comments = _ } = type_alias in
  (id, (loc, Kind.TypeDef { tparams; right }))

let opaque_type loc opaque_type =
  let open Ast.Statement.OpaqueType in
  let { id; tparams; impltype; supertype; comments = _ } = opaque_type in
  (id, (loc, Kind.OpaqueTypeDef { tparams; impltype; supertype }))

let interface loc interface =
  let open Ast.Statement.Interface in
  let { id; tparams; body; extends; comments = _ } = interface in
  (id, (loc, Kind.InterfaceDef { tparams; body; extends }))

let import_star loc id kind source = (id, (loc, Kind.ImportStarDef { kind; source }))

let import_named loc id name kind source = (id, (loc, Kind.ImportNamedDef { kind; source; name }))

let require loc id ?name source = (id, (loc, Kind.RequireDef { source; name }))

let sketchy_toplevel loc id = (id, (loc, Kind.SketchyToplevelDef))
