(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module Kind = Signature_builder_kind

type t = Loc.t Ast.Identifier.t * Kind.t

let rec pattern ?annot_path init (p: (Loc.t, Loc.t) Ast.Pattern.t) =
  let open Ast.Pattern in
  begin match p with
    | _, Identifier { Identifier.name; annot; _ } ->
      [name, Kind.VariableDef { annot = Kind.Annot_path.mk_annot ?annot_path annot; init }]
    | _, Object { Object.properties; annot } ->
      let open Object in
      let annot_path = Kind.Annot_path.mk_annot ?annot_path annot in
      List.fold_left (fun acc -> function
        | Property (_, { Property.key; pattern = p; _ }) ->
          begin match key with
            | Property.Identifier (_, x) ->
              let annot_path = Kind.Annot_path.mk_object ?annot_path x in
              acc @ (pattern ?annot_path init p)
            | Property.Literal (_, { Ast.Literal.raw; _ }) ->
              let annot_path = Kind.Annot_path.mk_object ?annot_path raw in
              acc @ (pattern ?annot_path init p)
            | Property.Computed _ ->
              acc @ (pattern init p)
          end
        | RestProperty (_, { RestProperty.argument = p }) ->
          acc @ (pattern init p)
      ) [] properties
    | _, Array { Array.elements; annot } ->
      let open Array in
      let annot_path = Kind.Annot_path.mk_annot ?annot_path annot in
      fst @@ List.fold_left (fun (acc, i) -> function
        | None -> acc, i+1
        | Some (Element p) ->
          let annot_path = Kind.Annot_path.mk_array ?annot_path i in
          acc @ (pattern ?annot_path init p), i+1
        | Some (RestElement (_, { RestElement.argument = p })) ->
          acc @ (pattern init p), i+1
      ) ([], 0) elements
    | _, Assignment { Assignment.left; _ } -> pattern ?annot_path init left
    | _, Expression _ -> [] (* TODO *)
  end

let variable_declaration (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
  let open Ast.Statement.VariableDeclaration in
  let { declarations; kind } = decl in
  List.fold_left (fun acc (_, { Declarator.id; init }) ->
    let init = match kind, init with
      | Const, Some _ -> init
      | _ -> None in
    acc @ (pattern init id)
  ) [] declarations

let function_declaration function_declaration =
  let open Ast.Function in
  let { id; generator; tparams; params; return; body; _ } = function_declaration in
  id, Kind.FunctionDef { generator; tparams; params; return; body }

let class_ class_ =
  let open Ast.Class in
  let {
    id; tparams; body; extends; implements;
    classDecorators = _;
  } = class_ in
  let super, super_targs = match extends with
  | None -> None, None
  | Some (_, { Extends.expr; targs; }) -> Some expr, targs in
  id, Kind.ClassDef { tparams; body; super; super_targs; implements }

let declare_variable declare_variable =
  let open Ast.Statement.DeclareVariable in
  let { id; annot } = declare_variable in
  id, Kind.VariableDef { annot = Kind.Annot_path.mk_annot annot; init = None }

let declare_function declare_function =
  let open Ast.Statement.DeclareFunction in
  let { id; annot; _ } = declare_function in
  id, Kind.DeclareFunctionDef { annot }

let declare_class declare_class =
  let open Ast.Statement.DeclareClass in
  let {
    id; tparams; body = (_, body); extends; mixins; implements
  } = declare_class in
  id, Kind.DeclareClassDef { tparams; body; extends; mixins; implements }

let type_alias type_alias =
  let open Ast.Statement.TypeAlias in
  let { id; right; tparams } = type_alias in
  id, Kind.TypeDef { tparams; right }

let opaque_type opaque_type =
  let open Ast.Statement.OpaqueType in
  let { id; tparams; impltype; supertype; _ } = opaque_type in
  id, Kind.OpaqueTypeDef { tparams; impltype; supertype }

let interface interface =
  let open Ast.Statement.Interface in
  let {
    id; tparams; body = (_, body); extends
  } = interface in
  id, Kind.InterfaceDef { tparams; body; extends }

let import_star id kind source =
  id, Kind.ImportStarDef { kind; source }

let import_named id name kind source =
  id, Kind.ImportNamedDef { kind; source; name }

let require id source =
  id, Kind.RequireDef { source }

let sketchy_toplevel id =
  id, Kind.SketchyToplevelDef
