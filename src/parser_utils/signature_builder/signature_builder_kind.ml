(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast

module Annot_path = struct
  type t =
    | Annot of (Loc.t, Loc.t) Ast.Type.annotation
    | Object of Loc.t * (t * (Loc.t * string))

  let mk_annot ?annot_path = function
    | Ast.Type.Missing _ -> annot_path
    | Ast.Type.Available annot -> Some (Annot annot)

  let mk_object prop_loc ?annot_path (loc, x) =
    match annot_path with
    | None -> None
    | Some annot_path -> Some (Object (prop_loc, (annot_path, (loc, x))))
end

module Init_path = struct
  type t =
    | Init of (Loc.t, Loc.t) Ast.Expression.t
    | Object of Loc.t * (t * (Loc.t * string))

  let mk_init = function
    | None -> None
    | Some init -> Some (Init init)

  let mk_object prop_loc ?init_path (loc, x) =
    match init_path with
    | None -> None
    | Some init_path -> Some (Object (prop_loc, (init_path, (loc, x))))
end

module Sort = struct
  type t =
    | Type
    | Value

  let to_string = function
    | Type -> "type"
    | Value -> "value"

  let is_import_type =
    Ast.Statement.ImportDeclaration.(
      function
      | ImportType
      | ImportTypeof ->
        true
      | ImportValue -> true)

  (* conditional *)

  let is_import_value =
    Ast.Statement.ImportDeclaration.(
      function
      | ImportType
      | ImportTypeof ->
        false
      | ImportValue -> true)

  let of_import_kind =
    Ast.Statement.ImportDeclaration.(
      function
      | ImportValue
      | ImportTypeof ->
        Value
      | ImportType -> Type)
end

type t =
  | WithPropertiesDef of {
      properties: ((Loc.t, Loc.t) Ast.Identifier.t * (Loc.t, Loc.t) Ast.Expression.t) list;
      base: t;
    }
  | VariableDef of {
      id: (Loc.t, Loc.t) Ast.Identifier.t;
      annot: Annot_path.t option;
      init: Init_path.t option;
    }
  | FunctionDef of {
      generator: bool;
      async: bool;
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      params: (Loc.t, Loc.t) Ast.Function.Params.t;
      return: (Loc.t, Loc.t) Ast.Type.annotation_or_hint;
      body: (Loc.t, Loc.t) Ast.Function.body;
      predicate: (Loc.t, Loc.t) Ast.Type.Predicate.t option;
    }
  | DeclareFunctionDef of {
      annot: (Loc.t, Loc.t) Ast.Type.annotation;
      predicate: (Loc.t, Loc.t) Ast.Type.Predicate.t option;
    }
  | ClassDef of {
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      body: (Loc.t, Loc.t) Ast.Class.Body.t;
      super: (Loc.t, Loc.t) Ast.Expression.t option;
      super_targs: (Loc.t, Loc.t) Ast.Type.TypeArgs.t option;
      implements: (Loc.t, Loc.t) Ast.Class.Implements.t list;
    }
  | DeclareClassDef of {
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      body: Loc.t * (Loc.t, Loc.t) Ast.Type.Object.t;
      extends: (Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t) option;
      mixins: (Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t) list;
      implements: (Loc.t, Loc.t) Ast.Class.Implements.t list;
    }
  | EnumDef of { body: Loc.t Ast.Statement.EnumDeclaration.body }
  | TypeDef of {
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      right: (Loc.t, Loc.t) Ast.Type.t;
    }
  | OpaqueTypeDef of {
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      impltype: (Loc.t, Loc.t) Ast.Type.t option;
      supertype: (Loc.t, Loc.t) Ast.Type.t option;
    }
  | InterfaceDef of {
      tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
      body: Loc.t * (Loc.t, Loc.t) Ast.Type.Object.t;
      extends: (Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t) list;
    }
  | ImportNamedDef of {
      kind: Ast.Statement.ImportDeclaration.importKind;
      source: Loc.t Ast_utils.source;
      name: Loc.t Ast_utils.ident;
    }
  | ImportStarDef of {
      kind: Ast.Statement.ImportDeclaration.importKind;
      source: Loc.t Ast_utils.source;
    }
  | RequireDef of {
      source: Loc.t Ast_utils.source;
      name: Loc.t Ast_utils.ident Nel.t option;
    }
  | SketchyToplevelDef

type ctor =
  | VariableDefKind
  | FunctionDefKind
  | DeclareFunctionDefKind
  | ClassDefKind
  | DeclareClassDefKind
  | EnumDefKind
  | TypeDefKind
  | OpaqueTypeDefKind
  | InterfaceDefKind
  | ImportNamedDefKind
  | ImportStarDefKind
  | RequireDefKind
  | SketchyToplevelDefKind

let rec kind_to_ctor = function
  | WithPropertiesDef { base; _ } -> kind_to_ctor base
  | VariableDef _ -> VariableDefKind
  | FunctionDef _ -> FunctionDefKind
  | DeclareFunctionDef _ -> DeclareFunctionDefKind
  | ClassDef _ -> ClassDefKind
  | DeclareClassDef _ -> DeclareClassDefKind
  | EnumDef _ -> EnumDefKind
  | TypeDef _ -> TypeDefKind
  | OpaqueTypeDef _ -> OpaqueTypeDefKind
  | InterfaceDef _ -> InterfaceDefKind
  | ImportNamedDef _ -> ImportNamedDefKind
  | ImportStarDef _ -> ImportStarDefKind
  | RequireDef _ -> RequireDefKind
  | SketchyToplevelDef -> SketchyToplevelDefKind

let rec to_string = function
  | WithPropertiesDef { base; _ } -> Printf.sprintf "WithPropertiesDef(%s)" (to_string base)
  | VariableDef _ -> "VariableDef"
  | FunctionDef _ -> "FunctionDef"
  | DeclareFunctionDef _ -> "DeclareFunctionDef"
  | ClassDef _ -> "ClassDef"
  | DeclareClassDef _ -> "DeclareClassDef"
  | EnumDef _ -> "EnumDef"
  | TypeDef _ -> "TypeDef"
  | OpaqueTypeDef _ -> "OpaqueTypeDef"
  | InterfaceDef _ -> "InterfaceDef"
  | ImportNamedDef _ -> "ImportNamedDef"
  | ImportStarDef _ -> "ImportStarDef"
  | RequireDef _ -> "RequireDef"
  | SketchyToplevelDef -> "SketchyToplevelDef"

let rec is_type = function
  | WithPropertiesDef { base; _ } -> is_type base
  | VariableDef _ -> true (* conditional *)
  | FunctionDef _ -> false
  | DeclareFunctionDef _ -> true
  | ClassDef _ -> true
  | DeclareClassDef _ -> true
  | EnumDef _ -> true
  | TypeDef _ -> true
  | OpaqueTypeDef _ -> true
  | InterfaceDef _ -> true
  | ImportNamedDef { kind; _ } -> Sort.is_import_type kind
  | ImportStarDef { kind; _ } -> Sort.is_import_type kind
  | RequireDef _ -> true (* conditional *)
  | SketchyToplevelDef -> true

(* don't care *)

let rec is_value = function
  | WithPropertiesDef { base; _ } -> is_value base
  | VariableDef _ -> true
  | FunctionDef _ -> true
  | DeclareFunctionDef _ -> true
  | ClassDef _ -> true
  | DeclareClassDef _ -> true
  | EnumDef _ -> true
  | TypeDef _ -> false
  | OpaqueTypeDef _ -> false
  | InterfaceDef _ -> false
  | ImportNamedDef { kind; _ } -> Sort.is_import_value kind
  | ImportStarDef { kind; _ } -> Sort.is_import_value kind
  | RequireDef _ -> true
  | SketchyToplevelDef -> true

(* don't care *)

let validator = function
  | Sort.Type -> is_type
  | Sort.Value -> is_value

let get_function_kind_info = function
  | FunctionDef { generator; async; tparams; params; return; body; predicate = _ } ->
    Some (generator, async, tparams, params, return, body)
  | VariableDef
      {
        id = _;
        annot = None;
        init = Some (Init_path.Init (_, Ast.Expression.(Function stuff | ArrowFunction stuff)));
      } ->
    Ast.Function.(
      let { id = _; generator; async; tparams; params; return; body; _ } = stuff in
      Some (generator, async, tparams, params, return, body))
  | _ -> None
