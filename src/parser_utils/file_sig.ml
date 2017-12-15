(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

type t = {
  module_sig: module_sig;
  declare_modules: (Loc.t * module_sig) SMap.t
}

and module_sig = {
  requires: require list;
  module_kind: module_kind;
  type_exports_named: type_export SMap.t;
  type_exports_star: export_star SMap.t;
}

and require =
  | Require of ident
  | ImportDynamic of ident
  | Import0 of ident
  | Import of {
    source: ident;
    named: Loc.t Nel.t SMap.t SMap.t;
    ns: Loc.t Nel.t SMap.t;
    types: Loc.t Nel.t SMap.t SMap.t;
    typesof: Loc.t Nel.t SMap.t SMap.t;
    typesof_ns: Loc.t Nel.t SMap.t;
  }

and module_kind =
  | CommonJS of {
    clobbered: Loc.t option;
  }
  | ES of {
    named: export SMap.t;
    star: export_star SMap.t;
  }

and export =
  | ExportDefault of { local: ident option }
  | ExportNamed of {
    loc: Loc.t;
    local: ident option;
    source: ident option;
  }
  | ExportNs of {
    loc: Loc.t;
    source: ident;
  }

and export_star =
  | ExportStar of { star_loc: Loc.t; source_loc: Loc.t }

and type_export =
  | TypeExportNamed of {
    loc: Loc.t;
    local: ident option;
    source: ident option;
  }

and ident = Loc.t * string

type error =
  | IndeterminateModuleType of Loc.t

let empty_module_sig = {
  requires = [];
  module_kind = CommonJS { clobbered = None };
  type_exports_named = SMap.empty;
  type_exports_star = SMap.empty;
}

let empty_file_sig = {
  module_sig = empty_module_sig;
  declare_modules = SMap.empty;
}

let combine_nel _ a b = Some (Nel.concat (a, [b]))

let require_loc_map msig =
  let acc = SMap.empty in
  (* requires *)
  let acc = List.fold_left (fun acc require ->
    match require with
    | Require (loc, mref)
    | ImportDynamic (loc, mref)
    | Import0 (loc, mref)
    | Import { source = (loc, mref); _ } ->
      SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
  ) acc msig.requires in
  (* export type {...} from 'foo' *)
  let acc = SMap.fold (fun _ type_export acc ->
    match type_export with
    | TypeExportNamed { source = Some (loc, mref); _ } ->
      SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
    | _ -> acc
  ) msig.type_exports_named acc in
  (* export type * from 'foo' *)
  let acc = SMap.fold (fun mref export_star acc ->
    match export_star with
    | ExportStar { source_loc; _ } ->
      SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append
  ) msig.type_exports_star acc in
  let acc = match msig.module_kind with
  | CommonJS _ -> acc
  | ES { named; star } ->
    (* export {...} from 'foo' *)
    let acc = SMap.fold (fun _ export acc ->
      match export with
      | ExportNamed { source = Some (loc, mref); _ }
      | ExportNs { source = (loc, mref); _ } ->
        SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
      | _ -> acc
    ) named acc in
    (* export * from 'foo' *)
    let acc = SMap.fold (fun mref export_star acc ->
      match export_star with
      | ExportStar { source_loc; _ } ->
        SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append
    ) star acc in
    acc
  in
  acc

let add_declare_module name m loc fsig = {
  fsig with
  declare_modules = SMap.add name (loc, m) fsig.declare_modules;
}

let add_require require msig =
  let requires = require :: msig.requires in
  Ok ({ msig with requires })

let add_type_exports named star msig =
  let type_exports_named = List.fold_left (fun acc (export, name) ->
    let type_export = match export with
    | ExportNamed { loc; local; source } -> TypeExportNamed { loc; local; source }
    | ExportDefault _ -> failwith "export default type"
    | ExportNs _ -> failwith "export type * as X"
    in
    SMap.add name type_export acc
  ) msig.type_exports_named named in
  let type_exports_star = List.fold_left (fun acc (export_star, mref) ->
    SMap.add mref export_star acc
  ) msig.type_exports_star star in
  Ok { msig with type_exports_named; type_exports_star }

let add_es_exports loc named star msig =
  let result = match msig.module_kind with
  | CommonJS { clobbered = Some _; _ } -> Error (IndeterminateModuleType loc)
  | CommonJS { clobbered = None } -> Ok (SMap.empty, SMap.empty)
  | ES { named; star } -> Ok (named, star)
  in
  match result with
  | Error e -> Error e
  | Ok (named0, star0) ->
    let named = List.fold_left (fun acc (export, name) ->
      SMap.add name export acc
    ) named0 named in
    let star = List.fold_left (fun acc (export_star, mref) ->
      SMap.add mref export_star acc
    ) star0 star in
    let module_kind = ES { named; star } in
    Ok ({ msig with module_kind })

let clobber_cjs_exports loc msig =
  match msig.module_kind with
  | CommonJS { clobbered = _ } ->
    let clobbered = Some loc in
    let module_kind = CommonJS { clobbered } in
    Ok ({ msig with module_kind })
  | ES _ -> Error (IndeterminateModuleType loc)

(* Subclass of the AST visitor class that calculates requires. Initializes with
   the scope builder class.
*)
class requires_calculator ~ast = object(this)
  inherit [(t, error) result] visitor ~init:(Ok empty_file_sig) as super

  val scope_info = Scope_builder.program ast

  val mutable curr_declare_module: module_sig option = None;

  method private update_module_sig f =
    match curr_declare_module with
    | Some m ->
      (match f m with
      | Error e -> this#set_acc (Error e)
      | Ok msig -> curr_declare_module <- Some msig)
    | None ->
      this#update_acc (function
        | Error _ as acc -> acc
        | Ok fsig ->
          match f fsig.module_sig with
          | Error e -> Error e
          | Ok module_sig -> Ok ({ fsig with module_sig })
      )

  method private add_require require =
    this#update_module_sig (add_require require)

  method private add_exports loc kind named batch =
    let add = Ast.Statement.(match kind with
    | ExportType -> add_type_exports
    | ExportValue -> (add_es_exports loc)
    ) in
    this#update_module_sig (add named batch)

  method private clobber_cjs_exports loc =
    this#update_module_sig (clobber_cjs_exports loc)

  method! call (expr: Loc.t Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; arguments } = expr in
    begin match callee, arguments with
    | ((_, Identifier (loc, "require")), [Expression (source_loc, (
        Literal { Ast.Literal.value = Ast.Literal.String name; _ } |
        TemplateLiteral { TemplateLiteral.
          quasis = [_, { TemplateLiteral.Element.
            value = { TemplateLiteral.Element.cooked = name; _ }; _
          }]; _
        }
      ))]) ->
      if not (Scope_api.is_local_use scope_info loc)
      then this#add_require (Require (source_loc, name))
    | ((_, Identifier (loc, "requireLazy")),
       [Expression (_, Array ({ Array.elements })); Expression (_);])
      ->
      let element = function
        | Some (Expression (source_loc, Literal { Ast.Literal.value = Ast.Literal.String name; _ })) ->
          if not (Scope_api.is_local_use scope_info loc)
          then this#add_require (Require (source_loc, name))
        | _ -> () in
      List.iter element elements
    | _ -> ()
    end;
    super#call expr

  method! import (expr: Loc.t Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | loc, (
        Literal { Ast.Literal.value = Ast.Literal.String name; _ } |
        TemplateLiteral { TemplateLiteral.
          quasis = [_, { TemplateLiteral.Element.
            value = { TemplateLiteral.Element.cooked = name; _ }; _
          }]; _
        }
      ) ->
      this#add_require (ImportDynamic (loc, name))
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration stmt_loc (decl: Loc.t Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind; source; specifiers; default } = decl in
    let source = match source with
    | loc, { Ast.StringLiteral.value = name; _ } -> loc, name
    in
    let import = match default, specifiers with
    | None, None -> Import0 source
    | _ ->
      let named = ref SMap.empty in
      let ns = ref SMap.empty in
      let types = ref SMap.empty in
      let typesof = ref SMap.empty in
      let typesof_ns = ref SMap.empty in
      let ref_of_kind = function
        | ImportType -> types
        | ImportTypeof -> typesof
        | ImportValue -> named
      in
      let add_named remote local loc ref =
        let locals = SMap.singleton local (Nel.one loc) in
        let combine_nel_smap a b = SMap.union a b ~combine:combine_nel in
        ref := SMap.add remote locals !ref ~combine:combine_nel_smap
      in
      let add_ns local loc ref =
        let locs = Nel.one loc in
        ref := SMap.add local locs !ref ~combine:Nel.rev_append
      in
      Option.iter ~f:(fun (loc, local) ->
        add_named "default" local loc (ref_of_kind importKind)
      ) default;
      Option.iter ~f:(function
        | ImportNamespaceSpecifier (loc, (_, local)) ->
          (match importKind with
          | ImportType -> failwith "import type * is a parse error"
          | ImportTypeof -> add_ns local loc typesof_ns
          | ImportValue -> add_ns local loc ns)
        | ImportNamedSpecifiers named_specifiers ->
          List.iter (function {local; remote; kind} ->
            let importKind = match kind with Some k -> k | None -> importKind in
            let loc, local_name = match local with Some x -> x | None -> remote in
            let _, remote_name = remote in
            add_named remote_name local_name loc (ref_of_kind importKind)
          ) named_specifiers
      ) specifiers;
      Import {
        source;
        named = !named;
        ns = !ns;
        types = !types;
        typesof = !typesof;
        typesof_ns = !typesof_ns;
      }
    in
    this#add_require import;
    super#import_declaration stmt_loc decl

  method! export_default_declaration stmt_loc (decl: Loc.t Ast.Statement.ExportDefaultDeclaration.t) =
    let open Ast.Statement in
    let open Ast.Statement.ExportDefaultDeclaration in
    let local =  match decl with
    | Declaration (_, FunctionDeclaration { Ast.Function.id; _ }) -> id
    | Declaration (_, ClassDeclaration { Ast.Class.id; _ }) -> id
    | _ -> None
    in
    let export = ExportDefault { local } in
    this#add_exports stmt_loc ExportValue [export, "default"] [];
    super#export_default_declaration stmt_loc decl

  method! export_named_declaration stmt_loc (decl: Loc.t Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind; source; specifiers; declaration} = decl in
    let source = match source with
    | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) -> Some (loc, mref)
    | None -> None
    in
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some (loc, stmt) ->
      let open Ast.Statement in
      assert (source = None);
      match stmt with
      | FunctionDeclaration { Ast.Function.id = Some (loc, name); _ }
      | ClassDeclaration { Ast.Class.id = Some (loc, name); _ } ->
        let export = ExportNamed { loc; local = None; source } in
        this#add_exports stmt_loc ExportValue [export, name] []
      | VariableDeclaration { VariableDeclaration.declarations = decls; _ } ->
        let bindings = Ast_utils.bindings_of_variable_declarations decls in
        let bindings = List.map (fun (loc, name) ->
          let export = ExportNamed { loc; local = None; source } in
          (export, name)
        ) bindings in
        this#add_exports stmt_loc ExportValue bindings []
      | TypeAlias { TypeAlias.id; _ }
      | OpaqueType { OpaqueType.id; _ }
      | InterfaceDeclaration { Interface.id; _ } ->
        let export = ExportNamed { loc; local = None; source } in
        this#add_exports stmt_loc ExportType [export, (snd id)] [];
      | _ -> failwith "unsupported declaration"
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      this#export_specifiers stmt_loc exportKind source specifiers
    end;
    super#export_named_declaration stmt_loc decl

  method! declare_module_exports loc (annot: Loc.t Ast.Type.annotation) =
    this#clobber_cjs_exports loc;
    super#declare_module_exports loc annot

  method! declare_export_declaration stmt_loc (decl: Loc.t Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    let source = match source with
    | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) ->
      assert (not default); (* declare export default from not supported *)
      Some (loc, mref)
    | _ -> None
    in
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some declaration ->
      let open Ast.Statement in
      assert (source = None);
      match declaration with
      | Variable (_, { DeclareVariable.id; _ })
      | Function (_, { DeclareFunction.id; _ })
      | Class (_, { DeclareClass.id; _ }) ->
        let name, export =
          if default
          then "default", ExportDefault { local = Some id }
          else snd id, ExportNamed { loc = fst id; local = None; source }
        in
        this#add_exports stmt_loc ExportValue [export, name] []
      | DefaultType _ ->
        let export = ExportDefault { local = None } in
        this#add_exports stmt_loc ExportValue [export, "default"] []
      | NamedType (_, { TypeAlias.id; _ })
      | NamedOpaqueType (_, { OpaqueType.id; _ })
      | Interface (_, { Interface.id; _ }) ->
        assert (not default);
        let export = ExportNamed { loc = fst id; local = None; source } in
        this#add_exports stmt_loc ExportType [export, snd id] []
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      assert (not default);
      (* declare export type unsupported *)
      let exportKind = Ast.Statement.ExportValue in
      this#export_specifiers stmt_loc exportKind source specifiers
    end;
    super#declare_export_declaration stmt_loc decl

  method! assignment (expr: Loc.t Ast.Expression.Assignment.t) =
    let open Ast.Expression in
    let open Ast.Expression.Assignment in
    (* module.exports = e *)
    let { operator; left; _ } = expr in
    begin match operator, left with
    | Assign, (assign_loc, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Ast.Expression.Identifier (_, "module");
        property = Member.PropertyIdentifier (_, "exports"); _
      })) ->
      (* expressions not allowed in declare module body *)
      assert (curr_declare_module = None);
      if not (Scope_api.is_local_use scope_info module_loc)
      then this#clobber_cjs_exports assign_loc
    | _ -> ()
    end;
    super#assignment expr

  method! declare_module loc (m: Loc.t Ast.Statement.DeclareModule.t) =
    let name = Ast.Statement.DeclareModule.(match m.id with
    | Identifier (_, name) -> name
    | Literal (_, { Ast.StringLiteral.value; _ }) -> value
    ) in
    curr_declare_module <- Some (empty_module_sig);
    let ret = super#declare_module loc m in
    begin match curr_declare_module with
    | None -> failwith "lost curr_declare_module"
    | Some m ->
      this#update_acc (function
        | Error _ as acc -> acc
        | Ok fsig -> Ok (add_declare_module name m loc fsig)
      )
    end;
    curr_declare_module <- None;
    ret

  method private export_specifiers stmt_loc kind source =
    let open Ast.Statement.ExportNamedDeclaration in
    function
    | ExportBatchSpecifier (_, Some (loc, name)) ->
      (* export type * as X from "foo" unsupported *)
      assert (kind = Ast.Statement.ExportValue);
      let mref = match source with
      | Some mref -> mref
      | None -> failwith "export batch without source"
      in
      this#add_exports stmt_loc kind [ExportNs { loc; source = mref }, name] []
    | ExportBatchSpecifier (star_loc, None) ->
      let source_loc, mref = match source with
      | Some (source_loc, mref) -> source_loc, mref
      | _ -> failwith "batch export missing source"
      in
      this#add_exports stmt_loc kind [] [ExportStar { star_loc; source_loc }, mref]
    | ExportSpecifiers specs ->
      let bindings = List.fold_left ExportSpecifier.(fun acc (_, spec) ->
        let name, loc, local = match spec.exported with
        | None -> snd spec.local, fst spec.local, None
        | Some remote -> snd remote, fst remote, Some spec.local
        in
        let export = ExportNamed { loc; local; source } in
        (export, name) :: acc
      ) [] specs in
      this#add_exports stmt_loc kind bindings []
end

let program ~ast =
  let walk = new requires_calculator ~ast in
  walk#eval walk#program ast
