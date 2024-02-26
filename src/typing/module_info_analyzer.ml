(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason

module Module_info = struct
  type t = {
    mutable kind: kind;
    mutable type_named: Type.Exports.t;
    mutable type_star: (ALoc.t * Type.t) list;
  }

  and cjs_exports_state =
    | CJSExportNames of (ALoc.t * Type.t) SMap.t
    | CJSModuleExports of ALoc.t * Type.t

  and kind =
    | Unknown
    | CJS of cjs_exports_state
    | ES of {
        named: Type.Exports.t;
        star: (ALoc.t * Type.t) list;
      }

  let export_value info name ~name_loc type_ =
    match info.kind with
    | Unknown ->
      info.kind <-
        ES
          {
            named =
              NameUtils.Map.singleton
                name
                { Type.preferred_def_locs = None; name_loc = Some name_loc; type_ };
            star = [];
          }
    | ES { named; star } ->
      info.kind <-
        ES
          {
            named =
              NameUtils.Map.add
                name
                { Type.preferred_def_locs = None; name_loc = Some name_loc; type_ }
                named;
            star;
          }
    | CJS _ ->
      (* Indeterminate module. We already errored in module_exports_checker. *)
      ()

  let export_star info loc module_t =
    match info.kind with
    | Unknown -> info.kind <- ES { named = NameUtils.Map.empty; star = [(loc, module_t)] }
    | ES { named; star } -> info.kind <- ES { named; star = (loc, module_t) :: star }
    | CJS _ ->
      (* Indeterminate module. We already errored in module_exports_checker. *)
      ()

  let export_type info name ~name_loc type_ =
    info.type_named <-
      NameUtils.Map.add
        name
        { Type.preferred_def_locs = None; name_loc = Some name_loc; type_ }
        info.type_named

  let export_type_star info loc module_t = info.type_star <- (loc, module_t) :: info.type_star

  let cjs_mod_export info f =
    match info.kind with
    | Unknown -> info.kind <- CJS (f (CJSExportNames SMap.empty))
    | CJS state -> info.kind <- CJS (f state)
    | ES _ -> ()

  (* Re-exporting names from another file can lead to conflicts. We resolve
   * conflicts on a last-export-wins basis. Star exports are accumulated in
   * source order, so the head of each list is the last export. This helper
   * function interleaves the two reverse-sorted lists. *)
  let rec fold_star2 f g acc = function
    | ([], []) -> acc
    | (xs, []) -> List.fold_left f acc xs
    | ([], ys) -> List.fold_left g acc ys
    | ((x :: xs' as xs), (y :: ys' as ys)) ->
      if ALoc.compare (fst x) (fst y) > 0 then
        fold_star2 f g (f acc x) (xs', ys)
      else
        fold_star2 f g (g acc y) (xs, ys')
end

let export_specifiers info loc source export_kind =
  let open Ast.Statement in
  let module E = ExportNamedDeclaration in
  (* [declare] export [type] {foo [as bar]}; *)
  let export_ref loc local_type remote_name =
    match export_kind with
    | Ast.Statement.ExportType -> Module_info.export_type info remote_name ~name_loc:loc local_type
    | Ast.Statement.ExportValue ->
      Module_info.export_value info remote_name ~name_loc:loc local_type
  in
  (* [declare] export [type] {foo [as bar]} from 'module' *)
  let export_from loc local_type remote_name =
    match export_kind with
    | Ast.Statement.ExportType -> Module_info.export_type info remote_name ~name_loc:loc local_type
    | Ast.Statement.ExportValue ->
      Module_info.export_value info remote_name ~name_loc:loc local_type
  in
  let export_specifier export (_, { E.ExportSpecifier.local; exported }) =
    let ((local_loc, local_type), { Ast.Identifier.name = local_name; comments = _ }) = local in
    let remote_name =
      match exported with
      | None -> OrdinaryName local_name
      | Some (_, { Ast.Identifier.name = remote_name; comments = _ }) -> OrdinaryName remote_name
    in
    export local_loc local_type remote_name
  in
  function
  (* [declare] export [type] {foo [as bar]} [from ...]; *)
  | E.ExportSpecifiers specifiers ->
    let export =
      match source with
      | Some _ -> export_from
      | None -> export_ref
    in
    Base.List.iter ~f:(export_specifier export) specifiers
  (* [declare] export [type] * as id from "source"; *)
  | E.ExportBatchSpecifier (_, Some (_, { Ast.Identifier.name; _ })) ->
    let ((_, module_t), _) = Base.Option.value_exn source in
    (match export_kind with
    | Ast.Statement.ExportValue ->
      Module_info.export_type info (OrdinaryName name) ~name_loc:loc module_t
    | Ast.Statement.ExportType ->
      Module_info.export_value info (OrdinaryName name) ~name_loc:loc module_t)
  (* [declare] export [type] * from "source"; *)
  | E.ExportBatchSpecifier (_, None) ->
    let ((_, module_t), _) = Base.Option.value_exn source in
    (match export_kind with
    | Ast.Statement.ExportValue -> Module_info.export_star info loc module_t
    | Ast.Statement.ExportType -> Module_info.export_type_star info loc module_t)

let visit_toplevel_statement cx info ~in_declare_namespace :
    (ALoc.t, ALoc.t * Type.t) Ast.Statement.t -> unit =
  let open Ast.Statement in
  function
  | (_, Empty _)
  | (_, Block _)
  | (_, If _)
  | (_, Labeled _)
  | (_, Break _)
  | (_, Continue _)
  | (_, Throw _)
  | (_, Try _)
  | (_, With _)
  | (_, Switch _)
  | (_, Return _)
  | (_, While _)
  | (_, DoWhile _)
  | (_, For _)
  | (_, ForIn _)
  | (_, ForOf _)
  | (_, Debugger _)
  | (_, FunctionDeclaration _)
  | (_, ComponentDeclaration _)
  | (_, VariableDeclaration _)
  | (_, ClassDeclaration _)
  | (_, DeclareModule _)
  | (_, ImportDeclaration _) ->
    ()
  | (_, DeclareTypeAlias { TypeAlias.id; _ })
  | (_, TypeAlias { TypeAlias.id; _ })
  | (_, DeclareOpaqueType { OpaqueType.id; _ })
  | (_, OpaqueType { OpaqueType.id; _ })
  | (_, DeclareInterface { Interface.id; _ })
  | (_, InterfaceDeclaration { Interface.id; _ }) ->
    (* A declared namespace will auto-export all toplevel names *)
    if in_declare_namespace then
      let ((name_loc, t), { Ast.Identifier.name; _ }) = id in
      Module_info.export_type info (OrdinaryName name) ~name_loc t
  | (_, DeclareVariable { DeclareVariable.id; _ })
  | (_, DeclareFunction { DeclareFunction.id; _ })
  | (_, DeclareClass { DeclareClass.id; _ })
  | (_, DeclareComponent { DeclareComponent.id; _ })
  | (_, DeclareEnum { EnumDeclaration.id; _ })
  | (_, EnumDeclaration { EnumDeclaration.id; _ })
  | (_, DeclareNamespace { DeclareNamespace.id; _ }) ->
    (* A declared namespace will auto-export all toplevel names *)
    if in_declare_namespace then
      let ((name_loc, t), { Ast.Identifier.name; _ }) = id in
      Module_info.export_value info (OrdinaryName name) ~name_loc t
  | ( _,
      Expression
        {
          Expression.expression =
            ( _,
              Ast.Expression.Assignment
                {
                  Ast.Expression.Assignment.operator = None;
                  left = (_, Ast.Pattern.Expression (_, Ast.Expression.Member member));
                  right = ((_, t), _);
                  comments = _;
                }
            );
          directive = _;
          comments = _;
        }
    ) ->
    (match member with
    (* module.exports = ... *)
    | {
     Ast.Expression.Member._object =
       ( _,
         Ast.Expression.Identifier
           ((module_loc, _), { Ast.Identifier.name = "module"; comments = _ })
       );
     property =
       Ast.Expression.Member.PropertyIdentifier
         ((exports_loc, _), { Ast.Identifier.name = "exports"; comments = _ });
     comments = _;
    }
      when Type_env.is_global_var cx module_loc ->
      Module_info.cjs_mod_export info (fun _ -> Module_info.CJSModuleExports (exports_loc, t))
    (* module.exports.foo = ... *)
    | {
     Ast.Expression.Member._object =
       ( _,
         Ast.Expression.Member
           {
             Ast.Expression.Member._object =
               ( (module_loc, _),
                 Ast.Expression.Identifier (_, { Ast.Identifier.name = "module"; comments = _ })
               );
             property =
               Ast.Expression.Member.PropertyIdentifier
                 (_, { Ast.Identifier.name = "exports"; comments = _ });
             comments = _;
           }
       );
     property =
       Ast.Expression.Member.PropertyIdentifier ((key_loc, _), { Ast.Identifier.name; comments = _ });
     comments = _;
    }
      when Type_env.is_global_var cx module_loc ->
      Module_info.cjs_mod_export info (function
          | Module_info.CJSModuleExports _ as state -> state
          | Module_info.CJSExportNames named ->
            Module_info.CJSExportNames (SMap.add name (key_loc, t) named)
          )
    (* exports.foo = ... *)
    | {
     Ast.Expression.Member._object =
       ( _,
         Ast.Expression.Identifier
           ((exports_loc, _), { Ast.Identifier.name = "exports"; comments = _ })
       );
     property =
       Ast.Expression.Member.PropertyIdentifier ((key_loc, _), { Ast.Identifier.name; comments = _ });
     comments = _;
    }
      when Type_env.is_global_var cx exports_loc ->
      Module_info.cjs_mod_export info (function
          | Module_info.CJSModuleExports _ as state -> state
          | Module_info.CJSExportNames named ->
            Module_info.CJSExportNames (SMap.add name (key_loc, t) named)
          )
    | _ -> ())
  | (_, Expression _) -> ()
  | (loc, DeclareExportDeclaration decl) ->
    let module D = DeclareExportDeclaration in
    let { D.default; declaration; specifiers; source; comments = _ } = decl in
    let export_maybe_default_binding id =
      let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
      match default with
      | None -> Module_info.export_value info (OrdinaryName name) ~name_loc t
      | Some default_loc ->
        Module_info.export_value info (OrdinaryName "default") ~name_loc:default_loc t
    in
    let f = function
      | D.Variable (_, { DeclareVariable.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        Module_info.export_value info (OrdinaryName name) ~name_loc t
      | D.Function (_, f) -> export_maybe_default_binding f.DeclareFunction.id
      | D.Class (_, c) -> export_maybe_default_binding c.DeclareClass.id
      | D.Component (_, c) -> export_maybe_default_binding c.DeclareComponent.id
      | D.DefaultType ((_, t), _) ->
        let default_loc = Base.Option.value_exn default in
        Module_info.export_type info (OrdinaryName "default") ~name_loc:default_loc t
      | D.NamedType (_, { TypeAlias.id; _ })
      | D.NamedOpaqueType (_, { OpaqueType.id; _ })
      | D.Interface (_, { Interface.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        Module_info.export_type info (OrdinaryName name) ~name_loc t
      | D.Enum (_, { EnumDeclaration.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        if Context.enable_enums cx then
          Module_info.export_value info (OrdinaryName name) ~name_loc t
    in
    Option.iter f declaration;
    let export_kind = Ast.Statement.ExportValue in
    Option.iter (export_specifiers info loc source export_kind) specifiers
  | ( _,
      DeclareModuleExports
        { Ast.Statement.DeclareModuleExports.annot = (exports_loc, ((_, t), _)); comments = _ }
    ) ->
    Module_info.cjs_mod_export info (fun _ -> Module_info.CJSModuleExports (exports_loc, t))
  | ( loc,
      ExportNamedDeclaration
        { ExportNamedDeclaration.declaration; specifiers; source; export_kind; comments = _ }
    ) ->
    let export_id ((name_loc, t), { Ast.Identifier.name; comments = _ }) =
      match export_kind with
      | Ast.Statement.ExportValue -> Module_info.export_value info (OrdinaryName name) ~name_loc t
      | Ast.Statement.ExportType -> Module_info.export_type info (OrdinaryName name) ~name_loc t
    in
    let () =
      match declaration with
      | None -> ()
      | Some (_, stmt) -> begin
        match stmt with
        | FunctionDeclaration { Ast.Function.id = Some id; _ }
        | ClassDeclaration { Ast.Class.id = Some id; _ }
        | TypeAlias { TypeAlias.id; _ }
        | OpaqueType { OpaqueType.id; _ }
        | InterfaceDeclaration { Interface.id; _ }
        | ComponentDeclaration { ComponentDeclaration.id; _ }
        | EnumDeclaration { EnumDeclaration.id; _ } ->
          export_id id
        | VariableDeclaration { VariableDeclaration.declarations; _ } ->
          Flow_ast_utils.fold_bindings_of_variable_declarations
            (fun _ () id -> export_id id)
            ()
            declarations
        | _ -> (* Parser Error: Invalid export-declaration type! *) ()
      end
    in
    Option.iter (export_specifiers info loc source export_kind) specifiers
  | ( _,
      ExportDefaultDeclaration
        { ExportDefaultDeclaration.default = (_, t); declaration; comments = _ }
    ) ->
    let module D = ExportDefaultDeclaration in
    let export_loc =
      match declaration with
      | D.Declaration (loc, stmt) ->
        (match stmt with
        | FunctionDeclaration { Ast.Function.id = Some ((id_loc, _), _); _ }
        | ClassDeclaration { Ast.Class.id = Some ((id_loc, _), _); _ }
        | EnumDeclaration { EnumDeclaration.id = ((id_loc, _), _); _ }
        | ComponentDeclaration { ComponentDeclaration.id = ((id_loc, _), _); _ } ->
          id_loc
        | _ -> loc)
      | D.Expression ((loc, _), _) -> loc
    in
    Module_info.export_value info (OrdinaryName "default") ~name_loc:export_loc t

(* A best effort way to pick a location as the signature location of the module.
 * - For cjs, we will first try to pick the location of module.exports, then
 *   fallback to the first module.exports prop assignment
 * - For esm, we will first try to pick the location of default exports, then
 *   fallback to the first export. *)
let module_exports_sig_loc { Module_info.kind; type_named; _ } =
  let first_loc_of_named_exports named =
    named
    |> NameUtils.Map.values
    |> Base.List.filter_map ~f:(fun { Type.name_loc; _ } -> name_loc)
    |> Base.List.sort ~compare:ALoc.compare
    |> Base.List.hd
  in
  match kind with
  | Module_info.Unknown -> None
  | Module_info.(CJS (CJSModuleExports (l, _))) -> Some l
  | Module_info.(CJS (CJSExportNames names)) ->
    (match
       names
       |> SMap.values
       |> Base.List.map ~f:fst
       |> Base.List.sort ~compare:ALoc.compare
       |> Base.List.hd
     with
    | None -> first_loc_of_named_exports type_named
    | loc -> loc)
  | Module_info.ES { named; _ } ->
    (match NameUtils.Map.find_opt (Reason.OrdinaryName "default") named with
    | Some { Type.name_loc; _ } -> name_loc
    | None ->
      (match first_loc_of_named_exports named with
      | None -> first_loc_of_named_exports type_named
      | loc -> loc))

(* After we have seen all the export statements in a module, this function will
 * calculate a ModuleT type (or a tvar that resolves to one) describing the
 * exports of a file.
 *
 * For CommonJS modules, this is fairly simple. We have the exported value
 * itself, plus any type exports. If the exported value is an object, we treat
 * the fields as named exports for ES module dependents.
 *
 * For ES modules, we have both named exports and "star" exports, which copy the
 * exports of one file into another. This can lead to conflits, which are
 * resolved carefully. Note that locally named exports always win, even if they
 * are followed by a star export that includes a conflicting name.
 *
 * Finally, both CJS and ES modules can export types, which also has a star
 * export variant. Conflicts are handled in the same way.
 *)
let mk_module_t =
  let open Module_info in
  let open Type in
  let mk_esm_module_t cx reason =
    ModuleT
      {
        module_reason = reason;
        module_export_types =
          {
            value_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
            type_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
            cjs_export = None;
            has_every_named_export = false;
          };
        module_is_strict = Context.is_strict cx;
        module_available_platforms = Context.available_platforms cx;
      }
  in
  (*
   * When CommonJS modules set their export type, we do two things:
   *
   * (1) Set the type in the cjs_export slot of the ModuleT container
   *
   * (2) If the type is an object, mark it's properties as named exports, via
   *     CJSExtractNamedExportsT. (this is for convenience as part of our
   *     ES <-> CJS module interop semantics)
   *)
  let mk_commonjs_module_t cx reason_exports_module reason cjs_exports_state =
    let export_t =
      match cjs_exports_state with
      | CJSModuleExports (_, t) -> t
      | CJSExportNames named ->
        let props =
          SMap.fold
            (fun name (key_loc, type_) acc ->
              NameUtils.Map.add
                (OrdinaryName name)
                (Field
                   {
                     preferred_def_locs = None;
                     key_loc = Some key_loc;
                     polarity = Polarity.Positive;
                     type_;
                   }
                )
                acc)
            named
            NameUtils.Map.empty
        in
        Obj_type.mk_with_proto
          cx
          reason
          (Type.ObjProtoT reason)
          ~obj_kind:Type.Exact
          ~frozen:false
          ~props
    in
    let module_export_types =
      {
        value_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
        type_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
        cjs_export = Some export_t;
        has_every_named_export = false;
      }
    in
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
        Flow_js.flow
          cx
          ( export_t,
            CJSExtractNamedExportsT
              ( reason,
                {
                  module_reason = reason_exports_module;
                  module_export_types;
                  module_is_strict = Context.is_strict cx;
                  module_available_platforms = Context.available_platforms cx;
                },
                t
              )
          )
    )
  in
  let copy_star_exports cx reason exports module_t =
    let copy_named_exports module_t (loc, from_ns) =
      let reason = repos_reason loc reason in
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          Flow_js.flow cx (from_ns, CopyNamedExportsT (reason, module_t, tout))
      )
    in
    let copy_type_exports module_t (loc, from_ns) =
      let reason = repos_reason loc reason in
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          Flow_js.flow cx (from_ns, CopyTypeExportsT (reason, module_t, tout))
      )
    in
    Module_info.fold_star2 copy_named_exports copy_type_exports module_t exports
  in
  let export_named cx reason export_kind value_exports_tmap type_exports_tmap module_t =
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        Flow_js.flow
          cx
          ( module_t,
            ExportNamedT { reason; value_exports_tmap; type_exports_tmap; export_kind; tout }
          )
    )
  in
  fun cx info self_reason exports_reason ->
    match info.kind with
    | Unknown ->
      mk_commonjs_module_t cx self_reason exports_reason (CJSExportNames SMap.empty)
      |> export_named cx self_reason DirectExport NameUtils.Map.empty info.type_named
      |> copy_star_exports cx self_reason ([], info.type_star)
    | CJS cjs_exports_state ->
      mk_commonjs_module_t cx self_reason exports_reason cjs_exports_state
      |> export_named cx self_reason DirectExport NameUtils.Map.empty info.type_named
      |> copy_star_exports cx self_reason ([], info.type_star)
    | ES { named; star } ->
      mk_esm_module_t cx self_reason
      |> export_named cx self_reason DirectExport named info.type_named
      |> copy_star_exports cx self_reason (star, info.type_star)

let mk_namespace_t cx info reason =
  let open Module_info in
  if not (Base.List.is_empty info.type_star) then failwith "namespace should not have star exports";
  let named =
    match info.kind with
    | Unknown -> NameUtils.Map.empty
    | CJS _ -> failwith "namespace should never transition into CJS state"
    | ES { named = _; star = _ :: _ } -> failwith "namespace should not have star exports"
    | ES { named; star = [] } -> named
  in
  Flow_js_utils.namespace_type cx reason named info.type_named

let analyze_program cx (prog_aloc, { Flow_ast.Program.statements; _ }) =
  let info =
    { Module_info.kind = Module_info.Unknown; type_named = NameUtils.Map.empty; type_star = [] }
  in
  Base.List.iter ~f:(visit_toplevel_statement cx info ~in_declare_namespace:false) statements;
  let module_sig_loc = module_exports_sig_loc info |> Base.Option.value ~default:prog_aloc in
  let module_t =
    let self_reason = Reason.(mk_reason (RCustom "self") prog_aloc) in
    let file_loc = Loc.{ none with source = Some (Context.file cx) } |> ALoc.of_loc in
    let exports_reason = Reason.(mk_reason RExports file_loc) in
    mk_module_t cx info self_reason exports_reason
  in
  (module_sig_loc, module_t)

let analyze_declare_namespace cx reason statements =
  let info =
    { Module_info.kind = Module_info.Unknown; type_named = NameUtils.Map.empty; type_star = [] }
  in
  Base.List.iter statements ~f:(fun ((loc, stmt') as stmt) ->
      match
        Flow_ast_utils.acceptable_statement_in_declaration_context ~in_declare_namespace:true stmt'
      with
      | Ok () -> visit_toplevel_statement cx info ~in_declare_namespace:true stmt
      | Error kind ->
        Flow_js_utils.add_output
          cx
          Error_message.(
            EUnsupportedSyntax
              ( loc,
                ContextDependentUnsupportedStatement (UnsupportedStatementInDeclareNamespace kind)
              )
          );
        ()
  );
  mk_namespace_t cx info reason
