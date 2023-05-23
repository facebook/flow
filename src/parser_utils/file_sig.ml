(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
open Flow_ast_visitor
module Scope_api = Scope_api.With_Loc
module Scope_builder = Scope_builder.With_Loc
module Loc = Loc_sig.LocS

type t = {
  requires: require list;
  module_kind: module_kind;
}

and options = {
  enable_enums: bool;
  enable_relay_integration: bool;
  haste_module_ref_prefix: string option;
  haste_module_ref_prefix_LEGACY_INTEROP: string option;
  relay_integration_module_prefix: string option;
}

and require =
  | Require of {
      source: Loc.t Ast_utils.source;
      require_loc: Loc.t;
      bindings: require_bindings option;
      prefix: string option;
    }
  | ImportDynamic of {
      source: Loc.t Ast_utils.source;
      import_loc: Loc.t;
    }
  | Import0 of { source: Loc.t Ast_utils.source }
  | Import of {
      import_loc: Loc.t;
      source: Loc.t Ast_utils.source;
      named: imported_locs Nel.t SMap.t SMap.t;
      ns: Loc.t Ast_utils.ident option;
      types: imported_locs Nel.t SMap.t SMap.t;
      typesof: imported_locs Nel.t SMap.t SMap.t;
      typesof_ns: Loc.t Ast_utils.ident option;
    }
  | ExportFrom of { source: Loc.t Flow_ast_utils.source }

and imported_locs = {
  remote_loc: Loc.t;
  local_loc: Loc.t;
}

and require_bindings =
  | BindIdent of Loc.t Ast_utils.ident
  | BindNamed of (Loc.t Ast_utils.ident * require_bindings) list

and module_kind =
  | CommonJS of { mod_exp_loc: Loc.t option }
  | ES
[@@deriving show]

type tolerable_error =
  | IndeterminateModuleType of Loc.t
  (* e.g. `module.exports.foo = 4` when not at the top level *)
  | BadExportPosition of Loc.t
  (* e.g. `foo(module)`, dangerous because `module` is aliased *)
  | BadExportContext of string (* offending identifier *) * Loc.t
  | SignatureVerificationError of Loc.t Signature_error.t
[@@deriving show]

type tolerable_t = t * tolerable_error list

let empty = { requires = []; module_kind = CommonJS { mod_exp_loc = None } }

let default_opts =
  {
    enable_relay_integration = false;
    enable_enums = false;
    haste_module_ref_prefix = None;
    haste_module_ref_prefix_LEGACY_INTEROP = None;
    relay_integration_module_prefix = None;
  }

module PP = struct
  let string_of_option f = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some (%s)" (f x)

  let items_to_collection_string indent open_ close items =
    let indent_str = String.make (indent * 2) ' ' in
    let items_str =
      items |> Base.List.map ~f:(Printf.sprintf "%s%s;\n" (indent_str ^ "  ")) |> String.concat ""
    in
    Printf.sprintf "%s\n%s%s%s" open_ items_str indent_str close

  let items_to_list_string indent items = items_to_collection_string indent "[" "]" items

  let items_to_record_string indent items =
    let items =
      items |> Base.List.map ~f:(fun (label, value) -> Printf.sprintf "%s: %s" label value)
    in
    items_to_collection_string indent "{" "}" items
end

let to_string t =
  let string_of_require_list require_list =
    let string_of_require_bindings = function
      | BindIdent (_, name) -> Printf.sprintf "BindIdent: %s" name
      | BindNamed named ->
        Printf.sprintf
          "BindNamed: %s"
          (String.concat ", " @@ Base.List.map ~f:(fun ((_, name), _) -> name) named)
    in
    let string_of_require = function
      | Require { source = (_, name); bindings; _ } ->
        Printf.sprintf
          "Require (%s, %s)"
          name
          (PP.string_of_option string_of_require_bindings bindings)
      | ImportDynamic _ -> "ImportDynamic"
      | Import0 _ -> "Import0"
      | Import _ -> "Import"
      | ExportFrom _ -> "ExportFrom"
    in
    PP.items_to_list_string 2 (Base.List.map ~f:string_of_require require_list)
  in
  let string_of_module_kind = function
    | CommonJS _ -> "CommonJS"
    | ES -> "ES"
  in
  PP.items_to_record_string
    1
    [
      ("requires", string_of_require_list t.requires);
      ("module_kind", string_of_module_kind t.module_kind);
    ]

let combine_nel _ a b = Some (Nel.concat (a, [b]))

let require_loc_map msig =
  List.fold_left
    (fun acc require ->
      match require with
      | Require { source = (loc, mref); _ }
      | ImportDynamic { source = (loc, mref); _ }
      | Import0 { source = (loc, mref) }
      | Import { source = (loc, mref); _ }
      | ExportFrom { source = (loc, mref) } ->
        SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append)
    SMap.empty
    msig.requires

let require_set msig =
  let map = require_loc_map msig in
  SMap.fold (fun key _ acc -> SSet.add key acc) map SSet.empty

let add_require require msig errs =
  let requires = require :: msig.requires in
  ({ msig with requires }, errs)

let add_es_export loc source { module_kind; requires } errs =
  let requires =
    match source with
    | None -> requires
    | Some source -> ExportFrom { source } :: requires
  in
  match module_kind with
  | CommonJS { mod_exp_loc = Some _ } ->
    (* We still need to add requires so the dependency is recorded, because we
     * continue checking the file and will try to find the resolved module. *)
    let errs = IndeterminateModuleType loc :: errs in
    ({ module_kind; requires }, errs)
  | CommonJS _
  | ES ->
    ({ module_kind = ES; requires }, errs)

let set_cjs_exports mod_exp_loc msig errs =
  match msig.module_kind with
  | CommonJS { mod_exp_loc = original_mod_exp_loc } ->
    let mod_exp_loc = Base.Option.first_some original_mod_exp_loc (Some mod_exp_loc) in
    let module_kind = CommonJS { mod_exp_loc } in
    ({ msig with module_kind }, errs)
  | ES ->
    let err = IndeterminateModuleType mod_exp_loc in
    (msig, err :: errs)

(* Subclass of the AST visitor class that calculates requires and exports. Initializes with the
   scope builder class.
*)
class requires_exports_calculator ~ast ~opts =
  object (this)
    inherit [tolerable_t, Loc.t] visitor ~init:(empty, []) as super

    val scope_info : Scope_api.info =
      Scope_builder.program ~with_types:true ~enable_enums:opts.enable_enums ast

    (* This ensures that we do not add a `require` with no bindings to `requires` (when processing a
     * `call`) when we have already added that `require` with bindings (when processing a
     * `variable_declarator`). *)
    val mutable visited_requires_with_bindings : Loc.LSet.t = Loc.LSet.empty

    method private visited_requires_with_bindings loc bindings =
      bindings = None && Loc.LSet.mem loc visited_requires_with_bindings

    method private visit_requires_with_bindings loc bindings =
      if bindings <> None then
        visited_requires_with_bindings <- Loc.LSet.add loc visited_requires_with_bindings

    method private update_file_sig f =
      this#update_acc (fun (fsig, errs) ->
          let (fsig, errs) = f fsig errs in
          (fsig, errs)
      )

    method private add_require require = this#update_file_sig (add_require require)

    method private add_exports loc kind source =
      let open Ast.Statement in
      let add =
        match (kind, source) with
        | (ExportValue, _) -> add_es_export loc source
        | (ExportType, None) -> (fun msig errs -> (msig, errs))
        | (ExportType, Some source) -> add_require (ExportFrom { source })
      in
      this#update_file_sig add

    method private set_cjs_exports mod_exp_loc = this#update_file_sig (set_cjs_exports mod_exp_loc)

    method private add_cjs_export mod_exp_loc = this#update_file_sig (set_cjs_exports mod_exp_loc)

    method private add_tolerable_error (err : tolerable_error) =
      this#update_acc (fun (fsig, errs) -> (fsig, err :: errs))

    method! expression (expr : (Loc.t, Loc.t) Ast.Expression.t) =
      let open Ast.Expression in
      begin
        match expr with
        (* Disallow expressions consisting of `module` or `exports`. These are dangerous because they
         * can allow aliasing and mutation. *)
        | ( _,
            Identifier (loc, { Ast.Identifier.name = ("module" | "exports") as name; comments = _ })
          )
          when not (Scope_api.is_local_use scope_info loc) ->
          this#add_tolerable_error (BadExportContext (name, loc))
        | _ -> ()
      end;
      super#expression expr

    method! binary loc (expr : (Loc.t, Loc.t) Ast.Expression.Binary.t) =
      let open Ast.Expression in
      let open Ast.Expression.Binary in
      let is_module_or_exports = function
        | (_, Identifier (_, { Ast.Identifier.name = "module" | "exports"; comments = _ })) -> true
        | _ -> false
      in
      let is_legal_operator = function
        | StrictEqual
        | StrictNotEqual ->
          true
        | _ -> false
      in
      let identify_or_recurse subexpr =
        if not (is_module_or_exports subexpr) then ignore (this#expression subexpr)
      in
      let { operator; left; right; comments = _ } = expr in
      (* Allow e.g. `require.main === module` by avoiding the recursive calls (where the errors
       * are generated) if the AST matches specific patterns. *)
      if is_legal_operator operator then (
        identify_or_recurse left;
        identify_or_recurse right;
        expr
      ) else
        super#binary loc expr

    method! member loc (expr : (Loc.t, Loc.t) Ast.Expression.Member.t) =
      let open Ast.Expression in
      let open Ast.Expression.Member in
      let { _object; property; comments = _ } = expr in
      (* Strip the loc to simplify the patterns *)
      let (_, _object) = _object in
      (* This gets called when patterns like `module.id` appear on the LHS of an
       * assignment, in addition to when they appear in ordinary expression
       * locations. Therefore we have to prevent anything that would be dangerous
       * if it appeared on the LHS side of an assignment. Ordinary export
       * statements are handled by handle_assignment, which stops recursion so we
       * don't arrive here in those cases. *)
      begin
        match (_object, property) with
        (* Allow `module.anythingButExports` *)
        | ( Identifier (_, { Ast.Identifier.name = "module"; comments = _ }),
            PropertyIdentifier (_, { Ast.Identifier.name = prop; comments = _ })
          )
          when prop <> "exports" ->
          ()
        (* Allow `module.exports.whatever` -- this is safe because handle_assignment has already
         * looked for assignments to it before recursing down here. *)
        | ( Member
              {
                _object = (_, Identifier (_, { Ast.Identifier.name = "module"; comments = _ }));
                property = PropertyIdentifier (_, { Ast.Identifier.name = "exports"; comments = _ });
                _;
              },
            PropertyIdentifier _
          )
        (* Allow `exports.whatever`, for the same reason as above *)
        | (Identifier (_, { Ast.Identifier.name = "exports"; comments = _ }), PropertyIdentifier _)
          ->
          (* In these cases we don't know much about the property so we should recurse *)
          ignore (this#member_property property)
        | _ -> ignore (super#member loc expr)
      end;
      expr

    method! call call_loc (expr : (Loc.t, Loc.t) Ast.Expression.Call.t) =
      let open Ast.Expression in
      let { Call.callee; targs = _; arguments; comments = _ } = expr in
      this#handle_call call_loc callee arguments None;
      super#call call_loc expr

    method! literal loc (expr : (Loc.t, Loc.t) Ast.Literal.t) =
      let open Ast.Literal in
      this#handle_literal loc expr.value;
      super#literal loc expr

    method! tagged_template loc (expr : ('loc, 'loc) Ast.Expression.TaggedTemplate.t) =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi; comments = _ } = expr in
      match tag with
      | (_, Ast.Expression.Identifier (_, { Ast.Identifier.name = "graphql"; _ }))
        when opts.enable_relay_integration ->
        (match
           Graphql.extract_module_name ~module_prefix:opts.relay_integration_module_prefix quasi
         with
        | Ok module_name ->
          this#add_require
            (Require
               {
                 source = (loc, module_name);
                 require_loc = loc;
                 bindings = None;
                 prefix = opts.relay_integration_module_prefix;
               }
            );
          expr
        | Error _ -> expr)
      | _ -> super#tagged_template loc expr

    method! import import_loc (expr : (Loc.t, Loc.t) Ast.Expression.Import.t) =
      let open Ast.Expression in
      let { Import.argument; comments = _ } = expr in
      begin
        match argument with
        | ( loc,
            ( Literal { Ast.Literal.value = Ast.Literal.String name; _ }
            | TemplateLiteral
                {
                  TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          TemplateLiteral.Element.value =
                            { TemplateLiteral.Element.cooked = name; _ };
                          _;
                        }
                      );
                    ];
                  _;
                } )
          ) ->
          this#add_require (ImportDynamic { source = (loc, name); import_loc })
        | _ -> ()
      end;
      super#import import_loc expr

    method! import_declaration import_loc (decl : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t)
        =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default; comments = _ } = decl in
      let source =
        match source with
        | (loc, { Ast.StringLiteral.value = name; _ }) -> (loc, name)
      in
      let import =
        match (default, specifiers) with
        | (None, None) -> Import0 { source }
        | _ ->
          let named = ref SMap.empty in
          let ns = ref None in
          let types = ref SMap.empty in
          let typesof = ref SMap.empty in
          let typesof_ns = ref None in
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
          let set_ns local loc ref =
            if !ref = None then
              ref := Some (loc, local)
            else
              failwith "unreachable"
          in
          Base.Option.iter
            ~f:(fun (loc, { Ast.Identifier.name = local; comments = _ }) ->
              add_named
                "default"
                local
                { remote_loc = loc; local_loc = loc }
                (ref_of_kind import_kind))
            default;
          Base.Option.iter
            ~f:(function
              | ImportNamespaceSpecifier (_, (loc, { Ast.Identifier.name = local; comments = _ }))
                ->
                (match import_kind with
                | ImportType ->
                  (* this is a parse error. ignore it. *)
                  ()
                | ImportTypeof -> set_ns local loc typesof_ns
                | ImportValue -> set_ns local loc ns)
              | ImportNamedSpecifiers named_specifiers ->
                List.iter
                  (function
                    | { local; remote; kind } ->
                      let import_kind =
                        match kind with
                        | Some k -> k
                        | None -> import_kind
                      in
                      let (local_loc, { Ast.Identifier.name = local_name; comments = _ }) =
                        match local with
                        | Some x -> x
                        | None -> remote
                      in
                      let (remote_loc, { Ast.Identifier.name = remote_name; comments = _ }) =
                        remote
                      in
                      add_named
                        remote_name
                        local_name
                        { remote_loc; local_loc }
                        (ref_of_kind import_kind))
                  named_specifiers)
            specifiers;
          Import
            {
              import_loc;
              source;
              named = !named;
              ns = !ns;
              types = !types;
              typesof = !typesof;
              typesof_ns = !typesof_ns;
            }
      in
      this#add_require import;
      super#import_declaration import_loc decl

    method! export_default_declaration
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t) =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default = _; declaration = _; comments = _ } = decl in
      this#add_exports stmt_loc Ast.Statement.ExportValue None;
      super#export_default_declaration stmt_loc decl

    method! export_named_declaration
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportNamedDeclaration.t) =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind; source; specifiers; declaration; comments = _ } = decl in
      let source =
        match source with
        | Some (loc, { Ast.StringLiteral.value = mref; raw = _; comments = _ }) -> Some (loc, mref)
        | None -> None
      in
      begin
        match declaration with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc export_kind source
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc export_kind source
      end;
      super#export_named_declaration stmt_loc decl

    method! declare_module_exports
        loc (exports : (Loc.t, Loc.t) Ast.Statement.DeclareModuleExports.t) =
      this#set_cjs_exports loc;
      super#declare_module_exports loc exports

    method! declare_export_declaration
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.t) =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default = _; source; specifiers; declaration; comments = _ } = decl in
      let source =
        match source with
        | Some (loc, { Ast.StringLiteral.value = mref; raw = _; comments = _ }) -> Some (loc, mref)
        | _ -> None
      in
      begin
        match declaration with
        | None -> () (* assert specifiers <> None *)
        | Some declaration ->
          let open Ast.Statement in
          let export_kind =
            match declaration with
            | Variable _
            | Function _
            | Class _
            | Component _
            | DefaultType _
            | Enum _ ->
              ExportValue
            | NamedType _
            | NamedOpaqueType _
            | Interface _ ->
              ExportType
          in
          this#add_exports stmt_loc export_kind source
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc Ast.Statement.ExportValue source
      end;
      super#declare_export_declaration stmt_loc decl

    method! assignment loc (expr : (Loc.t, Loc.t) Ast.Expression.Assignment.t) =
      this#handle_assignment ~is_toplevel:false loc expr;
      expr

    method handle_assignment
        ~(is_toplevel : bool) loc (expr : (Loc.t, Loc.t) Ast.Expression.Assignment.t) =
      let open Ast.Expression in
      let open Ast.Expression.Assignment in
      let { operator; left; right; comments = _ } = expr in
      (* Handle exports *)
      match (operator, left) with
      (* module.exports = ... *)
      | ( None,
          ( mod_exp_loc,
            Ast.Pattern.Expression
              ( _,
                Member
                  {
                    Member._object =
                      (module_loc, Identifier (_, { Ast.Identifier.name = "module"; comments = _ }));
                    property =
                      Member.PropertyIdentifier
                        (_, { Ast.Identifier.name = "exports"; comments = _ });
                    _;
                  }
              )
          )
        )
        when not (Scope_api.is_local_use scope_info module_loc) ->
        this#handle_cjs_default_export module_loc mod_exp_loc;
        ignore (this#expression right);
        if not is_toplevel then this#add_tolerable_error (BadExportPosition mod_exp_loc)
      (* exports.foo = ... *)
      | ( None,
          ( _,
            Ast.Pattern.Expression
              ( _,
                Member
                  {
                    Member._object =
                      ( (mod_exp_loc as module_loc),
                        Identifier (_, { Ast.Identifier.name = "exports"; comments = _ })
                      );
                    property = Member.PropertyIdentifier _;
                    _;
                  }
              )
          )
        )
      (* module.exports.foo = ... *)
      | ( None,
          ( _,
            Ast.Pattern.Expression
              ( _,
                Member
                  {
                    Member._object =
                      ( mod_exp_loc,
                        Member
                          {
                            Member._object =
                              ( module_loc,
                                Identifier (_, { Ast.Identifier.name = "module"; comments = _ })
                              );
                            property =
                              Member.PropertyIdentifier
                                (_, { Ast.Identifier.name = "exports"; comments = _ });
                            _;
                          }
                      );
                    property = Member.PropertyIdentifier _;
                    _;
                  }
              )
          )
        )
        when not (Scope_api.is_local_use scope_info module_loc) ->
        this#add_cjs_export mod_exp_loc;
        ignore (this#expression right);
        if not is_toplevel then this#add_tolerable_error (BadExportPosition mod_exp_loc)
      (* module = ... *)
      | ( None,
          ( _,
            Ast.Pattern.Identifier
              {
                Ast.Pattern.Identifier.name =
                  (loc, { Ast.Identifier.name = ("exports" | "module") as id; comments = _ });
                _;
              }
          )
        )
        when not (Scope_api.is_local_use scope_info loc) ->
        ignore (this#expression right);
        this#add_tolerable_error (BadExportContext (id, loc))
      | _ -> ignore (super#assignment loc expr)

    method private handle_cjs_default_export module_loc mod_exp_loc =
      if not (Scope_api.is_local_use scope_info module_loc) then this#set_cjs_exports mod_exp_loc

    method! variable_declarator
        ~kind (decl : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
      begin
        match decl with
        | (_, { Ast.Statement.VariableDeclaration.Declarator.id; init = Some init }) ->
          this#handle_require id init
        | _ -> ()
      end;
      super#variable_declarator ~kind decl

    method private require_pattern (pattern : (Loc.t, Loc.t) Ast.Pattern.t) =
      match pattern with
      | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
        Some (BindIdent (Flow_ast_utils.source_of_ident name))
      | (_, Ast.Pattern.Object { Ast.Pattern.Object.properties; _ }) ->
        let named_bind =
          Base.List.fold_result properties ~init:[] ~f:(fun named prop ->
              match prop with
              | Ast.Pattern.Object.Property
                  ( _,
                    {
                      Ast.Pattern.Object.Property.key =
                        Ast.Pattern.Object.Property.Identifier remote;
                      pattern;
                      _;
                    }
                  ) ->
                (match this#require_pattern pattern with
                | Some bindings -> Ok ((Flow_ast_utils.source_of_ident remote, bindings) :: named)
                | None -> Error ())
              | _ -> Error ()
          )
        in
        (match named_bind with
        | Ok named_bind -> Some (BindNamed named_bind)
        | Error () -> None)
      | _ -> None

    method private handle_require
        (left : (Loc.t, Loc.t) Ast.Pattern.t) (right : (Loc.t, Loc.t) Ast.Expression.t) =
      let open Ast.Expression in
      let bindings = this#require_pattern left in
      match right with
      | (call_loc, Call { Call.callee; targs = _; arguments; comments = _ }) ->
        this#handle_call call_loc callee arguments bindings
      | _ -> ()

    method private handle_call call_loc callee arguments bindings =
      let open Ast.Expression in
      if not (this#visited_requires_with_bindings call_loc bindings) then (
        this#visit_requires_with_bindings call_loc bindings;
        match (callee, arguments) with
        | ( (_, Identifier (loc, { Ast.Identifier.name = "require"; comments = _ })),
            ( _,
              {
                Ast.Expression.ArgList.arguments =
                  [
                    Expression
                      ( source_loc,
                        ( Literal { Ast.Literal.value = Ast.Literal.String name; _ }
                        | TemplateLiteral
                            {
                              TemplateLiteral.quasis =
                                [
                                  ( _,
                                    {
                                      TemplateLiteral.Element.value =
                                        { TemplateLiteral.Element.cooked = name; _ };
                                      _;
                                    }
                                  );
                                ];
                              _;
                            } )
                      );
                  ];
                comments = _;
              }
            )
          ) ->
          if not (Scope_api.is_local_use scope_info loc) then
            this#add_require
              (Require
                 { source = (source_loc, name); require_loc = call_loc; bindings; prefix = None }
              )
        | _ -> ()
      )

    method private handle_literal loc lit =
      let open Ast.Literal in
      match lit with
      | ModuleRef { string_value; prefix_len; _ } ->
        let mref = Base.String.drop_prefix string_value prefix_len in
        let prefix =
          if prefix_len > 0 then
            Some (Base.String.prefix string_value prefix_len)
          else
            None
        in
        this#add_require
          (Require { source = (loc, mref); require_loc = loc; bindings = None; prefix })
      | _ -> ()

    (* skip declare module *)
    method! declare_module _loc (m : (Loc.t, Loc.t) Ast.Statement.DeclareModule.t) = m

    method! toplevel_statement_list (stmts : (Loc.t, Loc.t) Ast.Statement.t list) =
      let open Ast in
      let id = Flow_ast_mapper.id in
      let map_expression (expr : (Loc.t, Loc.t) Expression.t) =
        let open Expression in
        match expr with
        | (loc, Assignment assg) ->
          this#handle_assignment ~is_toplevel:true loc assg;
          expr
        | _ -> this#expression expr
      in
      let map_expression_statement (stmt : (Loc.t, Loc.t) Statement.Expression.t) =
        Statement.Expression.(
          let { expression; _ } = stmt in
          id map_expression expression stmt (fun expr -> { stmt with expression = expr })
        )
      in
      let map_statement (stmt : (Loc.t, Loc.t) Statement.t) =
        let open Statement in
        match stmt with
        | (loc, Expression expr) ->
          id map_expression_statement expr stmt (fun expr -> (loc, Expression expr))
        | _ -> this#statement stmt
      in
      ListUtils.ident_map map_statement stmts
  end

(* Now that we've determined the kind of the export, we can filter out
   BadExportPosition and BadExportContext from ES modules. These errors are only
   relevant for CommonJS modules, since their goal is to capture aliasing of
   `module` and `module.exports`. For ES modules these uses should be allowed,
   so we discard these errors to allow more flexibility. Note that this pass only
   accummulates BadExportPosition and BadExportContext errors. *)
let filter_irrelevant_errors ~module_kind tolerable_errors =
  match module_kind with
  | CommonJS _ -> tolerable_errors
  | ES ->
    List.filter
      (function
        | BadExportPosition _
        | BadExportContext _ ->
          false
        | _ -> true)
      tolerable_errors

let program ~ast ~opts =
  let walk = new requires_exports_calculator ~ast ~opts in
  let (fsig, tolerable_errors) = walk#eval walk#program ast in
  let module_kind = fsig.module_kind in
  (fsig, filter_irrelevant_errors ~module_kind tolerable_errors)
