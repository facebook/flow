(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
open Flow_ast_visitor

module Make
    (L : Loc_sig.S)
    (Scope_api : Scope_api_sig.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L and module Api = Scope_api) =
struct
  module L = L

  type t = {
    module_sig: module_sig;
    declare_modules: (L.t * module_sig) SMap.t;
  }

  and options = {
    module_ref_prefix: string option;
    enable_enums: bool;
    enable_relay_integration: bool;
    relay_integration_module_prefix: string option;
  }

  and module_sig = {
    requires: require list;
    module_kind: module_kind;
  }

  and require =
    | Require of {
        source: L.t Ast_utils.source;
        require_loc: L.t;
        bindings: require_bindings option;
      }
    | ImportDynamic of {
        source: L.t Ast_utils.source;
        import_loc: L.t;
      }
    | Import0 of { source: L.t Ast_utils.source }
    | Import of {
        import_loc: L.t;
        source: L.t Ast_utils.source;
        named: imported_locs Nel.t SMap.t SMap.t;
        ns: L.t Ast_utils.ident option;
        types: imported_locs Nel.t SMap.t SMap.t;
        typesof: imported_locs Nel.t SMap.t SMap.t;
        typesof_ns: L.t Ast_utils.ident option;
      }
    | ExportFrom of { source: L.t Flow_ast_utils.source }

  and imported_locs = {
    remote_loc: L.t;
    local_loc: L.t;
  }

  and require_bindings =
    | BindIdent of L.t Ast_utils.ident
    | BindNamed of (L.t Ast_utils.ident * require_bindings) list

  and module_kind =
    | CommonJS of { mod_exp_loc: L.t option }
    | ES
  [@@deriving show]

  type tolerable_error =
    | IndeterminateModuleType of L.t
    (* e.g. `module.exports.foo = 4` when not at the top level *)
    | BadExportPosition of L.t
    (* e.g. `foo(module)`, dangerous because `module` is aliased *)
    | BadExportContext of string (* offending identifier *) * L.t
    | SignatureVerificationError of L.t Signature_error.t
  [@@deriving show]

  type tolerable_t = t * tolerable_error list

  let empty_module_sig = { requires = []; module_kind = CommonJS { mod_exp_loc = None } }

  let empty = { module_sig = empty_module_sig; declare_modules = SMap.empty }

  let default_opts =
    {
      module_ref_prefix = None;
      enable_relay_integration = false;
      enable_enums = false;
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
    let string_of_module_sig module_sig =
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
          ("requires", string_of_require_list module_sig.requires);
          ("module_kind", string_of_module_kind module_sig.module_kind);
        ]
    in
    PP.items_to_record_string 0 [("module_sig", string_of_module_sig t.module_sig)]

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

  let add_declare_module name m loc fsig =
    { fsig with declare_modules = SMap.add name (loc, m) fsig.declare_modules }

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
      inherit [tolerable_t, L.t] visitor ~init:(empty, []) as super

      val scope_info : Scope_api.info =
        Scope_builder.program ~with_types:true ~enable_enums:opts.enable_enums ast

      val mutable curr_declare_module : module_sig option = None

      (* This ensures that we do not add a `require` with no bindings to `module_sig.requires` (when
       * processing a `call`) when we have already added that `require` with bindings (when processing
       * a `variable_declarator`). *)
      val mutable visited_requires_with_bindings : L.LSet.t = L.LSet.empty

      method private visited_requires_with_bindings loc bindings =
        bindings = None && L.LSet.mem loc visited_requires_with_bindings

      method private visit_requires_with_bindings loc bindings =
        if bindings <> None then
          visited_requires_with_bindings <- L.LSet.add loc visited_requires_with_bindings

      method private update_module_sig f =
        this#update_acc (fun (fsig, errs) ->
            match curr_declare_module with
            | Some msig ->
              let (msig, errs) = f msig errs in
              curr_declare_module <- Some msig;
              (fsig, errs)
            | None ->
              let (module_sig, errs) = f fsig.module_sig errs in
              ({ fsig with module_sig }, errs)
        )

      method private add_require require = this#update_module_sig (add_require require)

      method private add_exports loc kind source =
        let open Ast.Statement in
        let add =
          match (kind, source) with
          | (ExportValue, _) -> add_es_export loc source
          | (ExportType, None) -> (fun msig errs -> (msig, errs))
          | (ExportType, Some source) -> add_require (ExportFrom { source })
        in
        this#update_module_sig add

      method private set_cjs_exports mod_exp_loc =
        this#update_module_sig (set_cjs_exports mod_exp_loc)

      method private add_cjs_export mod_exp_loc =
        this#update_module_sig (set_cjs_exports mod_exp_loc)

      method private add_tolerable_error (err : tolerable_error) =
        this#update_acc (fun (fsig, errs) -> (fsig, err :: errs))

      method! expression (expr : (L.t, L.t) Ast.Expression.t) =
        let open Ast.Expression in
        begin
          match expr with
          (* Disallow expressions consisting of `module` or `exports`. These are dangerous because they
           * can allow aliasing and mutation. *)
          | ( _,
              Identifier
                (loc, { Ast.Identifier.name = ("module" | "exports") as name; comments = _ })
            )
            when not (Scope_api.is_local_use scope_info loc) ->
            this#add_tolerable_error (BadExportContext (name, loc))
          | _ -> ()
        end;
        super#expression expr

      method! binary loc (expr : (L.t, L.t) Ast.Expression.Binary.t) =
        let open Ast.Expression in
        let open Ast.Expression.Binary in
        let is_module_or_exports = function
          | (_, Identifier (_, { Ast.Identifier.name = "module" | "exports"; comments = _ })) ->
            true
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

      method! member loc (expr : (L.t, L.t) Ast.Expression.Member.t) =
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
                  property =
                    PropertyIdentifier (_, { Ast.Identifier.name = "exports"; comments = _ });
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

      method! call call_loc (expr : (L.t, L.t) Ast.Expression.Call.t) =
        let open Ast.Expression in
        let { Call.callee; targs = _; arguments; comments = _ } = expr in
        this#handle_call call_loc callee arguments None;
        super#call call_loc expr

      method! literal loc (expr : L.t Ast.Literal.t) =
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
              (Require { source = (loc, module_name); require_loc = loc; bindings = None });
            expr
          | Error _ -> expr)
        | _ -> super#tagged_template loc expr

      method! import import_loc (expr : (L.t, L.t) Ast.Expression.Import.t) =
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

      method! import_declaration import_loc (decl : (L.t, L.t) Ast.Statement.ImportDeclaration.t) =
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
          stmt_loc (decl : (L.t, L.t) Ast.Statement.ExportDefaultDeclaration.t) =
        let open Ast.Statement.ExportDefaultDeclaration in
        let { default = _; declaration = _; comments = _ } = decl in
        this#add_exports stmt_loc Ast.Statement.ExportValue None;
        super#export_default_declaration stmt_loc decl

      method! export_named_declaration
          stmt_loc (decl : (L.t, L.t) Ast.Statement.ExportNamedDeclaration.t) =
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

      method! declare_module_exports loc (exports : (L.t, L.t) Ast.Statement.DeclareModuleExports.t)
          =
        this#set_cjs_exports loc;
        super#declare_module_exports loc exports

      method! declare_export_declaration
          stmt_loc (decl : (L.t, L.t) Ast.Statement.DeclareExportDeclaration.t) =
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

      method! assignment loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
        this#handle_assignment ~is_toplevel:false loc expr;
        expr

      method handle_assignment
          ~(is_toplevel : bool) loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
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
                        ( module_loc,
                          Identifier (_, { Ast.Identifier.name = "module"; comments = _ })
                        );
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
          (* expressions not allowed in declare module body *)
          assert (curr_declare_module = None);
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
        (* expressions not allowed in declare module body *)
        assert (curr_declare_module = None);
        if not (Scope_api.is_local_use scope_info module_loc) then this#set_cjs_exports mod_exp_loc

      method! variable_declarator
          ~kind (decl : (L.t, L.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        begin
          match decl with
          | (_, { Ast.Statement.VariableDeclaration.Declarator.id; init = Some init }) ->
            this#handle_require id init
          | _ -> ()
        end;
        super#variable_declarator ~kind decl

      method private require_pattern (pattern : (L.t, L.t) Ast.Pattern.t) =
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
          (left : (L.t, L.t) Ast.Pattern.t) (right : (L.t, L.t) Ast.Expression.t) =
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
                (Require { source = (source_loc, name); require_loc = call_loc; bindings })
          | _ -> ()
        )

      method private handle_literal loc lit =
        let open Ast.Literal in
        match opts.module_ref_prefix with
        | Some prefix -> begin
          match lit with
          | String s when String.starts_with ~prefix s ->
            this#add_require
              (Require
                 {
                   source = (loc, String_utils.lstrip s prefix);
                   require_loc = loc;
                   bindings = None;
                 }
              )
          | _ -> ()
        end
        | None -> ()

      method! declare_module loc (m : (L.t, L.t) Ast.Statement.DeclareModule.t) =
        let name =
          let open Ast.Statement.DeclareModule in
          match m.id with
          | Identifier (_, { Ast.Identifier.name; comments = _ }) -> name
          | Literal (_, { Ast.StringLiteral.value; _ }) -> value
        in
        curr_declare_module <- Some empty_module_sig;
        let ret = super#declare_module loc m in
        begin
          match curr_declare_module with
          | None -> failwith "lost curr_declare_module"
          | Some m ->
            this#update_acc (fun (fsig, errs) -> (add_declare_module name m loc fsig, errs))
        end;
        curr_declare_module <- None;
        ret

      method! toplevel_statement_list (stmts : (L.t, L.t) Ast.Statement.t list) =
        let open Ast in
        let id = Flow_ast_mapper.id in
        let map_expression (expr : (L.t, L.t) Expression.t) =
          let open Expression in
          match expr with
          | (loc, Assignment assg) ->
            this#handle_assignment ~is_toplevel:true loc assg;
            expr
          | _ -> this#expression expr
        in
        let map_expression_statement (stmt : (L.t, L.t) Statement.Expression.t) =
          Statement.Expression.(
            let { expression; _ } = stmt in
            id map_expression expression stmt (fun expr -> { stmt with expression = expr })
          )
        in
        let map_statement (stmt : (L.t, L.t) Statement.t) =
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
    let (exports, tolerable_errors) = walk#eval walk#program ast in
    let module_kind = exports.module_sig.module_kind in
    (exports, filter_irrelevant_errors ~module_kind tolerable_errors)

  class mapper =
    object (this)
      method file_sig (file_sig : t) =
        let { module_sig; declare_modules } = file_sig in
        let module_sig' = this#module_sig module_sig in
        let declare_modules' =
          SMapUtils.ident_map
            (fun (loc, module_sig) ->
              let loc = this#loc loc in
              let module_sig = this#module_sig module_sig in
              (loc, module_sig))
            declare_modules
        in
        if module_sig == module_sig' && declare_modules == declare_modules' then
          file_sig
        else
          { module_sig = module_sig'; declare_modules = declare_modules' }

      method module_sig (module_sig : module_sig) =
        let { requires; module_kind } = module_sig in
        let requires' = ListUtils.ident_map this#require requires in
        let module_kind' = this#module_kind module_kind in
        if requires == requires' && module_kind == module_kind' then
          module_sig
        else
          { requires = requires'; module_kind = module_kind' }

      method require (require : require) =
        match require with
        | Require { source; require_loc; bindings } ->
          let source' = this#source source in
          let require_loc' = this#loc require_loc in
          let bindings' = OptionUtils.ident_map this#require_bindings bindings in
          if source == source' && require_loc == require_loc' && bindings == bindings' then
            require
          else
            Require { source = source'; require_loc = require_loc'; bindings = bindings' }
        | ImportDynamic { source; import_loc } ->
          let source' = this#source source in
          let import_loc' = this#loc import_loc in
          if source == source' && import_loc == import_loc' then
            require
          else
            ImportDynamic { source = source'; import_loc = import_loc' }
        | Import0 { source } ->
          let source' = this#source source in
          if source == source' then
            require
          else
            Import0 { source = source' }
        | Import { import_loc; source; named; ns; types; typesof; typesof_ns } ->
          let import_loc' = this#loc import_loc in
          let source' = this#source source in
          let named' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) named
          in
          let ns' = OptionUtils.ident_map this#ident ns in
          let types' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) types
          in
          let typesof' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) typesof
          in
          let typesof_ns' = OptionUtils.ident_map this#ident typesof_ns in
          if
            import_loc == import_loc'
            && source == source'
            && named == named'
            && ns == ns'
            && types == types'
            && typesof == typesof'
            && typesof_ns == typesof_ns'
          then
            require
          else
            Import
              {
                import_loc = import_loc';
                source = source';
                named = named';
                ns = ns';
                types = types';
                typesof = typesof';
                typesof_ns = typesof_ns';
              }
        | ExportFrom { source } ->
          let source' = this#source source in
          if source == source' then
            require
          else
            ExportFrom { source = source' }

      method imported_locs (imported_locs : imported_locs) =
        let { remote_loc; local_loc } = imported_locs in
        let remote_loc' = this#loc remote_loc in
        let local_loc' = this#loc local_loc in
        if remote_loc == remote_loc' && local_loc == local_loc' then
          imported_locs
        else
          { remote_loc = remote_loc'; local_loc = local_loc' }

      method require_bindings (require_bindings : require_bindings) =
        match require_bindings with
        | BindIdent ident ->
          let ident' = this#ident ident in
          if ident == ident' then
            require_bindings
          else
            BindIdent ident'
        | BindNamed named ->
          let named' =
            ListUtils.ident_map
              (fun ((remote, require_bindings) as x) ->
                let remote' = this#ident remote in
                let require_bindings' = this#require_bindings require_bindings in
                if remote == remote' && require_bindings == require_bindings' then
                  x
                else
                  (remote', require_bindings'))
              named
          in
          if named == named' then
            require_bindings
          else
            BindNamed named'

      method module_kind (module_kind : module_kind) =
        match module_kind with
        | CommonJS { mod_exp_loc } ->
          let mod_exp_loc' = OptionUtils.ident_map this#loc mod_exp_loc in
          if mod_exp_loc == mod_exp_loc' then
            module_kind
          else
            CommonJS { mod_exp_loc = mod_exp_loc' }
        | ES -> ES

      method ident (ident : L.t Ast_utils.ident) =
        let (loc, str) = ident in
        let loc' = this#loc loc in
        if loc == loc' then
          ident
        else
          (loc', str)

      method source (source : L.t Ast_utils.source) =
        let (loc, str) = source in
        let loc' = this#loc loc in
        if loc == loc' then
          source
        else
          (loc', str)

      method tolerable_error (tolerable_error : tolerable_error) =
        match tolerable_error with
        | IndeterminateModuleType loc ->
          let loc' = this#loc loc in
          if loc == loc' then
            tolerable_error
          else
            IndeterminateModuleType loc'
        | BadExportPosition loc ->
          let loc' = this#loc loc in
          if loc == loc' then
            tolerable_error
          else
            BadExportPosition loc'
        | BadExportContext (str, loc) ->
          let loc' = this#loc loc in
          if loc == loc' then
            tolerable_error
          else
            BadExportContext (str, loc')
        | SignatureVerificationError sve ->
          Signature_error.(
            begin
              match sve with
              | ExpectedAnnotation (loc, sort) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (ExpectedAnnotation (loc', sort))
              | UnexpectedObjectKey (loc, key_loc) ->
                let loc' = this#loc loc in
                let key_loc' = this#loc key_loc in
                if loc == loc' && key_loc == key_loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedObjectKey (loc', key_loc'))
              | UnexpectedArraySpread (loc, spread_loc) ->
                let loc' = this#loc loc in
                let spread_loc' = this#loc spread_loc in
                if loc == loc' && spread_loc == spread_loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedArraySpread (loc', spread_loc'))
              | UnexpectedArrayHole loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedArrayHole loc')
              | EmptyArray loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (EmptyArray loc')
              | EmptyObject loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (EmptyObject loc')
              | UnexpectedExpression (loc, esort) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedExpression (loc', esort))
            end
          )

      method loc (loc : L.t) = loc
    end
end

module With_Loc = Make (Loc_sig.LocS) (Scope_api.With_Loc) (Scope_builder.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Scope_api.With_ALoc) (Scope_builder.With_ALoc)

let abstractify_tolerable_errors =
  let module WL = With_Loc in
  let module WA = With_ALoc in
  let abstractify_tolerable_error = function
    | WL.IndeterminateModuleType loc -> WA.IndeterminateModuleType (ALoc.of_loc loc)
    | WL.BadExportPosition loc -> WA.BadExportPosition (ALoc.of_loc loc)
    | WL.BadExportContext (name, loc) -> WA.BadExportContext (name, ALoc.of_loc loc)
    | WL.SignatureVerificationError err ->
      WA.SignatureVerificationError (Signature_error.map ALoc.of_loc err)
  in
  Base.List.map ~f:abstractify_tolerable_error

let abstractify_locs : With_Loc.t -> With_ALoc.t =
  let module WL = With_Loc in
  let module WA = With_ALoc in
  let abstractify_fst (loc, x) = (ALoc.of_loc loc, x) in
  let abstractify_imported_locs { WL.remote_loc; local_loc } =
    { WA.remote_loc = ALoc.of_loc remote_loc; local_loc = ALoc.of_loc local_loc }
  in
  let abstractify_imported_locs_map = SMap.map (SMap.map (Nel.map abstractify_imported_locs)) in
  let rec abstractify_require_bindings = function
    | WL.BindIdent x -> WA.BindIdent (abstractify_fst x)
    | WL.BindNamed named ->
      WA.BindNamed
        (Base.List.map
           ~f:(fun (remote, require_bindings) ->
             (abstractify_fst remote, abstractify_require_bindings require_bindings))
           named
        )
  in
  let abstractify_require = function
    | WL.Require { source; require_loc; bindings } ->
      WA.Require
        {
          source = abstractify_fst source;
          require_loc = ALoc.of_loc require_loc;
          bindings = Base.Option.map ~f:abstractify_require_bindings bindings;
        }
    | WL.ImportDynamic { source; import_loc } ->
      WA.ImportDynamic { source = abstractify_fst source; import_loc = ALoc.of_loc import_loc }
    | WL.Import0 { source } -> WA.Import0 { source = abstractify_fst source }
    | WL.Import { import_loc; source; named; ns; types; typesof; typesof_ns } ->
      WA.Import
        {
          import_loc = ALoc.of_loc import_loc;
          source = abstractify_fst source;
          named = abstractify_imported_locs_map named;
          ns = Base.Option.map ~f:abstractify_fst ns;
          types = abstractify_imported_locs_map types;
          typesof = abstractify_imported_locs_map typesof;
          typesof_ns = Base.Option.map ~f:abstractify_fst typesof_ns;
        }
    | WL.ExportFrom { source } -> WA.ExportFrom { source = abstractify_fst source }
  in
  let abstractify_requires = Base.List.map ~f:abstractify_require in
  let abstractify_module_kind = function
    | WL.CommonJS { mod_exp_loc } ->
      WA.CommonJS { mod_exp_loc = Base.Option.map ~f:ALoc.of_loc mod_exp_loc }
    | WL.ES -> WA.ES
  in
  let abstractify_module_sig { WL.requires; module_kind } =
    {
      WA.requires = abstractify_requires requires;
      module_kind = abstractify_module_kind module_kind;
    }
  in
  let abstractify_declare_modules =
    SMap.map (fun (loc, module_sig) -> (ALoc.of_loc loc, abstractify_module_sig module_sig))
  in
  fun { WL.module_sig; declare_modules } ->
    {
      WA.module_sig = abstractify_module_sig module_sig;
      declare_modules = abstractify_declare_modules declare_modules;
    }

let abstractify (t, tolerable_errors) =
  (abstractify_locs t, abstractify_tolerable_errors tolerable_errors)
