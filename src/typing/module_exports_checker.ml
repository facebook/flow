(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor
module Scope_api = Scope_api.With_ALoc
module Scope_builder = Scope_builder.With_ALoc
module Loc = Loc_sig.ALocS

type module_kind =
  | Unknown
  | CommonJS
  | ES

type state = module_kind * Error_message.t list

let add_es_export loc module_kind errs =
  match module_kind with
  | CommonJS ->
    let errs = Error_message.EIndeterminateModuleType loc :: errs in
    (module_kind, errs)
  | Unknown
  | ES ->
    (ES, errs)

let set_cjs_exports mod_exp_loc module_kind errs =
  match module_kind with
  | Unknown
  | CommonJS ->
    (CommonJS, errs)
  | ES ->
    let err = Error_message.EIndeterminateModuleType mod_exp_loc in
    (module_kind, err :: errs)

class exports_error_checker ~is_local_use =
  object (this)
    inherit [state, ALoc.t] visitor ~init:(Unknown, []) as super

    method private update_file_sig f =
      this#update_acc (fun (fsig, errs) ->
          let (fsig, errs) = f fsig errs in
          (fsig, errs)
      )

    method private add_exports loc kind =
      let open Ast.Statement in
      match kind with
      | ExportValue -> this#update_file_sig (add_es_export loc)
      | ExportType -> ()

    method private set_cjs_exports mod_exp_loc = this#update_file_sig (set_cjs_exports mod_exp_loc)

    method private add_cjs_export mod_exp_loc = this#update_file_sig (set_cjs_exports mod_exp_loc)

    method private add_error err = this#update_acc (fun (fsig, errs) -> (fsig, err :: errs))

    method! expression (expr : (ALoc.t, ALoc.t) Ast.Expression.t) =
      let open Ast.Expression in
      begin
        match expr with
        (* Disallow expressions consisting of `module` or `exports`. These are dangerous because they
         * can allow aliasing and mutation. *)
        | ( _,
            Identifier (loc, { Ast.Identifier.name = ("module" | "exports") as name; comments = _ })
          )
          when not (is_local_use loc) ->
          this#add_error (Error_message.EBadExportContext (name, loc))
        | _ -> ()
      end;
      super#expression expr

    method! binary loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Binary.t) =
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

    method! member loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Member.t) =
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

    method! export_default_declaration
        stmt_loc (decl : (ALoc.t, ALoc.t) Ast.Statement.ExportDefaultDeclaration.t) =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default = _; declaration = _; comments = _ } = decl in
      this#add_exports stmt_loc Ast.Statement.ExportValue;
      super#export_default_declaration stmt_loc decl

    method! export_named_declaration
        stmt_loc (decl : (ALoc.t, ALoc.t) Ast.Statement.ExportNamedDeclaration.t) =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind; source = _; specifiers; declaration; comments = _ } = decl in
      begin
        match declaration with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc export_kind
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc export_kind
      end;
      super#export_named_declaration stmt_loc decl

    method! declare_module_exports
        loc (exports : (ALoc.t, ALoc.t) Ast.Statement.DeclareModuleExports.t) =
      this#set_cjs_exports loc;
      super#declare_module_exports loc exports

    method! declare_export_declaration
        stmt_loc (decl : (ALoc.t, ALoc.t) Ast.Statement.DeclareExportDeclaration.t) =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default = _; source = _; specifiers; declaration; comments = _ } = decl in
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
          this#add_exports stmt_loc export_kind
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports stmt_loc Ast.Statement.ExportValue
      end;
      super#declare_export_declaration stmt_loc decl

    method! assignment loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
      this#handle_assignment ~is_toplevel:false loc expr;
      expr

    method handle_assignment ~is_toplevel loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t)
        =
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
        when not (is_local_use module_loc) ->
        if not (is_local_use module_loc) then this#set_cjs_exports mod_exp_loc;
        ignore (this#expression right);
        if not is_toplevel then this#add_error (Error_message.EBadExportPosition mod_exp_loc)
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
        when not (is_local_use module_loc) ->
        this#add_cjs_export mod_exp_loc;
        ignore (this#expression right);
        if not is_toplevel then this#add_error (Error_message.EBadExportPosition mod_exp_loc)
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
        when not (is_local_use loc) ->
        ignore (this#expression right);
        this#add_error (Error_message.EBadExportContext (id, loc))
      | _ -> ignore (super#assignment loc expr)

    method private check_declare_module_or_declare_namespace_body body =
      let f module_kind (loc, stmt) =
        let open Ast.Statement in
        match (module_kind, stmt) with
        (*
         * The first time we see either a `declare export` or a
         * `declare module.exports`, we lock in the kind of the module.
         *
         * `declare export type` and `declare export interface` are the two
         * exceptions to this rule because they are valid in both CommonJS
         * and ES modules (and thus do not indicate an intent for either).
         *)
        | (Unknown, DeclareModuleExports _) -> CommonJS
        | (Unknown, DeclareExportDeclaration { DeclareExportDeclaration.declaration; _ }) ->
          (match declaration with
          | Some (DeclareExportDeclaration.NamedType _)
          | Some (DeclareExportDeclaration.Interface _) ->
            module_kind
          | _ -> ES)
        (* We allow more than one `declare module.exports` statement *)
        | (CommonJS, DeclareModuleExports _) -> module_kind
        (*
         * It's never ok to mix and match `declare export` and
         * `declare module.exports` in the same module because it leaves the
         * kind of the module (CommonJS vs ES) ambiguous.
         *
         * The 1 exception to this rule is that `export type/interface` are
         * both ok in CommonJS modules.
         *)
        | (ES, DeclareModuleExports _) ->
          this#add_error (Error_message.EIndeterminateModuleType loc);
          module_kind
        | (CommonJS, DeclareExportDeclaration { DeclareExportDeclaration.declaration; _ }) ->
          (match declaration with
          | Some (DeclareExportDeclaration.NamedType _)
          | Some (DeclareExportDeclaration.Interface _) ->
            ()
          | _ -> this#add_error (Error_message.EIndeterminateModuleType loc));
          module_kind
        | _ -> module_kind
      in
      let _module_kind : module_kind = Base.List.fold body ~init:Unknown ~f in
      ()

    (* skip declare module for other visits designed for toplevel exports only *)
    method! declare_module _loc (m : (ALoc.t, ALoc.t) Ast.Statement.DeclareModule.t) =
      let { Ast.Statement.DeclareModule.body = (_, { Ast.Statement.Block.body; _ }); _ } = m in
      this#check_declare_module_or_declare_namespace_body body;
      m

    (* skip declare namespace for other visits designed for toplevel exports only *)
    method! declare_namespace _loc (ns : (ALoc.t, ALoc.t) Ast.Statement.DeclareNamespace.t) =
      let { Ast.Statement.DeclareNamespace.body = (_, { Ast.Statement.Block.body; _ }); _ } = ns in
      this#check_declare_module_or_declare_namespace_body body;
      ns

    method! toplevel_statement_list (stmts : (ALoc.t, ALoc.t) Ast.Statement.t list) =
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
   BadExportContext from ES modules. These errors are only
   relevant for CommonJS modules, since their goal is to capture aliasing of
   `module` and `module.exports`. For ES modules these uses should be allowed,
   so we discard these errors to allow more flexibility. Note that this pass only
   accummulates BadExportContext errors. *)
let filter_irrelevant_errors ~module_kind errors =
  match module_kind with
  | Unknown
  | CommonJS ->
    errors
  | ES ->
    Base.List.filter errors ~f:(function
        | Error_message.EBadExportPosition _ -> false
        | Error_message.EBadExportContext _ -> false
        | _ -> true
        )

let check_program ast =
  let scope_info = Scope_builder.program ~with_types:true ~enable_enums:true ast in
  let walk = new exports_error_checker ~is_local_use:(Scope_api.is_local_use scope_info) in
  let (module_kind, errors) = walk#eval walk#program ast in
  filter_irrelevant_errors ~module_kind errors
