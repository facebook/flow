(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor

(* Hoister class. Does a shallow visit of statements, looking for binding
   declarations (currently, variable declarations, parameters, and function
   declarations) and recording the corresponding bindings in a list. The list
   can have duplicates, which are handled elsewhere.

   TODO: Ideally implemented as a fold, not a map.
*)
(* TODO: It should be possible to vastly simplify hoisting by overriding the
   general method `pattern_identifier ?kind` for kind = Var | Let | Const that
   was recently introduced to distinguish bindings and assignments from other
   occurrences (`identifier`).

   Instead, it is implemented below by overriding several specific methods that
   are known to introduce bindings. The logic here is sufficiently tricky that
   we probably should not change it without extensive testing. *)

class ['loc] lexical_hoister ~flowmin_compatibility =
  object (this)
    inherit ['loc Bindings.t, 'loc] visitor ~init:Bindings.empty as super

    val mutable let_kind = Bindings.Let

    method private add_let_binding ?kind entry =
      let kind = Base.Option.value kind ~default:let_kind in
      this#update_acc Bindings.(add (entry, kind))

    method private add_const_binding ?kind entry =
      let kind = Base.Option.value kind ~default:Bindings.Const in
      this#update_acc Bindings.(add (entry, kind))

    method private add_function_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.Function))

    (* Ignore all statements except variable declarations, class declarations, and
       import declarations. The ignored statements cannot contain lexical
       bindings in the current scope. *)
    method! statement (stmt : ('loc, 'loc) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (_, VariableDeclaration _)
      | (_, ClassDeclaration _)
      | (_, DeclareClass _)
      | (_, EnumDeclaration _)
      | (_, ExportNamedDeclaration _)
      | (_, ExportDefaultDeclaration _)
      | (_, ImportDeclaration _)
      | (_, Labeled _) ->
        super#statement stmt
      | (_, FunctionDeclaration _)
      | (_, DeclareFunction _) ->
        this#flowmin_compatibility_statement stmt
      | _ -> this#nonlexical_statement stmt

    method flowmin_compatibility_statement stmt =
      (* Flowmin treats function declarations as vars, even though
         they're actually lets *)
      if flowmin_compatibility then
        this#nonlexical_statement stmt
      else
        super#statement stmt

    method nonlexical_statement stmt = stmt

    (* Base class statement, needed for subclass *)
    method base_statement = super#statement

    (* Ignore expressions. This includes, importantly, initializers of variable
       declarations. *)
    method! expression (expr : ('loc, 'loc) Ast.Expression.t) = expr

    (* This is visited by variable declarations, as well as other kinds of
       patterns that we ignore. *)
    method! pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      match kind with
      | None -> expr
      | Some (Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const) ->
        let open Ast.Pattern in
        let add_binding =
          match kind with
          | Some Ast.Statement.VariableDeclaration.Let -> this#add_let_binding ?kind:None
          | Some Ast.Statement.VariableDeclaration.Const -> this#add_const_binding ?kind:None
          | _ -> Utils_js.assert_false "Only lets and consts allowed"
        in
        let (_, patt) = expr in
        begin
          match patt with
          | Identifier { Identifier.name; _ } -> add_binding name
          | Object _
          | Array _ ->
            run (super#pattern ?kind) expr
          | _ -> ()
        end;
        expr
      | Some Ast.Statement.VariableDeclaration.Var -> expr

    method base_pattern = super#pattern

    method! function_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      let old_lkind = let_kind in
      let_kind <- Bindings.Parameter;
      let res = this#binding_pattern ~kind:Ast.Statement.VariableDeclaration.Let expr in
      let_kind <- old_lkind;
      res

    method! catch_clause_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      let old_lkind = let_kind in
      let_kind <- Bindings.CatchParameter;
      let res = this#binding_pattern ~kind:Ast.Statement.VariableDeclaration.Let expr in
      let_kind <- old_lkind;
      res

    method! class_ _loc (cls : ('loc, 'loc) Ast.Class.t) =
      let open Ast.Class in
      let {
        id;
        body = _;
        tparams = _;
        extends = _;
        implements = _;
        class_decorators = _;
        comments = _;
      } =
        cls
      in
      begin
        match id with
        | Some name -> this#add_let_binding ~kind:Bindings.Class name
        | None -> ()
      end;
      cls

    method! function_declaration _loc (expr : ('loc, 'loc) Ast.Function.t) =
      let open Ast.Function in
      let { id; _ } = expr in
      begin
        match id with
        | Some name -> this#add_function_binding name
        | None -> ()
      end;
      expr

    method! declare_class loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      this#add_let_binding ~kind:Bindings.Class decl.id;
      super#declare_class loc decl

    method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      let open Ast.Statement.DeclareFunction in
      match Declare_function_utils.declare_function_to_function_declaration_simple loc decl with
      | Some stmt ->
        let _ = this#statement (loc, stmt) in
        decl
      | None ->
        this#add_function_binding decl.id;
        super#declare_function loc decl

    method! enum_declaration _loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id; _ } = enum in
      this#add_const_binding ~kind:Bindings.Enum id;
      enum
  end

class ['loc] hoister ~flowmin_compatibility ~with_types =
  object (this)
    inherit ['loc] lexical_hoister ~flowmin_compatibility as super

    val mutable lexical = true

    method private add_var_binding entry = this#update_acc Bindings.(add (entry, Bindings.Var))

    method private add_type_binding entry = this#update_acc Bindings.(add (entry, Bindings.Type))

    method! private add_const_binding ?kind entry =
      if lexical then super#add_const_binding ?kind entry

    method! private add_let_binding ?kind entry = if lexical then super#add_let_binding ?kind entry

    method! flowmin_compatibility_statement = this#base_statement

    method! nonlexical_statement stmt =
      let cur_lex = lexical in
      lexical <- false;
      let res = super#base_statement stmt in
      lexical <- cur_lex;
      res

    (* This is visited by function parameters, variable declarations, and catch patterns (but not
       assignment expressions). *)
    method! pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      let open Ast.Pattern in
      let (_, patt) = expr in
      match kind with
      | Some Ast.Statement.VariableDeclaration.Var ->
        begin
          match patt with
          | Identifier { Identifier.name; _ } -> this#add_var_binding name
          | Object _
          | Array _ ->
            run (super#base_pattern ?kind) expr
          | Expression _ -> ()
        end;
        expr
      | _ -> super#pattern ?kind expr

    (* don't hoist let/const bindings *)
    method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      this#add_var_binding decl.id;
      super#declare_variable loc decl

    method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      if with_types then this#add_type_binding alias.id;
      super#type_alias loc alias

    method! opaque_type loc (alias : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      if with_types then this#add_type_binding alias.id;
      super#opaque_type loc alias

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      if with_types then this#add_type_binding interface.id;
      super#interface loc interface

    method! import_declaration loc decl =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; _ } = decl in
      (* when `with_types` is false, don't visit `import type ...` or `import typeof ...` *)
      match (with_types, import_kind) with
      | (false, ImportType)
      | (false, ImportTypeof) ->
        decl
      | _ -> super#import_declaration loc decl

    method! import_named_specifier
        ~import_kind:_ (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.named_specifier) =
      let open Ast.Statement.ImportDeclaration in
      (* when `with_types` is false, only add bindings for values, not types.
         `import_declaration` avoids visiting specifiers for `import type` and
         `import typeof`, so `kind = None` must mean a value here. *)
      let allowed_kind = function
        | None
        | Some ImportValue ->
          (true, this#add_const_binding ~kind:Bindings.Import)
        | Some ImportType
        | Some ImportTypeof ->
          (with_types, this#add_type_binding)
      in
      (match specifier with
      | { local = Some binding; remote = _; kind }
      | { local = None; remote = binding; kind } ->
        let (allowed, add_binding) = allowed_kind kind in
        if allowed then add_binding binding);
      specifier

    method! import_default_specifier (id : ('loc, 'loc) Ast.Identifier.t) =
      this#add_const_binding ~kind:Bindings.Import id;
      id

    method! import_namespace_specifier _loc (id : ('loc, 'loc) Ast.Identifier.t) =
      this#add_const_binding ~kind:Bindings.Import id;
      id
  end
