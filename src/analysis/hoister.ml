(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor

(* Hoister class. Does a shallow visit of statements, looking for binding
   declarations (currently, variable declarations, parameters, function
   declarations, and component declarations) and recording the corresponding
   bindings in a list. The list can have duplicates, which are handled
   elsewhere.

   TODO: Ideally implemented as a fold, not a map.
*)
(* TODO: It should be possible to vastly simplify hoisting by overriding the
   general method `pattern_identifier ?kind` for kind = Var | Let | Const that
   was recently introduced to distinguish bindings and assignments from other
   occurrences (`identifier`).

   Instead, it is implemented below by overriding several specific methods that
   are known to introduce bindings. The logic here is sufficiently tricky that
   we probably should not change it without extensive testing. *)

class ['loc] lexical_hoister ~flowmin_compatibility ~enable_enums =
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

    method private add_component_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.Component))

    method private add_declared_function_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.DeclaredFunction))

    method private add_declared_class_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.DeclaredClass))

    method private add_declared_var_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.DeclaredVar))

    method private add_declared_let_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.DeclaredLet))

    method private add_declared_const_binding entry =
      this#update_acc Bindings.(add (entry, Bindings.DeclaredConst))

    (* Ignore all statements except variable declarations, class declarations, and
       import declarations. The ignored statements cannot contain lexical
       bindings in the current scope. *)
    method! statement (stmt : ('loc, 'loc) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (_, DeclareModule _) ->
        (* Hoister should never add inner module declarations to bindings. *)
        stmt
      | (_, VariableDeclaration _)
      | (_, ClassDeclaration _)
      | (_, DeclareClass _)
      | (_, DeclareExportDeclaration _)
      | (_, DeclareVariable _)
      | (_, EnumDeclaration _)
      | (_, ExportNamedDeclaration _)
      | (_, ExportDefaultDeclaration _)
      | (_, ImportDeclaration _)
      | (_, Labeled _) ->
        super#statement stmt
      | (_, FunctionDeclaration _)
      | (_, ComponentDeclaration _)
      | (_, DeclareFunction _)
      | (_, DeclareComponent _) ->
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
      | Some (Ast.Variable.Let | Ast.Variable.Const) ->
        let open Ast.Pattern in
        let add_binding =
          match kind with
          | Some Ast.Variable.Let -> this#add_let_binding ?kind:None
          | Some Ast.Variable.Const -> this#add_const_binding ?kind:None
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
      | Some Ast.Variable.Var -> expr

    method base_pattern = super#pattern

    method! match_binding_pattern _loc binding_pattern =
      let open Ast.MatchPattern.BindingPattern in
      let { kind; id; comments = _ } = binding_pattern in
      (match kind with
      | Ast.Variable.Let -> this#add_let_binding id
      | Ast.Variable.Const -> this#add_const_binding id
      | Ast.Variable.Var ->
        (* `var` is not allowed. *)
        ());
      binding_pattern

    method! match_as_pattern as_pattern =
      let open Ast.MatchPattern.AsPattern in
      let { pattern; target; comments = _ } = as_pattern in
      (match target with
      | Binding (loc, binding) -> ignore @@ this#match_binding_pattern loc binding
      | Identifier id ->
        ignore @@ super#match_pattern pattern;
        this#add_const_binding id);
      as_pattern

    method! function_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      let old_lkind = let_kind in
      let_kind <- Bindings.Parameter;
      let res = this#binding_pattern ~kind:Ast.Variable.Let expr in
      let_kind <- old_lkind;
      res

    method! component_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      let old_lkind = let_kind in
      let_kind <- Bindings.ComponentParameter;
      let res = this#binding_pattern ~kind:Ast.Variable.Let expr in
      let_kind <- old_lkind;
      res

    method! catch_clause_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      let old_lkind = let_kind in
      let_kind <- Bindings.CatchParameter;
      let res = this#binding_pattern ~kind:Ast.Variable.Let expr in
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

    method! declare_component _loc (stmt : ('loc, 'loc) Ast.Statement.DeclareComponent.t) =
      let open Ast.Statement.DeclareComponent in
      let { id; _ } = stmt in
      this#add_component_binding id;
      stmt

    method! component_declaration _loc (stmt : ('loc, 'loc) Ast.Statement.ComponentDeclaration.t) =
      let open Ast.Statement.ComponentDeclaration in
      let { id; _ } = stmt in
      this#add_component_binding id;
      stmt

    method! declare_class loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      this#add_declared_class_binding decl.id;
      super#declare_class loc decl

    method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      (* The first binding found wins, so we make sure add a declared function binding when
       * we come across it before attempting to transform it into a regular function *)
      let open Ast.Statement.DeclareFunction in
      this#add_declared_function_binding decl.id;
      super#declare_function loc decl

    method! declare_variable _ (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id; kind; _ } = decl in
      (match kind with
      | Ast.Variable.Var -> ()
      | Ast.Variable.Let -> this#add_declared_let_binding id
      | Ast.Variable.Const -> this#add_declared_const_binding id);
      decl

    method! enum_declaration _loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id; _ } = enum in
      if enable_enums then this#add_const_binding ~kind:Bindings.Enum id;
      enum

    method! declare_namespace loc (namespace : ('loc, 'loc) Ast.Statement.DeclareNamespace.t) =
      let open Ast.Statement.DeclareNamespace in
      let { id; _ } = namespace in
      (match id with
      | Global _ -> ()
      | Local id ->
        let kind =
          if
            Flow_ast_utils.is_type_only_declaration_statement
              (loc, Ast.Statement.DeclareNamespace namespace)
          then
            Bindings.Type { imported = false; type_only_namespace = true }
          else
            Bindings.DeclaredConst
        in
        this#update_acc Bindings.(add (id, kind)));
      namespace
  end

class ['loc] hoister ~flowmin_compatibility ~enable_enums ~with_types =
  object (this)
    inherit ['loc] lexical_hoister ~flowmin_compatibility ~enable_enums as super

    val mutable lexical = true

    method private add_var_binding entry = this#update_acc Bindings.(add (entry, Bindings.Var))

    method private add_type_binding ~imported entry =
      this#update_acc Bindings.(add (entry, Bindings.Type { imported; type_only_namespace = false }))

    method! private add_const_binding ?kind entry =
      if lexical then super#add_const_binding ?kind entry

    method! private add_let_binding ?kind entry = if lexical then super#add_let_binding ?kind entry

    method! private add_declared_let_binding entry =
      if lexical then super#add_declared_let_binding entry

    method! private add_declared_const_binding entry =
      if lexical then super#add_declared_const_binding entry

    method! private add_function_binding entry =
      if lexical || flowmin_compatibility then super#add_function_binding entry

    method! private add_component_binding entry =
      if lexical || flowmin_compatibility then super#add_component_binding entry

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
      | Some Ast.Variable.Var ->
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
      let { id; kind; _ } = decl in
      (match kind with
      | Ast.Variable.Var -> this#add_declared_var_binding id
      | Ast.Variable.Let -> this#add_declared_let_binding id
      | Ast.Variable.Const -> this#add_declared_const_binding id);
      super#declare_variable loc decl

    (* We intentionally skip the hoisting of infer type names,
       since they are handled separately with its own hoister. *)
    method! infer_type t = t

    method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      if with_types then this#add_type_binding ~imported:false alias.id;
      super#type_alias loc alias

    method! opaque_type loc (alias : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      if with_types then this#add_type_binding ~imported:false alias.id;
      super#opaque_type loc alias

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      if with_types then this#add_type_binding ~imported:false interface.id;
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
        ~import_kind (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.named_specifier) =
      let open Ast.Statement.ImportDeclaration in
      let allowed_kind kind =
        match (kind, import_kind) with
        | (None, ImportValue)
        | (Some ImportValue, _) ->
          (true, this#add_const_binding ~kind:Bindings.Import)
        | (_, ImportType)
        | (_, ImportTypeof)
        | (Some ImportType, _)
        | (Some ImportTypeof, _) ->
          (with_types, this#add_type_binding ~imported:true)
      in
      (match specifier with
      | { local = Some binding; remote = _; remote_name_def_loc = _; kind }
      | { local = None; remote = binding; remote_name_def_loc = _; kind } ->
        let (allowed, add_binding) = allowed_kind kind in
        if allowed then add_binding binding);
      specifier

    method! import_default_specifier ~import_kind (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Statement.ImportDeclaration in
      begin
        match import_kind with
        | ImportValue -> this#add_const_binding ~kind:Bindings.Import id
        | ImportType
        | ImportTypeof
          when with_types ->
          this#add_type_binding ~imported:true id
        | _ -> ()
      end;
      id

    method! import_namespace_specifier ~import_kind _loc (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Statement.ImportDeclaration in
      begin
        match import_kind with
        | ImportValue -> this#add_const_binding ~kind:Bindings.Import id
        | ImportType
        | ImportTypeof
          when with_types ->
          this#add_type_binding ~imported:true id
        | _ -> ()
      end;
      id
  end
