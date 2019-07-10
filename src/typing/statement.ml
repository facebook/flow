(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Tast_utils = Typed_ast_utils

(* This module contains the traversal functions which set up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

module Anno = Type_annotation
module Class_type_sig = Anno.Class_type_sig
module Object_freeze = Anno.Object_freeze
module Flow = Flow_js
module T = Type

open Utils_js
open Reason
open Type
open Env.LookupMode

open Destructuring

(*************)
(* Utilities *)
(*************)

let ident_name = Flow_ast_utils.name_of_ident

let mk_ident ~comments name = { Ast.Identifier.name; comments }

let snd_fst ((_, x), _) = x

let translate_identifier_or_literal_key t = Ast.Expression.Object.(function
  | Property.Identifier (loc, name) -> Property.Identifier ((loc, t), name)
  | Property.Literal (loc, lit) -> Property.Literal ((loc, t), lit)
  | Property.PrivateName _ | Property.Computed _ -> assert_false "precondition not met")

let is_call_to_invariant callee =
  match callee with
  | (_, Ast.Expression.Identifier (_, { Ast.Identifier.name = "invariant"; _ })) -> true
  | _ -> false

let convert_tparam_instantiations cx tparams_map instantiations =
  let open Ast.Expression.TypeParameterInstantiation in
  let rec loop ts tasts cx tparams_map = function
  | [] -> (List.rev ts, List.rev tasts)
  | ast::asts ->
    begin match ast with
    | Explicit ast ->
        let (_, t), _ as tast = Anno.convert cx tparams_map ast in
        loop ((ExplicitArg t)::ts) ((Explicit tast)::tasts) cx tparams_map asts
    | Implicit loc ->
        let reason = mk_reason RImplicitInstantiation loc in
        let id = Tvar.mk_no_wrap cx reason in
        loop ((ImplicitArg (reason, id))::ts)
          ((Implicit (loc, OpenT (reason, id)))::tasts) cx tparams_map asts
    end
  in
  loop [] [] cx tparams_map instantiations

let convert_targs cx = function
  | None -> None, None
  | Some (loc, args) ->
    let targts, targs_ast = convert_tparam_instantiations cx SMap.empty args in
    Some targts, Some (loc, targs_ast)

class return_finder = object(this)
  inherit [bool, ALoc.t] Flow_ast_visitor.visitor ~init:false as super

  method! return _ node =
    (* TODO we could pass over `return;` since it's definitely returning `undefined`. It will likely
     * reposition existing errors from the `return;` to the location of the type annotation. *)
    this#set_acc true;
    node

  method! call _loc expr =
    begin if is_call_to_invariant Ast.Expression.Call.(expr.callee) then
      this#set_acc true
    end;
    expr

  method! throw _loc stmt =
    this#set_acc true;
    stmt

  method! function_body_any body =
    begin match body with
      (* If it's a body expression, some value is implicitly returned *)
      | Flow_ast.Function.BodyExpression _ -> this#set_acc true
      | _ -> ()
    end;
    super#function_body_any body

  (* Any returns in these constructs would be for nested function definitions, so we short-circuit
  *)
  method! class_ _ x = x
  method! function_declaration _ x = x
end

let might_have_nonvoid_return loc function_ast =
  let finder = new return_finder in
  finder#eval (finder#function_ loc) function_ast

module Func_stmt_config = struct
  type 'T ast = (ALoc.t, 'T) Ast.Function.Params.t
  type 'T param_ast = (ALoc.t, 'T) Ast.Function.Param.t
  type 'T rest_ast = (ALoc.t, 'T) Ast.Function.RestParam.t

  type expr =
    Context.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

  type pattern =
    | Id of (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t
    | Object of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        properties: (ALoc.t, ALoc.t) Ast.Pattern.Object.property list;
      }
    | Array of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        elements: (ALoc.t, ALoc.t) Ast.Pattern.Array.element option list;
      }

  type param = Param of {
    t: Type.t;
    loc: ALoc.t;
    ploc: ALoc.t;
    pattern: pattern;
    default: (ALoc.t, ALoc.t) Ast.Expression.t option;
    expr: expr;
  }

  type rest = Rest of {
    t: Type.t;
    loc: ALoc.t;
    ploc: ALoc.t;
    id: (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t;
  }

  let param_type (Param { t; pattern; default; _ }) =
    match pattern with
    | Id id ->
      let { Ast.Pattern.Identifier.
        name = (_, { Ast.Identifier.name; _ });
        optional; _;
      } = id in
      let t = if optional || default <> None then Type.optional t else t in
      Some name, t
    | _ ->
      let t = if default <> None then Type.optional t else t in
      None, t

  let rest_type (Rest { t; loc; id; _ }) =
    let { Ast.Pattern.Identifier.
      name = (_, { Ast.Identifier.name; _ });
      _;
    } = id in
    Some name, loc, t

  let subst_param cx map param =
    let Param { t; loc; ploc; pattern; default; expr } = param in
    let t = Flow.subst cx map t in
    Param { t; loc; ploc; pattern; default; expr }

  let subst_rest cx map rest =
    let Rest { t; loc; ploc; id } = rest in
    let t = Flow.subst cx map t in
    Rest { t; loc; ploc; id }

  let bind cx name t loc =
    let open Scope in
    if Context.enable_const_params cx
    then
      let kind = Entry.ConstParamBinding in
      Env.bind_implicit_const ~state:State.Initialized
        kind cx name t loc
    else
      let kind =
        if Env.promote_to_const_like cx loc
        then Entry.ConstlikeParamBinding
        else Entry.ParamBinding
      in
      Env.bind_implicit_let ~state:State.Initialized
        kind cx name t loc

  let destruct cx annot ~use_op:_ loc name default t =
    let reason = mk_reason (RIdentifier name) loc in
    let t = match annot with
    | Ast.Type.Missing _ -> t
    | Ast.Type.Available _ ->
      let source = Tvar.mk_where cx reason (fun t' ->
        Flow.flow cx (t, BecomeT (reason, t'))
      ) in
      AnnotT (reason, source, false)
    in
    Option.iter ~f:(fun d ->
      let default_t = Flow.mk_default cx reason d in
      Flow.flow_t cx (default_t, t)
    ) default;
    bind cx name t loc

  let eval_default cx ~expr = function
    | None -> None
    | Some e -> Some (expr cx e)

  let eval_param cx (Param {t; loc; ploc; pattern; default; expr}) =
    match pattern with
    | Id id ->
      let default = eval_default cx ~expr default in
      let () =
        match default with
        | None -> ()
        | Some ((_, default_t), _) -> Flow.flow_t cx (default_t, t)
      in
      let () =
        let { Ast.Pattern.Identifier.
          name = ((loc, _), { Ast.Identifier.name; _ });
          optional; _
        } = id in
        let t = if optional && default = None then Type.optional t else t in
        bind cx name t loc
      in
      loc, { Ast.Function.Param.
        argument = ((ploc, t), Ast.Pattern.Identifier id);
        default;
      }

    | Object { annot; properties } ->
      let default = eval_default cx ~expr default in
      let properties =
        let default = Option.map default (fun ((_, t), _) ->
          Default.expr t
        ) in
        let init = Destructuring.empty ?default t in
        let f = destruct cx annot in
        Destructuring.object_properties cx ~expr ~f init properties
      in
      loc, { Ast.Function.Param.
        argument = ((ploc, t), Ast.Pattern.Object { Ast.Pattern.Object.
          properties;
          annot;
        });
        default;
      }

    | Array { annot; elements } ->
      let default = eval_default cx ~expr default in
      let elements =
        let default = Option.map default (fun ((_, t), _) ->
          Default.expr t
        ) in
        let init = Destructuring.empty ?default t in
        let f = destruct cx annot in
        Destructuring.array_elements cx ~expr ~f init elements
      in
      loc, { Ast.Function.Param.
        argument = ((ploc, t), Ast.Pattern.Array { Ast.Pattern.Array.
          elements;
          annot;
        });
        default;
      }

  let eval_rest cx (Rest {t; loc; ploc; id}) =
    let () =
      let { Ast.Pattern.Identifier.
        name = ((loc, _), { Ast.Identifier.name; _ });
        _
      } = id in
      bind cx name t loc
    in
    loc, { Ast.Function.RestParam.
      argument = ((ploc, t), Ast.Pattern.Identifier id)
    }
end
module Func_stmt_params = Func_params.Make (Func_stmt_config)
module Func_stmt_sig = Func_sig.Make (Func_stmt_params)
module Class_stmt_sig = Class_sig.Make (Func_stmt_sig)

module Class_property = struct
  type t =
    | Public of string
    | Private of string

  let compare = compare
end
module Class_property_map = Map.Make(Class_property)

(************)
(* Visitors *)
(************)

(********************************************************************
 * local inference preliminary pass: traverse AST, collecting
 * declarations and populating variable environment (scope stack)
 * in prep for main pass
 ********************************************************************)

let rec variable_decl cx { Ast.Statement.VariableDeclaration.kind; declarations } =
  let bind = match kind with
    | Ast.Statement.VariableDeclaration.Const -> Env.bind_const
    | Ast.Statement.VariableDeclaration.Let -> Env.bind_let
    | Ast.Statement.VariableDeclaration.Var -> Env.bind_var
  in
  Flow_ast_utils.fold_bindings_of_variable_declarations (
    fun () (loc, { Ast.Identifier.name; comments = _ }) ->
      let reason = mk_reason (RIdentifier name) loc in
      let t = Tvar.mk cx reason in
      bind cx name t loc
  ) () declarations

and toplevel_decls cx =
  List.iter (statement_decl cx)

(* TODO: detect structural misuses abnormal control flow constructs *)
and statement_decl cx = Ast.Statement.(
  let block_body cx { Block.body } =
    Env.in_lex_scope cx (fun () ->
      toplevel_decls cx body
    )
  in

  let catch_clause cx { Try.CatchClause.body = (_, b); _ } =
    block_body cx b
  in

  function

  | (_, Empty) -> ()

  | (_, Block b) ->
      block_body cx b

  | (_, Expression _) -> ()

  | (_, If { If.consequent; alternate; _ }) ->
      statement_decl cx consequent;
      (match alternate with
        | None -> ()
        | Some st -> statement_decl cx st
      )

  | (_, Labeled { Labeled.body; _ }) ->
      statement_decl cx body

  | (_, Break _) -> ()

  | (_, Continue _) -> ()

  | (_, With _) ->
      (* TODO disallow or push vars into env? *)
      ()

  | (_, DeclareTypeAlias { TypeAlias.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } )
  | (_, TypeAlias { TypeAlias.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } ) ->
      let r = DescFormat.type_reason name (name_loc) in
      let tvar = Tvar.mk cx r in
      Env.bind_type cx name tvar name_loc

  | (_, DeclareOpaqueType { OpaqueType.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } )
  | (_, OpaqueType { OpaqueType.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } ) ->
      let r = DescFormat.type_reason name name_loc in
      let tvar = Tvar.mk cx r in
      Env.bind_type cx name tvar name_loc

  | (_, Switch { Switch.cases; _ }) ->
      Env.in_lex_scope cx (fun () ->
        cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
          toplevel_decls cx consequent
        )
      )

  | (_, Return _) -> ()

  | (_, Throw _) -> ()

  | (_, Try { Try.block = (_, b); handler; finalizer }) ->
      block_body cx b;

      (match handler with
        | None -> ()
        | Some (_, h) -> catch_clause cx h
      );

      (match finalizer with
        | None -> ()
        | Some (_, b) -> block_body cx b
      )

  | (_, While { While.body; _ }) ->
      statement_decl cx body

  | (_, DoWhile { DoWhile.body; _ }) ->
      statement_decl cx body

  | (_, For { For.init; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match init with
          | Some (For.InitDeclaration (_, decl)) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, ForIn { ForIn.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForIn.LeftDeclaration (_, decl) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, ForOf { ForOf.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForOf.LeftDeclaration (_, decl) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, Debugger) -> ()

  | (loc, FunctionDeclaration { Ast.Function.id; async; generator; _ }) ->
      (match id with
      | Some (_, { Ast.Identifier.name; comments= _ }) ->
        let r = func_reason ~async ~generator loc in
        let tvar = Tvar.mk cx r in
        Env.bind_fun cx name tvar loc
      | None ->
        failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
      )

  | _, EnumDeclaration _ -> ()

  | (loc, DeclareVariable { DeclareVariable.id = (id_loc, { Ast.Identifier.name; comments= _ }); _ }) ->
      let r = mk_reason (RCustom (spf "declare %s" name)) loc in
      let t = Tvar.mk cx r in
      Env.bind_declare_var cx name t id_loc

  | (loc, DeclareFunction ({ DeclareFunction.
      id = (id_loc, { Ast.Identifier.name; comments= _ });
      _; } as declare_function)) ->
      (match declare_function_to_function_declaration cx loc declare_function with
      | None ->
          let r = mk_reason (RCustom (spf "declare %s" name)) loc in
          let t = Tvar.mk cx r in
          Env.bind_declare_fun cx name t id_loc
      | Some (func_decl, _) ->
          statement_decl cx (loc, func_decl)
      )

  | (_, VariableDeclaration decl) ->
      variable_decl cx decl

  | (_, ClassDeclaration { Ast.Class.id; _ }) -> (
      match id with
      | Some (name_loc, { Ast.Identifier.name; comments= _ }) ->
        let r = mk_reason (RType name) name_loc in
        let tvar = Tvar.mk cx r in
        Env.bind_implicit_let Scope.Entry.ClassNameBinding cx name tvar name_loc
      | None -> ()
    )

  | (_, DeclareClass { DeclareClass.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ })
  | (_, DeclareInterface { Interface.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ })
  | (_, InterfaceDeclaration { Interface.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ }) as stmt ->
      let is_interface = match stmt with
      | (_, DeclareInterface _) -> true
      | (_, InterfaceDeclaration _) -> true
      | _ -> false in
      let r = mk_reason (RType name) name_loc in
      let tvar = Tvar.mk cx r in
      (* interface is a type alias, declare class is a var *)
      if is_interface
      then Env.bind_type cx name tvar name_loc
      else Env.bind_declare_var cx name tvar name_loc

  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, { Ast.Identifier.name= value; comments= _ })
      | DeclareModule.Literal (_, { Ast.StringLiteral.value; _ }) -> value
      in
      let r = mk_reason (RModule name) loc in
      let t = Tvar.mk cx r in
      Env.bind_declare_var cx (internal_module_name name) t loc

  | _,
    DeclareExportDeclaration {
      DeclareExportDeclaration.default; declaration; _
    } ->
        DeclareExportDeclaration.(match declaration with
        | Some (Variable (loc, v)) ->
            statement_decl cx (loc, DeclareVariable v)
        | Some (Function (loc, f)) ->
            statement_decl cx (loc, DeclareFunction f)
        | Some (Class (loc, c)) ->
            statement_decl cx (loc, DeclareClass c)
        | Some (DefaultType _) -> ()
        | Some (NamedType (loc, t)) ->
            statement_decl cx (loc, TypeAlias t)
        | Some (NamedOpaqueType (loc, t)) ->
            statement_decl cx (loc, OpaqueType t)
        | Some (Interface (loc, i)) ->
            statement_decl cx (loc, InterfaceDeclaration i)
        | None ->
            if Option.is_none default
            then ()
            else failwith (
              "Parser Error: declare export default must always have an " ^
              "associated declaration or type!"
            )
        )

  | (_, DeclareModuleExports _) -> ()

  | (_, ExportNamedDeclaration { ExportNamedDeclaration.declaration; _ }) -> (
      match declaration with
      | Some stmt -> statement_decl cx stmt
      | None -> ()
    )
  | _, ExportDefaultDeclaration { ExportDefaultDeclaration.declaration; _ } -> (
      match declaration with
      | ExportDefaultDeclaration.Declaration stmt ->
        let stmt, _ = Import_export.nameify_default_export_decl stmt in
        statement_decl cx stmt
      | ExportDefaultDeclaration.Expression _ -> ()
    )
  | (_, ImportDeclaration { ImportDeclaration.importKind; specifiers; default; source = _ }) ->
      let isType =
        match importKind with
        | ImportDeclaration.ImportType -> true
        | ImportDeclaration.ImportTypeof -> true
        | ImportDeclaration.ImportValue -> false
      in

      let bind_import local_name (loc: ALoc.t) isType =
        let reason = if isType
          then DescFormat.type_reason local_name loc
          else mk_reason (RIdentifier local_name) loc in
        let tvar = Tvar.mk cx reason in
        if isType
        then Env.bind_import_type cx local_name tvar loc
        else Env.bind_import cx local_name tvar loc
      in

      Option.iter ~f:(fun local ->
        bind_import (ident_name local) (fst local) isType
      ) default;

      Option.iter ~f:(function
        | ImportDeclaration.ImportNamespaceSpecifier (_, local) ->
          bind_import (ident_name local) (fst local) isType

        | ImportDeclaration.ImportNamedSpecifiers named_specifiers ->
          List.iter (fun { ImportDeclaration.local; remote; kind;} ->
            let (loc, { Ast.Identifier.name= local_name; comments= _ }) = Option.value ~default:remote local in
            let isType = isType || (
              match kind with
              | None -> isType
              | Some kind ->
                kind = ImportDeclaration.ImportType
              || kind = ImportDeclaration.ImportTypeof
            ) in
            bind_import local_name loc isType
          ) named_specifiers
      ) specifiers
)

(***************************************************************
 * local inference main pass: visit AST statement list, calling
 * flow to check types/create graphs for merge-time checking
 ***************************************************************)

(* accumulates a list of previous statements' ASTs in reverse order *)
(* can raise Abnormal.(Exn (Stmts _, _)). *)
and toplevels =
  let rec loop acc cx = function
  | [] -> List.rev acc
  | (loc, Ast.Statement.Empty)::stmts ->
      loop ((loc, Ast.Statement.Empty)::acc) cx stmts
  | stmt::stmts ->
    match Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx stmt) with
    | stmt, Some abnormal ->
      (* control flow exit out of a flat list:
         check for unreachable code and rethrow *)
      let warn_unreachable loc =
        Flow.add_output cx (Error_message.EUnreachable loc) in
      let rest = Core_list.map ~f:Ast.Statement.(fun stmt ->
        match stmt with
        | (_, Empty) as stmt -> stmt
        (* function declarations are hoisted, so not unreachable *)
        | (_, FunctionDeclaration _ ) -> statement cx stmt
        (* variable declarations are hoisted, but associated assignments are
           not, so skip variable declarations with no assignments.
           Note: this does not seem like a practice anyone would use *)
        | (_, VariableDeclaration d) as stmt -> VariableDeclaration.(d.declarations |>
            List.iter Declarator.(function
            | (_, { init = Some (loc, _); _ } ) -> warn_unreachable loc
            | _ -> ()
          ));
          Tast_utils.unreachable_mapper#statement stmt
        | (loc, _) as stmt ->
          warn_unreachable loc;
          Tast_utils.unreachable_mapper#statement stmt
      ) stmts in
      Abnormal.throw_stmts_control_flow_exception
        (List.rev_append acc (stmt::rest))
        abnormal
    | stmt, None -> loop (stmt::acc) cx stmts
  in
  fun cx -> loop [] cx

(* can raise Abnormal.(Exn (Stmt _, _)) *)
and statement cx : 'a -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t = Ast.Statement.(
  let variables cx decls =
    let open VariableDeclaration in
    let { declarations; kind } = decls in
    let declarations = Core_list.map ~f:(fun (loc, { Declarator.id; init }) ->
      let id, init = variable cx kind id init in
      (loc, { Declarator.id; init })
    ) declarations in
    { declarations; kind; }
  in

  let interface_helper cx loc (iface_sig, self) =
    let def_reason = mk_reason (desc_of_t self) loc in
    Class_type_sig.generate_tests cx (fun iface_sig ->
      Class_type_sig.check_super cx def_reason iface_sig;
      Class_type_sig.check_implements cx def_reason iface_sig
    ) iface_sig;
    let t = Class_type_sig.classtype ~check_polarity:false cx iface_sig in
    Flow.unify cx self t;
    t
  in

  let interface cx loc decl =
    let { Interface.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } = decl in
    let reason = DescFormat.instance_reason name name_loc in
    let iface_sig, iface_t, decl_ast = Anno.mk_interface_sig cx reason decl in
    let t = interface_helper cx loc (iface_sig, iface_t) in
    Env.init_type cx name t loc;
    decl_ast
  in

  let declare_class cx loc decl =
    let { DeclareClass.id = (name_loc, { Ast.Identifier.name; comments= _ }); _ } = decl in
    let reason = DescFormat.instance_reason name name_loc in
    let class_sig, class_t, decl_ast = Anno.mk_declare_class_sig cx reason decl in
    let t = interface_helper cx loc (class_sig, class_t) in
    let use_op = Op (AssignVar {
      var = Some (mk_reason (RIdentifier name) loc);
      init = reason_of_t t;
    }) in
    Env.init_var ~has_anno:false cx ~use_op name t loc;
    decl_ast
  in

  let check cx b = Abnormal.catch_stmts_control_flow_exception(fun () ->
    toplevel_decls cx b.Block.body;
    toplevels cx b.Block.body) in

  let catch_clause cx catch_clause =
    let { Try.CatchClause.param; body = (b_loc, b) } = catch_clause in
    Ast.Pattern.(match param with
      | Some p -> (match p with
        | loc, Identifier {
            Identifier.name = name_loc, ({ Ast.Identifier.name; comments= _ } as id);
            annot = Ast.Type.Missing mloc;
            optional;
          } ->
            let r = mk_reason (RCustom "catch") loc in
            let t = Tvar.mk cx r in


            let stmts, abnormal_opt = Env.in_lex_scope cx (fun () ->
              Scope.(Env.bind_implicit_let
                ~state:State.Initialized Entry.CatchParamBinding cx name t loc);

               check cx b
            ) in
            { Try.CatchClause.
              param = Some ((loc, t), Ast.Pattern.Identifier { Ast.Pattern.Identifier.
                name = (name_loc, t), id;
                annot = Ast.Type.Missing (mloc, t);
                optional;
              });
              body = b_loc, { Block.body = stmts };
            },
            abnormal_opt


        | loc, Identifier _ ->
            Flow.add_output cx
              Error_message.(EUnsupportedSyntax (loc, CatchParameterAnnotation));
            Tast_utils.error_mapper#catch_clause catch_clause, None

        | loc, _ ->
            Flow.add_output cx
              Error_message.(EUnsupportedSyntax (loc, CatchParameterDeclaration));
            Tast_utils.error_mapper#catch_clause catch_clause, None
      )
      | None ->
        let stmts, abnormal_opt = Env.in_lex_scope cx (fun () ->
          check cx b
        ) in
        { Try.CatchClause.
          param = None;
          body = b_loc, { Block.body = stmts };
        },
        abnormal_opt
    )
  in

  function

  | (_, Empty) as stmt -> stmt

  | (loc, Block { Block.body }) ->
    let body, abnormal_opt =
      Abnormal.catch_stmts_control_flow_exception (fun () ->
        Env.in_lex_scope cx (fun () ->
          toplevel_decls cx body;
          toplevels cx body
        )
      )
    in
    Abnormal.check_stmt_control_flow_exception (
      (loc, Block { Block.body }),
      abnormal_opt
    )

  | (loc, Expression { Expression.expression = e; directive; }) ->
    loc, Expression { Expression.
      expression = expression cx e;
      directive;
    }

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let loc_test, _ = test in
      let test_ast, preds, not_preds, xts =
        predicates_of_condition cx test in

      (* grab a reference to the incoming env -
         we'll restore it and merge branched envs later *)
      let start_env =  Env.peek_env () in
      let oldset = Changeset.Global.clear () in

      (* swap in a refined clone of initial env for then *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc_test preds xts)
      );

      let then_ast, then_abnormal = Abnormal.catch_stmt_control_flow_exception
        (fun () -> statement cx consequent)
      in

      (* grab a reference to env after then branch *)
      let then_env = Env.peek_env () in

      (* then swap in a refined clone of initial env for else *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc_test not_preds xts)
      );

      let else_ast, else_abnormal = match alternate with
        | None -> None, None
        | Some st ->
          let else_ast, else_abnormal =
            Abnormal.catch_stmt_control_flow_exception
              (fun () -> statement cx st)
          in Some else_ast, else_abnormal
      in

      (* grab a reference to env after else branch *)
      let else_env = Env.peek_env () in

      (* snapshot if-else changes and merge old changes back into state *)
      let newset = Changeset.Global.merge oldset in

      (* adjust post-if environment. if we've returned from one arm,
         swap in the env generated by the other, otherwise merge *)
      let end_env = match then_abnormal, else_abnormal with
      | Some Abnormal.Return, None
      | Some Abnormal.Throw, None ->
        else_env

      | None, Some Abnormal.Return
      | None, Some Abnormal.Throw ->
        then_env

      | None, Some _
      | Some _, None
      | Some _, Some _ ->
        Env.merge_env cx loc (start_env, then_env, else_env) newset;
        start_env

      | None, None ->
        (* if neither branch has abnormal flow, then refinements that happen in
           the branches should be forgotten since the original type covers
           all of the options. *)
        Env.merge_env cx loc
          (start_env, then_env, else_env)
          (Changeset.exclude_refines newset);
        start_env
      in
      Env.update_env cx loc end_env;

      let ast = loc, If { If.
        test = test_ast;
        consequent = then_ast;
        alternate = else_ast;
      } in

      (* handle control flow in cases where we've thrown from both sides *)
      begin match then_abnormal, else_abnormal with
      | Some Abnormal.Throw, Some Abnormal.Return
      | Some Abnormal.Return, Some Abnormal.Throw ->
        Abnormal.throw_stmt_control_flow_exception ast Abnormal.Return;

      | Some then_exn, Some else_exn when then_exn = else_exn ->
        Abnormal.throw_stmt_control_flow_exception ast then_exn

      | _ -> ast
      end

  | (top_loc, Labeled { Labeled.label = _, { Ast.Identifier.name; comments= _ } as lab_ast; body }) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _)
        ->
        let oldset = Changeset.Global.clear () in
        let label = Some name in
        let save_break = Abnormal.clear_saved (Abnormal.Break label) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue label) in

        let env = Env.peek_env () in
        Env.widen_env cx loc;

        let loop_env = Env.clone_env env in
        Env.update_env cx loc loop_env;

        let body_ast, body_abnormal =
          Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
          |> Abnormal.ignore_break_or_continue_to_label label
        in
        let ast = top_loc, Labeled { Labeled.label = lab_ast; body = body_ast } in
        ignore (Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)
          : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t);

        let newset = Changeset.Global.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue label) save_continue <> None
        then Env.havoc_vars newset;

        Env.copy_env cx loc (env,loop_env) newset;

        if Abnormal.swap_saved (Abnormal.Break label) save_break <> None
        then Env.havoc_vars newset;

        ast

      | _ ->
        let oldset = Changeset.Global.clear () in
        let label = Some name in
        let save_break = Abnormal.clear_saved (Abnormal.Break label) in

        let body_ast, body_abnormal =
          Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
          |> Abnormal.ignore_break_to_label label
        in
        let ast = top_loc, Labeled { Labeled.label = lab_ast; body = body_ast } in
        ignore (Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)
          : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t);

        let newset = Changeset.Global.merge oldset in
        if Abnormal.swap_saved (Abnormal.Break label) save_break <> None
        then Env.havoc_vars newset;

        ast
      )

  | (loc, Break { Break.label }) ->
      (* save environment at unlabeled breaks, prior to activation clearing *)
      let label_opt, env, label_ast = match label with
        | None -> None, Env.(clone_env (peek_env ())), None
        | Some (_, { Ast.Identifier.name; comments= _ } as lab_ast) -> Some name, [], Some lab_ast
      in
      Env.reset_current_activation loc;
      let ast = loc, Break { Break.label = label_ast } in
      let abnormal = Abnormal.Break label_opt in
      Abnormal.save abnormal ~env;
      Abnormal.throw_stmt_control_flow_exception ast abnormal

  | (loc, Continue { Continue.label; comments }) ->
      let label_opt, label_ast = match label with
        | None -> None, None
        | Some (_, { Ast.Identifier.name; comments= _ } as lab_ast) -> Some name, Some lab_ast
      in
      Env.reset_current_activation loc;
      let ast = loc, Continue { Continue.label = label_ast; comments = comments } in
      let abnormal = Abnormal.Continue label_opt in
      Abnormal.save abnormal;
      Abnormal.throw_stmt_control_flow_exception ast abnormal

  | (_, With _) as s ->
      (* TODO or disallow? *)
      Tast_utils.error_mapper#statement s

  |((loc, DeclareTypeAlias {TypeAlias.id=name_loc, ({ Ast.Identifier.name; comments= _ } as id); tparams; right;})
  | (loc, TypeAlias {TypeAlias.id=name_loc, ({ Ast.Identifier.name; comments= _ } as id); tparams; right;})) as stmt ->
      let r = DescFormat.type_reason name name_loc in
      let typeparams, typeparams_map, tparams_ast =
        Anno.mk_type_param_declarations cx tparams in
      let (_, t), _ as right_ast = Anno.convert cx typeparams_map right in
      let t =
        let mod_reason = replace_reason ~keep_def_loc:true
          (fun desc -> RTypeAlias (name, true, desc)) in
        let rec loop = function
        | ExactT (r, t) -> ExactT (mod_reason r, loop t)
        | MaybeT (r, t) -> MaybeT (mod_reason r, loop t)
        | t -> mod_reason_of_t mod_reason t
        in
        loop t
      in
      let type_ = poly_type_of_tparams (Context.make_nominal cx) typeparams
        (DefT (r, bogus_trust (), TypeT (TypeAliasKind, t))) in
      Flow.check_polarity cx Polarity.Positive t;

      Env.init_type cx name type_ name_loc;
      let type_alias_ast = { TypeAlias.
        id = (name_loc, type_), id;
        tparams = tparams_ast;
        right = right_ast;
      } in
      (match stmt with
      | _, DeclareTypeAlias _ -> loc, DeclareTypeAlias type_alias_ast
      | _, TypeAlias _ -> loc, TypeAlias type_alias_ast
      | _ -> assert false)

  |((loc, DeclareOpaqueType
    {OpaqueType.id=name_loc, ({ Ast.Identifier.name; comments= _ } as id); tparams; impltype; supertype})
  | (loc, OpaqueType {OpaqueType.id=name_loc, ({ Ast.Identifier.name; comments= _ } as id); tparams; impltype; supertype}))
    as stmt ->
      let r = DescFormat.type_reason name name_loc in
      let typeparams, typeparams_map, tparams_ast =
        Anno.mk_type_param_declarations cx tparams in
      let underlying_t, impltype_ast = Anno.convert_opt cx typeparams_map impltype in
      let opaque_type_args = Core_list.map ~f:(fun {name; reason; polarity; _} ->
        let t = SMap.find_unsafe name typeparams_map in
        name, reason, t, polarity
      ) (TypeParams.to_list typeparams) in
      let super_t, supertype_ast = Anno.convert_opt cx typeparams_map supertype in
      let opaquetype = {
        underlying_t;
        super_t;
        opaque_id = name_loc;
        opaque_type_args;
        opaque_name = name
      } in
      let t = OpaqueT (mk_reason (ROpaqueType name) loc, opaquetype) in
      Flow.check_polarity cx Polarity.Positive t;
      let type_ = poly_type_of_tparams (Context.make_nominal cx) typeparams
        (DefT (r, bogus_trust (), TypeT (OpaqueKind, t))) in
      let open Flow in
      let () = match underlying_t, super_t with
      | Some l, Some u ->
        generate_tests cx (TypeParams.to_list typeparams) (fun map_ ->
          flow_t cx (subst cx map_ l, subst cx map_ u)
        ) |> ignore
      | _ -> ()
      in
      Env.init_type cx name type_ name_loc;
      let opaque_type_ast = { OpaqueType.
        id = (name_loc, type_), id;
        tparams = tparams_ast;
        impltype = impltype_ast;
        supertype = supertype_ast;
      } in
      (match stmt with
      | _, DeclareOpaqueType _ -> loc, DeclareOpaqueType opaque_type_ast
      | _, OpaqueType _ -> loc, OpaqueType opaque_type_ast
      | _ -> assert false)

  (*******************************************************)

  | (switch_loc, Switch { Switch.discriminant; cases; }) ->

    (* add default if absent *)
    let cases, added_default = Switch.Case.(
      if List.exists (fun (_, { test; _ }) -> test = None) cases
      then cases, false
      else cases @ [switch_loc, { test = None; consequent = [] }], true
    ) in

    (* typecheck discriminant *)
    let discriminant_ast = expression cx discriminant in

    (* switch body is a single lexical scope *)
    Env.in_lex_scope cx (fun () ->

      (* save incoming env state, clear changeset *)
      let incoming_changes = Changeset.Global.clear () in
      let incoming_env = Env.peek_env () in
      let incoming_depth = List.length incoming_env in

      (* set up all bindings *)
      cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
        toplevel_decls cx consequent
      );

      (** each case starts with this env - begins as clone of incoming_env
          plus bindings, also accumulates negative refis from case tests *)
      let case_start_env = Env.clone_env incoming_env in

      (* Some (env, writes, refis, reason) when a case falls through *)
      let fallthrough_case = ref None in

      (* switch_state tracks case effects and is used to create outgoing env *)
      let switch_state = ref None in
      let update_switch_state (case_env, case_writes, _, loc) =
        let case_env = ListUtils.last_n incoming_depth case_env in
        let state = match !switch_state with
        | None ->
          case_env, Changeset.empty, case_writes
        | Some (env, partial_writes, total_writes) ->
          let case_diff = Changeset.comp case_writes total_writes in
          let partial_writes = Changeset.union partial_writes case_diff in
          let total_writes = Changeset.inter case_writes total_writes in
          (* merge new case into switch env *)
          Env.merge_env cx loc (env, env, case_env) case_writes;
          env, partial_writes, total_writes
        in switch_state := Some state
      in

      (* traverse case list, get list of control flow exits and list of ASTs *)
      let exits, cases_ast = cases |> Core_list.map ~f:(
        fun (loc, { Switch.Case.test; consequent }) ->

        (* compute predicates implied by case expr or default *)
        let test_ast, preds, not_preds, xtypes = match test with
        | None ->
          None, Key_map.empty, Key_map.empty, Key_map.empty
        | Some expr ->
          let fake = loc, Ast.Expression.(Binary {
            Binary.operator = Binary.StrictEqual;
            left = discriminant; right = expr
          }) in
          let (_, fake_ast), preds, not_preds, xtypes = predicates_of_condition cx fake in
          let expr_ast = match fake_ast with
          | Ast.Expression.(Binary { Binary.right; _ }) -> right
          | _ -> assert false
          in
          Some expr_ast, preds, not_preds, xtypes
        in

        (* swap in case's starting env and clear changeset *)
        let case_env = Env.clone_env case_start_env in
        Env.update_env cx loc case_env;
        let save_changes = Changeset.Global.clear () in

        (* add test refinements - save changelist for later *)
        let test_refis = Env.refine_with_preds cx loc preds xtypes in

        (* merge env changes from fallthrough case, if present *)
        Option.iter !fallthrough_case ~f:(fun (env, writes, refis, _) ->
          let changes = Changeset.union writes refis in
          Env.merge_env cx loc (case_env, case_env, env) changes
        );

        (** process statements, track control flow exits: exit will be an
            unconditional exit, break_opt will be any break *)
        let save_break = Abnormal.clear_saved (Abnormal.Break None) in
        let consequent_ast, exit = Abnormal.catch_stmts_control_flow_exception (
          fun () -> toplevels cx consequent
        ) in
        let break_opt = Abnormal.swap_saved (Abnormal.Break None) save_break in

        (* restore ambient changes and save case writes *)
        let case_writes = Changeset.include_writes save_changes |> Changeset.Global.merge in

        (* track fallthrough to next case and/or break to switch end *)
        let falls_through, breaks_to_end = match exit with
          | Some Abnormal.Throw
          | Some Abnormal.Return
          | Some Abnormal.Break (Some _)
          | Some Abnormal.Continue _ ->
            false, false
          | Some Abnormal.Break None ->
            false, true
          | None ->
            true, Option.is_some break_opt
        in

        (* save state for fallthrough *)
        fallthrough_case := if falls_through
          then Some (case_env, case_writes, test_refis, loc)
          else None;

        (* if we break to end, add effects to terminal state *)
        if breaks_to_end then begin match break_opt with
          | None ->
            Flow.add_output cx
              Error_message.(EInternal (loc, BreakEnvMissingForCase))
          | Some break_env ->
            update_switch_state (break_env, case_writes, test_refis, loc)
        end;

        (* add negative refis of this case's test to common start env *)
        (* TODO add API to do this without having to swap in env *)
        Env.update_env cx loc case_start_env;
        let _ = Env.refine_with_preds cx loc not_preds xtypes in

        exit, (loc, { Switch.Case.test = test_ast; consequent = consequent_ast})
      ) |> List.split in

    let cases_ast = List.(
      if added_default
      then cases_ast |> rev |> tl |> rev
      else cases_ast
    ) in

    (* if last case fell out, update terminal switch state with it *)
    Option.iter !fallthrough_case ~f:update_switch_state;

    (** env in switch_state has accumulated switch effects. now merge in
        original types for partially written values, and swap env in *)
    Option.iter !switch_state ~f:(fun (env, partial_writes, _) ->
      Env.merge_env cx switch_loc (env, env, incoming_env) partial_writes;
      Env.update_env cx switch_loc env);

    (* merge original changeset back in *)
    let _ = Changeset.Global.merge incoming_changes in

    (** abnormal exit: if every case exits abnormally the same way (or falls
        through to a case that does), then the switch as a whole exits that way.
       (as with if/else, we merge `throw` into `return` when both appear) *)
    let uniform_switch_exit case_exits =
      let rec loop = function
      | acc, fallthrough, [] ->
        (* end of cases: if nothing is falling through, we made it *)
        if fallthrough then None else acc
      | _, _, Some (Abnormal.Break _) :: _ ->
        (* break wrecks everything *)
        None
      | acc, _, None :: exits ->
        (* begin or continue to fall through *)
        loop (acc, true, exits)
      | acc, _, exit :: exits when exit = acc ->
        (* current case exits the same way as prior cases *)
        loop (acc, acc = None, exits)
      | Some Abnormal.Throw, _, Some Abnormal.Return :: exits
      | Some Abnormal.Return, _, Some Abnormal.Throw :: exits ->
        (* fuzz throw into return *)
        loop (Some Abnormal.Return, false, exits)
      | None, _, exit :: exits ->
        (* terminate an initial sequence of fall-thruugh cases *)
        (* (later sequences will have acc = Some _ ) *)
        loop (exit, false, exits)
      | _, _, _ ->
        (* the new case exits differently from previous ones - fail *)
        None
      in loop (None, false, case_exits)
    in
    let ast = switch_loc, Switch { Switch.
      discriminant = discriminant_ast;
      cases = cases_ast;
    } in
    begin match uniform_switch_exit exits with
    | None -> ast
    | Some abnormal -> Abnormal.throw_stmt_control_flow_exception ast abnormal
    end
  )

  (*******************************************************)

  | (loc, Return { Return.argument; comments }) ->
      let reason = mk_reason (RCustom "return") loc in
      let ret = Env.get_internal_var cx "return" loc in
      let t, argument_ast = match argument with
        | None -> VoidT.at loc |> with_trust literal_trust, None
        | Some expr ->
          if Env.in_predicate_scope () then
            let ((_, t), _ as ast, p_map, n_map, _) = predicates_of_condition cx expr in
            let pred_reason = replace_reason (fun desc ->
              RPredicateOf desc
            ) reason in
            OpenPredT (pred_reason, t, p_map, n_map), Some ast
          else
            let (_, t), _ as ast = expression cx expr in
            t, Some ast
      in
      let t = match Env.var_scope_kind () with
      | Scope.Async ->
        (* Convert the return expression's type T to Promise<T>. If the
         * expression type is itself a Promise<T>, ensure we still return
         * a Promise<T> via Promise.resolve. *)
        let reason = mk_reason (RCustom "async return") loc in
        let t' = Flow.get_builtin_typeapp cx reason "Promise" [
          Tvar.mk_derivable_where cx reason (fun tvar ->
            let funt = Flow.get_builtin cx "$await" reason in
            let callt = mk_functioncalltype reason None [Arg t] tvar in
            let reason = repos_reason (aloc_of_reason (reason_of_t t)) reason in
            Flow.flow cx (funt, CallT (unknown_use, reason, callt))
          )
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | Scope.Generator ->
        (* Convert the return expression's type R to Generator<Y,R,N>, where
         * Y and R are internals, installed earlier. *)
        let reason = mk_reason (RCustom "generator return") loc in
        let t' = Flow.get_builtin_typeapp cx reason "Generator" [
          Env.get_internal_var cx "yield" loc;
          Tvar.mk_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_internal_var cx "next" loc
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | Scope.AsyncGenerator ->
        let reason = mk_reason (RCustom "async generator return") loc in
        let t' = Flow.get_builtin_typeapp cx reason "AsyncGenerator" [
          Env.get_internal_var cx "yield" loc;
          Tvar.mk_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_internal_var cx "next" loc
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | _ -> t
      in
      let use_op = Op (FunReturnStatement {
        value = Option.value_map argument ~default:(reason_of_t t) ~f:mk_expression_reason;
      }) in
      Flow.flow cx (t, UseT (use_op, ret));
      Env.reset_current_activation loc;
      Abnormal.save Abnormal.Return;
      Abnormal.throw_stmt_control_flow_exception
        ( loc, Return { Return.
            argument = argument_ast;
            comments = comments;
          } )
        Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      let argument_ast = expression cx argument in
      Env.reset_current_activation loc;
      Abnormal.save Abnormal.Throw;
      Abnormal.throw_stmt_control_flow_exception
        (loc, Throw { Throw.argument = argument_ast })
        Abnormal.Throw

  (***************************************************************************)
  (* Try-catch-finally statements have a lot of control flow possibilities. (To
     simplify matters, a missing catch block is considered to to be a catch
     block that throws, and a missing finally block is considered to be an empty
     block.)

     A try block may either

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the catch block.

     A catch block may either:

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the finally block and
     throws at the end of the finally block.

     A finally block may either:

     * exit normally: in this case, the try-catch-finally statement exits
     normally. (Note that to be in this case, either the try block exited
     normally, or it didn't and the catch block exited normally.)

     * exit abnormally: in this case, the try-catch-finally statement exits
     abnormally.

     Based on these possibilities, approximations for the local state at various
     points in a try-catch-finally statement can be derived.

     * The start of a catch block is reachable via anywhere in the try
     block. Thus, the local state must be conservative.

     * The start of a finally block is reachable via the end of the try block,
     or anywhere in the catch block. Thus, the local state must be conservative.

     * The end of a try-catch-finally statement is reachable via the end of the
     finally block. However, in this case we can assume that either

     ** the try block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of the
     try block.

     ** the catch block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of
     the catch block.

     Thus, a finally block should be analyzed twice, with each of the following
     assumptions for the local state at its start: (1) conservative (to model
     abnormal exits in the try or catch blocks); (2) whatever is at the end of
     the try block merged with whatever is at the end of the catch block (for
     normal exits in the try and catch blocks).

     Important to understand: since (1) is conservative, it should produce
     errors whenever (2) does, so that's not why we do them separately.
     But since (2) models exactly the states from which subsequent code is
     reachable, we can use its tighter approximation as the basis for
     subsequent analysis without loss of soundness.
   *)
  (***************************************************************************)
  | (loc, Try { Try.block = (b_loc, b); handler; finalizer }) ->
      let oldset = Changeset.Global.clear () in

      (* save ref to initial env and swap in a clone *)
      let start_env = Env.peek_env () in
      Env.(update_env cx loc (clone_env start_env));

      let try_block_ast, try_abnormal = Env.in_lex_scope cx (fun () ->
        Abnormal.catch_stmts_control_flow_exception (fun () ->
          toplevel_decls cx b.Block.body;
          toplevels cx b.Block.body
        )
      ) in

      (* save ref to env at end of try *)
      let try_env = Env.peek_env () in

      (* traverse catch block, save exceptions *)
      let catch_ast, catch_abnormal = match handler with
      | None ->
        (* a missing catch is equivalent to a catch that always throws *)
        None, Some Abnormal.Throw

      | Some (h_loc, h) ->
        (* if try throws to here, we need an env that's conservative
           over everything that happened from start_env to try_env *)
        Env.(
          let e = clone_env start_env in
          merge_env cx loc (e, e, try_env) (Changeset.Global.peek ());
          update_env cx loc e
        );

        let catch_block_ast, catch_abnormal = catch_clause cx h in
        Some (h_loc, catch_block_ast), catch_abnormal
      in

      (* save ref to env at end of catch *)
      let catch_env = Env.peek_env () in

      (* build initial env for non-throwing finally *)
      let nonthrow_finally_env = Env.(match catch_abnormal with
      | None ->
        (* if catch ends normally, then non-throwing finally can be
           reached via it or a non-throwing try. merge terminal states *)
        let e = clone_env start_env in
        merge_env cx loc (e, try_env, catch_env) (Changeset.Global.peek ());
        e
      | Some _ ->
        (* if catch throws, then the only way into non-throwing finally
           is via non-throwing try *)
        try_env
      ) in

      (* traverse finally block, save exceptions,
         and leave in place the terminal env of the non-throwing case
         (in which subsequent code is reachable) *)
      let finally_ast, finally_abnormal = match finalizer with
      | None ->
        Env.update_env cx loc nonthrow_finally_env;
        None, None

      | Some (f_loc, { Block.body }) ->
        (* analyze twice, with different start states *)

        (* 1. throwing-finally case. *)
        (* env may be in any state from start of try through end of catch *)
        Env.(
          let e = clone_env start_env in
          merge_env cx loc (e, e, catch_env) (Changeset.Global.peek ());
          update_env cx loc e
        );

        let _, finally_abnormal = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_stmts_control_flow_exception (fun () ->
            toplevel_decls cx body;
            toplevels cx body
          )
        ) in

        (* 2. non-throwing finally case. *)
        Env.update_env cx loc nonthrow_finally_env;

        (* (exceptions will be the same in both cases) *)
        let finally_block_ast, _ = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_stmts_control_flow_exception (fun () ->
            toplevel_decls cx body;
            toplevels cx body
          )
        ) in

        Some (f_loc, { Block.body = finally_block_ast }), finally_abnormal
      in

      let newset = Changeset.Global.merge oldset in
      ignore newset;

      let ast = loc, Try { Try.
        block = b_loc, { Block.body = try_block_ast };
        handler = catch_ast;
        finalizer = finally_ast;
      } in

      (* if finally has abnormal control flow, we throw here *)
      ignore (Abnormal.check_stmt_control_flow_exception (ast, finally_abnormal)
        : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t);

      (* other ways we throw due to try/catch abends *)
      begin match try_abnormal, catch_abnormal with
      | Some (Abnormal.Throw as try_abnormal), Some Abnormal.Throw
      | Some (Abnormal.Return as try_abnormal), Some _ ->
          Abnormal.throw_stmt_control_flow_exception ast try_abnormal

      | Some Abnormal.Throw, Some (Abnormal.Return as catch_abnormal) ->
          Abnormal.throw_stmt_control_flow_exception ast catch_abnormal

      | _ -> ast
      end


  (***************************************************************************)
  (* Refinements for `while` are derived by the following Hoare logic rule:

     [Pre' & c] S [Post']
     Pre' = Pre | Post'
     Post = Pre' & ~c
     ----------------------
     [Pre] while c S [Post]
  *)
  (***************************************************************************)
  | (loc, While { While.test; body }) ->
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      (* generate loop test preds and their complements *)
      let test_ast, preds, not_preds, orig_types =
        predicates_of_condition cx test in

      (* save current changeset and install an empty one *)
      let oldset = Changeset.Global.clear () in

      (* widen_env wraps specifics in tvars, anticipating widening inflows *)
      Env.widen_env cx loc;

      (* start_env is Pre above: env as of loop top *)
      let start_env = Env.peek_env () in

      (* swap in Pre & c *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc preds orig_types)
      );

      (* traverse loop body - after this, body_env = Post' *)
      let body_ast, _ = Abnormal.catch_stmt_control_flow_exception
        (fun () -> statement cx body) in

      (* save ref to env after loop body *)
      let body_env = Env.peek_env () in

      (* save loop body changeset to newset, install merged changes *)
      let newset = Changeset.Global.merge oldset in

      (* if we continued out of the loop, havoc vars changed by loop body *)
      if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
      then Env.havoc_vars newset;

      (* widen start_env with new specifics from body_env
         (turning Pre into Pre' = Pre | Post')
         then reinstall and add ~c to make Post *)
      Env.(
        copy_env cx loc (start_env, body_env) newset;
        update_env cx loc start_env;
        ignore (refine_with_preds cx loc not_preds orig_types)
      );

      (* if we broke out of the loop, havoc vars changed by loop body *)
      if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
      then Env.havoc_vars newset;

      loc, While { While.test = test_ast; body = body_ast }

  (***************************************************************************)
  (* Refinements for `do-while` are derived by the following Hoare logic rule:

     [Pre'] S [Post']
     Pre' = Pre | (Post' & c)
     Post = Post' & ~c
     -------------------------
     [Pre] do S while c [Post]
  *)
  (***************************************************************************)
  | (loc, DoWhile { DoWhile.body; test }) ->
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let env =  Env.peek_env () in
      let oldset = Changeset.Global.clear () in
      (* env = Pre *)
      (* ENV = [env] *)

      Env.widen_env cx loc;
      (* env = Pre', Pre' > Pre *)

      let body_env = Env.clone_env env in
      Env.update_env cx loc body_env;
      (* body_env = Pre' *)
      (* ENV = [body_env] *)

      let body_ast, body_abnormal =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
        |> Abnormal.ignore_break_or_continue_to_label None
      in

      if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
      then Env.havoc_vars (Changeset.Global.peek ());

      let test_ast, preds, not_preds, xtypes =
        predicates_of_condition cx test in
      (* body_env = Post' *)

      let done_env = Env.clone_env body_env in
      (* done_env = Post' *)

      let _ = Env.refine_with_preds cx loc preds xtypes in
      (* body_env = Post' & c *)

      let newset = Changeset.Global.merge oldset in
      Env.copy_env cx loc (env, body_env) newset;
      (* Pre' > Post' & c *)

      Env.update_env cx loc done_env;
      let _ = Env.refine_with_preds cx loc not_preds xtypes in
      if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
      then Env.havoc_vars newset;
      (* ENV = [done_env] *)
      (* done_env = Post' & ~c *)

      let ast = loc, DoWhile { DoWhile.body = body_ast; test = test_ast } in
      Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)

  (***************************************************************************)
  (* Refinements for `for` are derived by the following Hoare logic rule:

     [Pre] i [Init]
     [Pre' & c] S;u [Post']
     Pre' = Init | Post'
     Post = Pre' & ~c
     --------------------------
     [Pre] for (i;c;u) S [Post]

     NOTE: This rule is similar to that for `while`.
  *)
  (***************************************************************************)
  | (loc, For { For.init; test; update; body }) ->
      Env.in_lex_scope cx (fun () ->
        let save_break = Abnormal.clear_saved (Abnormal.Break None) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
        let init_ast = match init with
          | None -> None
          | Some (For.InitDeclaration (decl_loc, decl)) ->
              variable_decl cx decl;
              Some (For.InitDeclaration (decl_loc, variables cx decl))
          | Some (For.InitExpression expr) ->
              Some (For.InitExpression (expression cx expr))
        in

        let env =  Env.peek_env () in
        let oldset = Changeset.Global.clear () in
        Env.widen_env cx loc;

        let do_env = Env.clone_env env in
        Env.update_env cx loc do_env;

        let test_ast, preds, not_preds, xtypes = match test with
          | None ->
              None, Key_map.empty, Key_map.empty,
              Key_map.empty (* TODO: prune the "not" case *)
          | Some expr ->
              let expr_ast, preds, not_preds, xtypes =
                predicates_of_condition cx expr in
              Some expr_ast, preds, not_preds, xtypes
        in

        let body_env = Env.clone_env do_env in
        Env.update_env cx loc body_env;
        let _ = Env.refine_with_preds cx loc preds xtypes in

        let body_ast, _ = Abnormal.catch_stmt_control_flow_exception
          (fun () -> statement cx body) in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars (Changeset.Global.peek ());

        let update_ast =
          Option.map ~f:(expression cx) update in

        let newset = Changeset.Global.merge oldset in
        Env.copy_env cx loc (env, body_env) newset;

        Env.update_env cx loc do_env;
        let _ = Env.refine_with_preds cx loc not_preds xtypes in
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset;

        loc, For { For.
          init = init_ast;
          test = test_ast;
          update = update_ast;
          body = body_ast;
        }
      )

  (***************************************************************************)
  (* Refinements for `for-in` are derived by the following Hoare logic rule:

     [Pre] o [Init]
     [Pre'] S [Post']
     Pre' = Init | Post'
     Post = Pre'
     --------------------------
     [Pre] for (i in o) S [Post]
  *)
  (***************************************************************************)
  | (loc, ForIn { ForIn.left; right; body; each; }) ->
      let reason = mk_reason (RCustom "for-in") loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.Global.clear () in
        Env.widen_env cx loc;

        let body_env = Env.clone_env env in
        Env.update_env cx loc body_env;

        let eval_right () =
          let ((right_loc, _), _ as right_ast), preds, _, xtypes =
            predicates_of_condition cx right in
          let (_: Changeset.t) = Env.refine_with_preds cx right_loc preds xtypes in
          right_ast
        in

        let left_ast, right_ast = match left with
          | ForIn.LeftDeclaration (decl_loc, ({ VariableDeclaration.
              kind;
              declarations = [(vdecl_loc, { VariableDeclaration.Declarator.
                id;
                init = None;
              })]
            } as decl)) ->
              variable_decl cx decl;
              let right_ast = eval_right () in
              let id_ast, _ = variable cx kind id None
                ~if_uninitialized:(StrT.at %> with_trust bogus_trust) in
              ForIn.LeftDeclaration (decl_loc, { VariableDeclaration.
                kind;
                declarations = [(vdecl_loc, { VariableDeclaration.Declarator.
                  id = id_ast;
                  init = None;
                })];
              }), right_ast

          | ForIn.LeftPattern (pat_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
              name = name_loc, ({ Ast.Identifier.name= name_str; comments= _ } as id); optional; annot;
            }) ->
              let right_ast = eval_right () in
              let t = StrT.at pat_loc |> with_trust bogus_trust in
              let use_op = Op (AssignVar {
                var = Some (mk_reason (RIdentifier name_str) pat_loc);
                init = reason_of_t t;
              }) in
              ignore Env.(set_var cx ~use_op name_str t pat_loc);
              ForIn.LeftPattern ((pat_loc, t), Ast.Pattern.Identifier { Ast.Pattern.Identifier.
                name = ((name_loc, t), id);
                annot = (match annot with
                  | Ast.Type.Available _ ->
                    Tast_utils.unchecked_mapper#type_annotation_hint annot
                  | Ast.Type.Missing loc ->
                    Ast.Type.Missing (loc, t));
                optional;
              }), right_ast

          | _ ->
              let right_ast = eval_right () in
              Flow.add_output cx Error_message.(EInternal (loc, ForInLHS));
              Tast_utils.error_mapper#for_in_statement_lhs left, right_ast
        in

        let (_, right_t), _ = right_ast in
        Flow.flow cx (right_t, AssertForInRHST reason);

        let body_ast, _ = Abnormal.catch_stmt_control_flow_exception
          (fun () -> statement cx body) in

        let newset = Changeset.Global.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx loc (env,body_env) newset;

        Env.update_env cx loc env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset;

        loc, ForIn { ForIn.
          left = left_ast;
          right = right_ast;
          body = body_ast;
          each;
        }
      )

  | (loc, ForOf { ForOf.left; right; body; async; }) ->
      let reason_desc = match left with
      | ForOf.LeftDeclaration (_, {VariableDeclaration.declarations =
          [(_, {VariableDeclaration.Declarator.id = (_, Ast.Pattern.Identifier
            {Ast.Pattern.Identifier.name=(_, { Ast.Identifier.name; comments= _ }); _}); _})]; _}) -> RIdentifier name
      | ForOf.LeftPattern (_, Ast.Pattern.Identifier
          {Ast.Pattern.Identifier.name=(_, { Ast.Identifier.name; comments= _ }); _}) -> RIdentifier name
      | _ -> RCustom "for-of element"
      in
      let reason = mk_reason reason_desc loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      let eval_right () =
        let ((right_loc, t), _ as right_ast), preds, _, xtypes =
          predicates_of_condition cx right in
        let (_: Changeset.t) = Env.refine_with_preds cx right_loc preds xtypes in
        let elem_t = Tvar.mk cx reason in
        let o =
          (* Second and third args here are never relevant to the loop, but they should be as
             general as possible to allow iterating over arbitrary generators *)
          let targs = [
            elem_t;
            MixedT.why reason |> with_trust bogus_trust;
            EmptyT.why reason |> with_trust bogus_trust;
          ] in
          if async then
            let reason = mk_reason
              (RCustom "async iteration expected on AsyncIterable") loc in
            Flow.get_builtin_typeapp cx reason "$AsyncIterable" targs
          else
            Flow.get_builtin_typeapp cx
              (mk_reason (RCustom "iteration expected on Iterable") loc)
              "$Iterable" targs
        in
        Flow.flow_t cx (t, o); (* null/undefined are NOT allowed *)
        Flow.reposition cx (loc_of_t t) elem_t, right_ast
      in

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.Global.clear () in
        Env.widen_env cx loc;

        let body_env = Env.clone_env env in
        Env.update_env cx loc body_env;

        let left_ast, right_ast = match left with
          | ForOf.LeftDeclaration (decl_loc, ({ VariableDeclaration.
              kind;
              declarations = [(vdecl_loc, { VariableDeclaration.Declarator.
                id;
                init = None;
              })]
            } as decl)) ->
              variable_decl cx decl;
              let elem_t, right_ast = eval_right () in
              let id_ast, _ = variable cx kind id None
                ~if_uninitialized:(fun _ -> elem_t) in
              ForOf.LeftDeclaration (decl_loc, { VariableDeclaration.
                kind;
                declarations = [(vdecl_loc, { VariableDeclaration.Declarator.
                  id = id_ast;
                  init = None;
                })];
              }), right_ast

          | ForOf.LeftPattern (pat_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
              name = name_loc, ({ Ast.Identifier.name= name_str; comments=_ } as id); optional; annot;
            }) ->
              let elem_t, right_ast = eval_right () in
              let use_op = Op (AssignVar {
                var = Some (mk_reason (RIdentifier name_str) pat_loc);
                init = reason_of_t elem_t;
              }) in
              ignore Env.(set_var cx ~use_op name_str elem_t pat_loc);
              ForOf.LeftPattern (
                (pat_loc, elem_t),
                Ast.Pattern.Identifier { Ast.Pattern.Identifier.
                  name = ((name_loc, elem_t), id);
                  annot = (match annot with
                    | Ast.Type.Available annot ->
                      Ast.Type.Available (Tast_utils.error_mapper#type_annotation annot)
                    | Ast.Type.Missing loc ->
                      Ast.Type.Missing (loc, elem_t));
                  optional;
                }
              ), right_ast

          | _ ->
              let _, right_ast = eval_right () in
              Flow.add_output cx Error_message.(EInternal (loc, ForOfLHS));
              Tast_utils.error_mapper#for_of_statement_lhs left, right_ast
        in

        let body_ast, _ = Abnormal.catch_stmt_control_flow_exception
          (fun () -> statement cx body) in

        let newset = Changeset.Global.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx loc (env,body_env) newset;

        Env.update_env cx loc env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset;

        loc, ForOf { ForOf.
          left = left_ast;
          right = right_ast;
          body = body_ast;
          async;
        }
      )

  | (_, Debugger) as stmt -> stmt

  | (loc, FunctionDeclaration func) ->
      let {Ast.Function.id; sig_loc; _} = func in
      let fn_type, func_ast = mk_function_declaration None cx sig_loc func in
      (match id with
      | Some(_, { Ast.Identifier.name; comments= _ }) ->
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) loc);
          init = reason_of_t fn_type
        }) in
        Env.init_fun cx ~use_op name fn_type loc
      | None -> ());
      loc, FunctionDeclaration func_ast

  | loc, EnumDeclaration enum ->
    if not @@ Context.enable_enums cx then
      Flow.add_output cx (Error_message.EExperimentalEnums loc);
    let open EnumDeclaration in
    let {id = id_loc, ident; body} = enum in
    let t = AnyT.untyped @@ mk_reason REnumDeclaration loc in
    let id' = (id_loc, t), ident in
    loc, EnumDeclaration {id = id'; body}

  | (loc, DeclareVariable { DeclareVariable.
      id = id_loc, ({ Ast.Identifier.name; comments= _ } as id);
      annot;
    }) ->
      let r = mk_reason (RCustom (spf "declare %s" name)) loc in
      let t, annot_ast = Anno.mk_type_annotation cx SMap.empty r annot in
      Env.unify_declared_type cx name t;
      loc, DeclareVariable { DeclareVariable.
        id = (id_loc, t), id;
        annot = annot_ast;
      }

  | (loc, DeclareFunction declare_function) ->
      (match declare_function_to_function_declaration cx loc declare_function with
      | Some (func_decl, reconstruct_ast) ->
          loc, DeclareFunction (reconstruct_ast (statement cx (loc, func_decl)))
      | None -> (* error case *)
          let { DeclareFunction.id = (id_loc, id_name); annot; predicate; _ } = declare_function in
          let { Ast.Identifier.name; comments = _ } = id_name in
          let t, annot_ast =
            Anno.mk_type_available_annotation cx SMap.empty annot in
          Env.unify_declared_fun_type cx name loc t;
          let predicate = Option.map ~f:Tast_utils.error_mapper#type_predicate predicate in
          loc, DeclareFunction { DeclareFunction.
            id = (id_loc, t), id_name;
            annot = annot_ast;
            predicate;
          }
      )

  | (loc, VariableDeclaration decl) ->
      loc, VariableDeclaration (variables cx decl)

  | (class_loc, ClassDeclaration c) ->
      let (name_loc, name) = extract_class_name class_loc c in
      let reason = DescFormat.instance_reason name name_loc in
      Env.declare_implicit_let Scope.Entry.ClassNameBinding cx name name_loc;
      let class_t, c_ast = mk_class cx class_loc ~name_loc reason c in
      Env.init_implicit_let
        Scope.Entry.ClassNameBinding
        cx
        ~use_op:(Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) name_loc);
          init = reason_of_t class_t;
        }))
        name
        ~has_anno:false
        class_t
        name_loc;
      class_loc, ClassDeclaration c_ast

  | (loc, DeclareClass decl) ->
    loc, DeclareClass (declare_class cx loc decl)

  | (loc, DeclareInterface decl) ->
    loc, DeclareInterface (interface cx loc decl)
  | (loc, InterfaceDeclaration decl) ->
    loc, InterfaceDeclaration (interface cx loc decl)

  | (loc, DeclareModule { DeclareModule.id; body; kind; }) ->
    let _, name = match id with
    | DeclareModule.Identifier (id_loc, { Ast.Identifier.name= value; comments= _ })
    | DeclareModule.Literal (id_loc, { Ast.StringLiteral.value; _ }) ->
      id_loc, value
    in
    let body_loc, { Ast.Statement.Block.body = elements } = body in

    let module_ref = Reason.internal_module_name name in

    let module_scope = Scope.fresh () in
    Scope.add_entry
      (Reason.internal_name "exports")
      (Scope.Entry.new_var
        ~loc:ALoc.none
        ~specific:(Locationless.EmptyT.t |> with_trust bogus_trust)
        (Locationless.MixedT.t |> with_trust bogus_trust))
      module_scope;

    Env.push_var_scope cx module_scope;
    Context.push_declare_module cx (Module_info.empty_cjs_module module_ref);

    let elements_ast, elements_abnormal =
      Abnormal.catch_stmts_control_flow_exception (fun () ->
        toplevel_decls cx elements;
        toplevels cx elements;
      )
    in

    let reason = mk_reason (RModule name) loc in

    let () = match Context.module_kind cx with
    | Module_info.ES _ -> ()
    | Module_info.CJS clobbered ->
      let open Scope in
      let open Entry in
      let () = match clobbered with
      | Some _ -> ()
      | None ->
        let props = SMap.fold (fun x entry acc ->
          match entry with
          | Value {specific; _} ->
            let loc = Some (entry_loc entry) in
            Properties.add_field x Polarity.Positive loc specific acc
          | Type _ | Class _ -> acc
        ) module_scope.entries SMap.empty in
        let proto = ObjProtoT reason in
        let t = Obj_type.mk_with_proto cx reason ~props proto in
        Import_export.set_module_exports cx loc t
      in
      SMap.iter (fun x entry ->
        match entry with
        | Type {type_; type_binding_kind=TypeBinding; _} ->
          (* TODO we may want to provide a location here *)
          Import_export.export_type cx x None type_
        | Type {type_binding_kind=ImportTypeBinding; _ }
        | Value _ | Class _ -> ()
      ) module_scope.entries;
    in

    let module_t = Import_export.mk_module_t cx reason in

    let ast = loc, DeclareModule { DeclareModule.
      id = begin match id with
        | DeclareModule.Identifier (id_loc, id) ->
          DeclareModule.Identifier ((id_loc, module_t), id)
        | DeclareModule.Literal (id_loc, lit) ->
          DeclareModule.Literal ((id_loc, module_t), lit)
        end;
      body = body_loc, { Block.body = elements_ast };
      kind;
    } in
    ignore (Abnormal.check_stmt_control_flow_exception (ast, elements_abnormal)
      : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t);

    let t = Env.get_var_declared_type cx module_ref loc in
    Flow.flow_t cx (module_t, t);

    Context.pop_declare_module cx;
    Env.pop_var_scope ();

    ast



  | (loc, DeclareExportDeclaration ({ DeclareExportDeclaration.
      default; declaration; specifiers; source;
    } as decl)) ->
      let open DeclareExportDeclaration in
      let export_info, export_kind, declaration =
      (*  error-handling around calls to `statement` is omitted here because we
          don't expect declarations to have abnormal control flow *)
        match declaration with
        | Some (Variable (loc, v)) ->
            let { DeclareVariable.id = (_, { Ast.Identifier.name; comments= _ }); _; } = v in
            let dec_var = statement cx (loc, DeclareVariable v) in
            let ast = match dec_var with
              | _, DeclareVariable v_ast -> Some (Variable (loc, v_ast))
              | _ -> assert_false "DeclareVariable typed AST doesn't preserve structure"
            in
            [(spf "var %s" name, loc, name, None)], Ast.Statement.ExportValue, ast
        | Some (Function (loc, f)) ->
            let { DeclareFunction.id = (_, { Ast.Identifier.name; comments= _ }); _ } = f in
            let dec_fun = statement cx (loc, DeclareFunction f) in
            let ast = match dec_fun with
              | _, DeclareFunction f_ast -> Some (Function (loc, f_ast))
              | _ -> assert_false "DeclareFunction typed AST doesn't preserve structure"
            in
            [(spf "function %s() {}" name, loc, name, None)], Ast.Statement.ExportValue, ast
        | Some (Class (loc, c)) ->
            let { DeclareClass.id = (name_loc, { Ast.Identifier.name; comments= _ }); _; } = c in
            let dec_class = statement cx (loc, DeclareClass c) in
            let ast = match dec_class with
              | _, DeclareClass c_ast -> Some (Class (loc, c_ast))
              | _ -> assert_false "DeclareClass typed AST doesn't preserve structure"
            in
            [(spf "class %s {}" name, name_loc, name, None)], Ast.Statement.ExportValue, ast
        | Some (DefaultType (loc, t)) ->
            let (_, _type), _ as t_ast = Anno.convert cx SMap.empty (loc, t) in
            let ast = Some (DefaultType t_ast) in
            [( "<<type>>", loc, "default", Some _type)], Ast.Statement.ExportValue, ast
        | Some (NamedType (talias_loc, ({
            TypeAlias.
            id = (name_loc, { Ast.Identifier.name; comments= _ });
            _;
          } as talias))) ->
            let type_alias = statement cx (talias_loc, TypeAlias talias) in
            let ast = match type_alias with
              | _, TypeAlias talias -> Some (NamedType (talias_loc, talias))
              | _ -> assert_false "TypeAlias typed AST doesn't preserve structure"
            in
            [(spf "type %s = ..." name, name_loc, name, None)], Ast.Statement.ExportType, ast
        | Some (NamedOpaqueType (opaque_loc, ({
            OpaqueType.
            id = (name_loc, { Ast.Identifier.name; comments= _ });
            _;
          } as opaque_t))) ->
            let opaque_type = statement cx (opaque_loc, OpaqueType opaque_t) in
            let ast = match opaque_type with
              | _, OpaqueType opaque_t -> Some (NamedOpaqueType (opaque_loc, opaque_t))
              | _ -> assert_false "OpaqueType typed AST doesn't preserve structure"
            in
            [(spf "opaque type %s = ..." name, name_loc, name, None)], Ast.Statement.ExportType, ast
        | Some (Interface (loc, i)) ->
            let {Interface.id = (name_loc, { Ast.Identifier.name; comments= _ }); _;} = i in
            let int_dec = statement cx (loc, InterfaceDeclaration i) in
            let ast = match int_dec with
              | _, InterfaceDeclaration i_ast -> Some (Interface (loc, i_ast))
              | _ -> assert_false "InterfaceDeclaration typed AST doesn't preserve structure"
            in
            [(spf "interface %s {}" name, name_loc, name, None)], Ast.Statement.ExportType, ast
        | None ->
            [], Ast.Statement.ExportValue, None
      in

      export_statement cx loc ~default export_info specifiers source export_kind;

      loc, DeclareExportDeclaration { decl with DeclareExportDeclaration.declaration }

  | (loc, DeclareModuleExports (t_loc, t)) ->
    let (_, t), _ as t_ast = Anno.convert cx SMap.empty t in
    Import_export.cjs_clobber cx loc t;
    loc, DeclareModuleExports (t_loc, t_ast)

  | (loc, ExportNamedDeclaration ({ ExportNamedDeclaration.
      declaration; specifiers; source; exportKind;
    } as export_decl)) ->
      let declaration, export_info = match declaration with
      | Some decl ->
          Some (statement cx decl),
          (match decl with
          | _, FunctionDeclaration {Ast.Function.id = None; _} ->
            failwith (
              "Parser Error: Immediate exports of nameless functions can " ^
              "only exist for default exports!"
            )
          | _, FunctionDeclaration {Ast.Function.id = Some (id_loc, { Ast.Identifier.name; comments= _ }); _} ->
            Type_inference_hooks_js.dispatch_export_named_hook name id_loc;
            [(spf "function %s() {}" name, id_loc, name, None)]
          | _, ClassDeclaration {Ast.Class.id = None; _} ->
            failwith (
              "Parser Error: Immediate exports of nameless classes can " ^
              "only exist for default exports"
            )
          | _, ClassDeclaration {Ast.Class.id = Some (id_loc, { Ast.Identifier.name; comments= _ }); _} ->
            Type_inference_hooks_js.dispatch_export_named_hook name id_loc;
            [(spf "class %s {}" name, id_loc, name, None)]
          | _, VariableDeclaration {VariableDeclaration.declarations; _} ->
            Flow_ast_utils.fold_bindings_of_variable_declarations (fun acc (loc, { Ast.Identifier.name; comments= _ }) ->
              Type_inference_hooks_js.dispatch_export_named_hook name loc;
              (spf "var %s" name, loc, name, None)::acc
            ) [] declarations
            |> List.rev
          | _, TypeAlias {TypeAlias.id; _} ->
            let name = ident_name id in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, OpaqueType {OpaqueType.id; _} ->
            let name = ident_name id in
            [(spf "opaque type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration {Interface.id; _} ->
            let name = ident_name id in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | None -> None, [] in

      export_statement cx loc ~default:None export_info specifiers source exportKind;

      loc, ExportNamedDeclaration { export_decl with ExportNamedDeclaration.declaration }


  | (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration }) ->
      Type_inference_hooks_js.dispatch_export_named_hook "default" default;
      let declaration, export_info = match declaration with
      | ExportDefaultDeclaration.Declaration decl ->
          let decl, undo_nameify = Import_export.nameify_default_export_decl decl in
          ExportDefaultDeclaration.Declaration (undo_nameify (statement cx decl)),
          (match decl with
          | loc, FunctionDeclaration {Ast.Function.id = None; _} ->
            [("function() {}", loc, internal_name "*default*", None)]
          | loc, FunctionDeclaration {Ast.Function.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "function %s() {}" name, loc, name, None)]
          | loc, ClassDeclaration {Ast.Class.id = None; _} ->
            [("class {}", loc, internal_name "*default*", None)]
          | _, ClassDeclaration {Ast.Class.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "class %s {}" name, (fst ident), name, None)]
          | _, VariableDeclaration {VariableDeclaration.declarations; _} ->
            Flow_ast_utils.fold_bindings_of_variable_declarations (fun acc (loc, { Ast.Identifier.name; comments= _ }) ->
              (spf "var %s" name, loc, name, None)::acc
            ) [] declarations
            |> List.rev
          | _, TypeAlias {TypeAlias.id; _} ->
            let name = ident_name id in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, OpaqueType {OpaqueType .id; _} ->
            let name = ident_name id in
            [(spf "opaque type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration {Interface.id; _} ->
            let name = ident_name id in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | ExportDefaultDeclaration.Expression expr ->
          let (_, expr_t), _ as expr_ast = expression cx expr in
          ExportDefaultDeclaration.Expression expr_ast,
          [( "<<expression>>", fst expr, "default", Some expr_t)]
      in

      (* export default is always a value *)
      let exportKind = Ast.Statement.ExportValue in

      export_statement cx loc ~default:(Some default) export_info None None exportKind;

      loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration; }

  | (import_loc, ImportDeclaration import_decl) ->
    Context.add_import_stmt cx import_decl;

    let { ImportDeclaration.source; specifiers; default; importKind } = import_decl in

    let source_loc, { Ast.StringLiteral.value = module_name; _ } = source in

    let type_kind_of_kind = function
      | ImportDeclaration.ImportType -> Type.ImportType
      | ImportDeclaration.ImportTypeof -> Type.ImportTypeof
      | ImportDeclaration.ImportValue -> Type.ImportValue
    in

    let module_t = Import_export.import cx (source_loc, module_name) import_loc in

    let get_imported_t get_reason import_kind remote_export_name local_name =
      Tvar.mk_where cx get_reason (fun t ->
        let import_type =
          if remote_export_name = "default"
          then ImportDefaultT
            (get_reason, import_kind, (local_name, module_name), t, Context.is_strict cx)
          else ImportNamedT
            (get_reason, import_kind, remote_export_name, module_name, t, Context.is_strict cx)
        in
        Context.add_imported_t cx local_name t;
        Flow.flow cx (module_t, import_type)
      )
    in

    let specifiers, specifiers_ast = match specifiers with
      | Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers) ->
        let named_specifiers, named_specifiers_ast =
        named_specifiers
        |> Core_list.map ~f:(function { ImportDeclaration.local; remote; kind;} ->
          let (remote_name_loc, ({ Ast.Identifier.name= remote_name; comments= _ } as rmt)) = remote in
          let (loc, { Ast.Identifier.name= local_name; comments= _ }) = Option.value ~default:remote local in
          let imported_t =
            let import_reason =
              mk_reason (RNamedImportedType (module_name, local_name)) (fst remote)
            in
            if Type_inference_hooks_js.dispatch_member_hook
              cx remote_name remote_name_loc module_t
            then Unsoundness.why InferenceHooks import_reason
            else
              let import_kind = type_kind_of_kind (Option.value ~default:importKind kind) in
              get_imported_t import_reason import_kind remote_name local_name
          in
          let remote_ast = (remote_name_loc, imported_t), rmt in
          let local_ast = Option.map local ~f:(fun (local_loc, local_id) ->
            let { Ast.Identifier.name = local_name; comments } = local_id in
            (local_loc, imported_t), mk_ident ~comments local_name
          ) in
          (loc, local_name, imported_t, kind),
          { ImportDeclaration.
            local = local_ast;
            remote = remote_ast;
            kind;
          }
        )
        |> List.split
        in
        named_specifiers,
        Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers_ast)

      | Some (ImportDeclaration.ImportNamespaceSpecifier (ns_loc, local)) as specifiers ->
        let local_name = ident_name local in

        Type_inference_hooks_js.dispatch_import_hook cx (source_loc, module_name) ns_loc;

        let import_reason =
          let import_reason_desc =
            match importKind with
            | ImportDeclaration.ImportType -> RImportStarType local_name
            | ImportDeclaration.ImportTypeof -> RImportStarTypeOf local_name
            | ImportDeclaration.ImportValue -> RImportStar local_name
          in
          mk_reason import_reason_desc import_loc
        in

        begin match importKind with
          | ImportDeclaration.ImportType ->
            assert_false "import type * is a parse error"
          | ImportDeclaration.ImportTypeof ->
            let bind_reason = repos_reason (fst local) import_reason in
            let module_ns_t =
              Import_export.import_ns cx import_reason (fst source, module_name) import_loc
            in
            let module_ns_typeof =
              Tvar.mk_where cx bind_reason (fun t ->
                Context.add_imported_t cx local_name t;
                Flow.flow cx (module_ns_t,
                  ImportTypeofT (bind_reason, "*", t))
              )
            in
            [import_loc, local_name, module_ns_typeof, None]
          | ImportDeclaration.ImportValue ->
            let reason =
              mk_reason (RModule module_name) import_loc
            in
            let module_ns_t =
              Import_export.import_ns cx reason (fst source, module_name) import_loc
            in
            Context.add_imported_t cx local_name module_ns_t;
            [fst local, local_name, module_ns_t, None]
        end,
        specifiers
      | None -> [], None
    in

    let specifiers, default_ast = match default with
      | Some local ->
          let loc, ({ Ast.Identifier.name= local_name; comments= _ } as id) = local in
          let import_reason = mk_reason (RDefaultImportedType (local_name, module_name)) loc in
          let imported_t =
            if Type_inference_hooks_js.dispatch_member_hook
              cx "default" loc module_t
            then Unsoundness.why InferenceHooks import_reason
            else
              let import_kind = type_kind_of_kind importKind in
              get_imported_t import_reason import_kind "default" local_name
          in
          (loc, local_name, imported_t, None) :: specifiers,
          Some ((loc, imported_t), id)
      | None -> specifiers, None
    in

    List.iter (fun (loc, local_name, t, specifier_kind) ->
      let t_generic =
        let lookup_mode =
          match Option.value ~default:importKind specifier_kind with
          | ImportDeclaration.ImportType -> ForType
          | ImportDeclaration.ImportTypeof -> ForType
          | ImportDeclaration.ImportValue -> ForValue
        in
        Env.get_var_declared_type ~lookup_mode cx local_name loc
      in
      Flow.unify cx t t_generic
    ) specifiers;

    import_loc,
    ImportDeclaration { ImportDeclaration.
      source;
      specifiers = specifiers_ast;
      default = default_ast;
      importKind;
    }
)


and export_statement cx loc
  ~default declaration_export_info specifiers source exportKind =

  let open Ast.Statement in

  let lookup_mode = match exportKind with
    | Ast.Statement.ExportValue -> ForValue
    | Ast.Statement.ExportType -> ForType
  in

  let export_from_local (_, loc, local_name, local_tvar) = (
    let local_tvar = match local_tvar with
    | None -> Env.var_ref ~lookup_mode cx local_name loc
    | Some t -> t in

    let local_name = if (Option.is_some default) then "default" else local_name in

    (* Use the location of the "default" keyword if this is a default export. For named exports,
     * use the location of the identifier. *)
    let loc = Option.value ~default:loc default in
    match exportKind with
    | Ast.Statement.ExportType ->
      Import_export.export_type cx local_name (Some loc) local_tvar
    | Ast.Statement.ExportValue ->
      Import_export.export cx local_name loc local_tvar
  ) in

  (match (declaration_export_info, specifiers) with
    (* [declare] export [type] {foo, bar} [from ...]; *)
    | ([], Some (ExportNamedDeclaration.ExportSpecifiers specifiers)) ->
      let export_specifier specifier = (
        let loc, reason, local_name, remote_name =
          match specifier with
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, { Ast.Identifier.name= id; comments= _ });
              exported = None;
            } ->
            let reason = mk_reason (RCustom (spf "export {%s}" id)) loc in
            (loc, reason, id, id)
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, { Ast.Identifier.name= local; comments= _ });
              exported = Some (_, { Ast.Identifier.name= exported; comments= _ });
            } ->
            let reason =
              mk_reason (RCustom (spf "export {%s as %s}" local exported)) loc
            in
            (loc, reason, local, exported)
        in

        (**
          * Determine if we're dealing with the `export {} from` form
          * (and if so, retrieve the ModuleNamespaceObject tvar for the
          *  source module)
          *)
        let source_module_tvar = (
          match source with
          | Some (src_loc, { Ast.StringLiteral.value = module_name; _ }) ->
            let reason =
              mk_reason (RCustom "ModuleNamespace for export {} from") src_loc
            in
            Some (Import_export.import_ns cx reason (src_loc, module_name) loc)
          | None -> None
        ) in

        let local_tvar = (
          match source_module_tvar with
          | Some(tvar) ->
            Tvar.mk_where cx reason (fun t ->
              Flow.flow cx (tvar, GetPropT (unknown_use, reason, Named (reason, local_name), t))
            )
          | None ->
            Env.var_ref ~lookup_mode cx local_name loc
        ) in

        match exportKind with
        | Ast.Statement.ExportType ->
          Import_export.export_type cx remote_name (Some loc) local_tvar
        | Ast.Statement.ExportValue ->
          Import_export.export cx remote_name loc local_tvar
      ) in
      List.iter export_specifier specifiers

    (* [declare] export [type] * from "source"; *)
    | [],
      Some (ExportNamedDeclaration.ExportBatchSpecifier
        (batch_loc, star_as_name)
      ) ->
      let source_loc, source_module_name = (
        match source with
        | Some (loc, { Ast.StringLiteral.value; _ }) -> loc, value
        | None -> failwith (
          "Parser Error: `export * from` must specify a string " ^
          "literal for the source module name!"
        )
      ) in

      Import_export.warn_or_ignore_export_star_as cx star_as_name;

      let parse_export_star_as = Context.esproposal_export_star_as cx in
      (match star_as_name with
      | Some ident ->
        let (_, { Ast.Identifier.name; comments= _ }) = ident in
        let reason =
          mk_reason
            (RCustom (spf "export * as %s from %S" name source_module_name))
            loc
        in

        let remote_namespace_t =
          if parse_export_star_as = Options.ESPROPOSAL_ENABLE
          then Import_export.import_ns cx reason (source_loc, source_module_name) loc
          else AnyT.untyped (
            let config_value =
              if parse_export_star_as = Options.ESPROPOSAL_IGNORE
              then "ignore"
              else "warn"
            in
            let reason =
              spf "flowconfig: esproposal.export_star_as=%s" config_value in
            mk_reason (RCustom reason) batch_loc
          )
        in

        Import_export.export cx name loc remote_namespace_t;
      | None ->
        let source_module_t =
          Import_export.import cx (source_loc, source_module_name) loc
        in

        match exportKind with
        | Ast.Statement.ExportValue ->
          Import_export.export_star cx loc source_module_t
        | Ast.Statement.ExportType ->
          Import_export.export_type_star cx loc source_module_t
      )

    | ([], None) -> failwith (
        "Parser Error: Export statement missing one of: Declaration, " ^
        "Expression, or Specifier list!"
      )
    | (_, Some _) -> failwith (
        "Parser Error: Export statement with a declaration/expression " ^
        "cannot also include a list of specifiers!"
      )

    (* [declare] export [type] [default] <<declaration>>; *)
    | (export_info, None) ->
      (**
        * Export each declared binding. Some declarations export multiple
        * bindings, like a multi-declarator variable declaration.
        *)
      List.iter export_from_local export_info
  )

and object_prop cx map prop = Ast.Expression.Object.(
  match prop with
  (* named prop *)
  | Property (prop_loc, Property.Init {
      key =
        (Property.Identifier (loc, { Ast.Identifier.name; comments= _ }) |
        Property.Literal (loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        })) as key;
      value = v; shorthand; }) ->
    let (_, t), _ as v = expression cx v in
    Properties.add_field name Polarity.Neutral (Some loc) t map,
    Property (prop_loc, Property.Init {
      key = translate_identifier_or_literal_key t key;
      value = v;
      shorthand
    })

  (* named method *)
  | Property (prop_loc, Property.Method {
      key =
        (Property.Identifier (loc, { Ast.Identifier.name; comments= _ }) |
        Property.Literal (loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        })) as key;
      value = (fn_loc, func);
    }) ->
    let (_, t), v = expression cx (fn_loc, Ast.Expression.Function func) in
    let func = match v with Ast.Expression.Function func -> func | _ -> assert false in
    Properties.add_field name Polarity.Neutral (Some loc) t map,
    Property (prop_loc, Property.Method {
      key = translate_identifier_or_literal_key t key;
      value = fn_loc, func
    })

  (* We enable some unsafe support for getters and setters. The main unsafe bit
  *  is that we don't properly havok refinements when getter and setter methods
  *  are called. *)

  (* unsafe getter property *)
  | Property (loc, Property.Get {
      key =
        (Property.Identifier (id_loc, { Ast.Identifier.name; comments= _ }) |
        Property.Literal (id_loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        })) as key;
      value = (vloc, func);
    }) ->
    Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
    let function_type, func = mk_function_expression None cx vloc func in
    let return_t = Type.extract_getter_type function_type in
    Properties.add_getter name (Some id_loc) return_t map,
    Property (loc, Property.Get {
      key = translate_identifier_or_literal_key return_t key;
      value = vloc, func;
    })

  (* unsafe setter property *)
  | Property (loc, Property.Set {
      key =
        (Property.Identifier (id_loc, { Ast.Identifier.name; comments= _ }) |
        Property.Literal (id_loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        })) as key;
      value = vloc, func;
    }) ->
    Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
    let function_type, func = mk_function_expression None cx vloc func in
    let param_t = Type.extract_setter_type function_type in
    Properties.add_setter name (Some id_loc) param_t map,
    Property (loc, Property.Set {
      key = translate_identifier_or_literal_key param_t key;
      value = vloc, func;
    })

  (* non-string literal LHS *)
  | Property (loc, Property.Init { key = Property.Literal _; _ })
  | Property (loc, Property.Method { key = Property.Literal _; _ })
  | Property (loc, Property.Get { key = Property.Literal _; _ })
  | Property (loc, Property.Set { key = Property.Literal _; _ }) ->
    Flow.add_output cx
      Error_message.(EUnsupportedSyntax (loc, ObjectPropertyLiteralNonString));
    map, Tast_utils.error_mapper#object_property_or_spread_property prop

  (* computed getters and setters aren't supported yet regardless of the
     `enable_getters_and_setters` config option *)
  | Property (loc, Property.Get { key = Property.Computed _; _ })
  | Property (loc, Property.Set { key = Property.Computed _; _ }) ->
    Flow.add_output cx
      Error_message.(EUnsupportedSyntax (loc, ObjectPropertyComputedGetSet));
    map, Tast_utils.error_mapper#object_property_or_spread_property prop

  (* computed LHS silently ignored for now *)
  | Property (_, Property.Init { key = Property.Computed _; _ })
  | Property (_, Property.Method { key = Property.Computed _; _ }) ->
    map, Tast_utils.error_mapper#object_property_or_spread_property prop

  (* spread prop *)
  | SpreadProperty _ ->
    map, Tast_utils.error_mapper#object_property_or_spread_property prop

  | Property (_, Property.Init { key = Property.PrivateName _; _ })
  | Property (_, Property.Method { key = Property.PrivateName _; _ })
  | Property (_, Property.Get { key = Property.PrivateName _; _ })
  | Property (_, Property.Set { key = Property.PrivateName _; _ }) ->
    failwith "Internal Error: Non-private field with private name"
)

and prop_map_of_object cx props =
  let map, rev_prop_asts = List.fold_left (fun (map, rev_prop_asts) prop ->
    let map, prop = object_prop cx map prop in map, prop::rev_prop_asts
  ) (SMap.empty, []) props in map, List.rev rev_prop_asts

and object_ cx reason ?(allow_sealed=true) props =
  let open Ast.Expression.Object in

  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let obj_proto = ObjProtoT reason in

  (* Return an object with specified sealing. *)
  let mk_object ?(proto=obj_proto) ?(sealed=false) props =
    Obj_type.mk_with_proto cx reason ~sealed ~props proto
  in

  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj ~assert_exact =
    let use_op = Op (ObjectSpread {op = (reason_of_t from_obj)}) in
    Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (to_obj, ObjAssignToT(use_op, reason, from_obj, t, ObjAssign { assert_exact }));
    )
  in

  (* Add property to object, using optional tout argument to SetElemT to wait
     for the write to happen. This defers any reads until writes have happened,
     to avoid race conditions. *)
  let mk_computed key value obj =
    Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (obj, SetElemT (unknown_use, reason, key, value, Some t))
    )
  in

  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_object ?(proto=obj_proto) ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~proto ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object ~proto map) result ~assert_exact:false
        else result
      in
      if not sealed then result else
        Tvar.mk_where cx reason (fun t ->
          Flow.flow cx (result, ObjSealT (reason, t))
        )
  in

  let sealed, map, proto, result, rev_prop_asts = List.fold_left (
    fun (sealed, map, proto, result, rev_prop_asts) -> function
    (* Enforce that the only way to make unsealed object literals is ...{} (spreading empty object
       literals). Otherwise, spreading always returns sealed object literals.

       Also enforce that a spread of an inexact object can only appear as the first element of an
       object literal, because otherwise we cannot determine the type of the object literal without
       significantly losing precision about elements preceding that spread.

       Finally, the exactness of an object literal type is determined solely by its sealedness.

       TODO: This treatment of spreads is oblivious to issues that arise when spreading expressions
       of union type.
    *)
    | SpreadProperty (prop_loc, { SpreadProperty.argument }) ->
        let (_, spread), _ as argument = expression cx argument in
        let not_empty_object_literal_argument = match spread with
          | DefT (_, _, ObjT { flags; _ }) -> Obj_type.sealed_in_op reason flags.sealed
          | _ -> true in
        let obj = eval_object (map, result) in
        let result = mk_spread spread obj
          ~assert_exact:(not (SMap.is_empty map && result = None)) in
        sealed && not_empty_object_literal_argument,
        SMap.empty,
        proto,
        Some result,
        SpreadProperty (prop_loc, { SpreadProperty.
          argument;
        })::rev_prop_asts
    | Property (prop_loc, Property.Init {
        key = Property.Computed k;
        value = v;
        shorthand;
      }) ->
        let (_, kt), _ as k = expression cx k in
        let (_, vt), _ as v = expression cx v in
        let obj = eval_object (map, result) in
        let result = mk_computed kt vt obj in
        sealed,
        SMap.empty,
        proto,
        Some result,
        Property (prop_loc, Property.Init {
          key = Property.Computed k;
          value = v;
          shorthand;
        })::rev_prop_asts
    | Property (prop_loc, Property.Method {
        key = Property.Computed k;
        value = fn_loc, fn;
      }) ->
        let (_, kt), _ as k = expression cx k in
        let (_, vt), v = expression cx (fn_loc, Ast.Expression.Function fn) in
        let fn = match v with Ast.Expression.Function fn -> fn | _ -> assert false in
        let obj = eval_object (map, result) in
        let result = mk_computed kt vt obj in
        sealed,
        SMap.empty,
        proto,
        Some result,
        Property (prop_loc, Property.Method {
          key = Property.Computed k;
          value = fn_loc, fn;
        })::rev_prop_asts
    | Property (prop_loc, Property.Init {
        key =
          (Property.Identifier (_, { Ast.Identifier.name= "__proto__"; comments= _ }) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String "__proto__";
            _;
          })) as key;
        value = v;
        shorthand = false;
      }) ->
        let reason = mk_reason RPrototype (fst v) in
        let (_, vt), _ as v = expression cx v in
        let t = Tvar.mk_where cx reason (fun t ->
          Flow.flow cx (vt, ObjTestProtoT (reason, t))
        ) in
        sealed,
        map,
        Some t,
        result,
        Property (prop_loc, Property.Init {
          key = translate_identifier_or_literal_key vt key;
          value = v;
          shorthand = false;
        })::rev_prop_asts
    | prop ->
        let map, prop = object_prop cx map prop in
        sealed, map, proto, result, prop::rev_prop_asts
  ) (allow_sealed, SMap.empty, None, None, []) props in

  let sealed = match result with
    | Some _ -> sealed
    | None -> sealed && not (SMap.is_empty map)
  in
  eval_object ?proto ~sealed (map, result),
  List.rev rev_prop_asts

and variable cx kind ?if_uninitialized id init = Ast.Statement.(
  let init_var, declare_var = match kind with
    | VariableDeclaration.Const -> Env.init_const, Env.declare_const
    | VariableDeclaration.Let -> Env.init_let, Env.declare_let
    | VariableDeclaration.Var -> Env.init_var, (fun _ _ _ -> ())
  in

  Ast.Expression.(match init with
    | Some (_, Call { Call.callee = _, Identifier (_, { Ast.Identifier.name= "require"; comments= _ }); _ })
        when not (Env.local_scope_entry_exists "require") ->
      let loc, _ = id in
      (* Record the loc of the pattern, which contains the locations of any
         local definitions introduced by the pattern. This information is used
         by commands to automatically "follow" such definitions to the actual
         definitions in the required module. *)
      Type_inference_hooks_js.dispatch_require_pattern_hook loc
    | _ -> ()
  );

  (* Identifiers do not need to be initialized at the declaration site as long
   * as they are definitely initialized before use. Destructuring patterns must
   * be initialized, since their declaration involves some operation on the
   * right hand side, like a property access. *)
  let init_opt, init_ast = match id, init, if_uninitialized with
  | (_, Ast.Pattern.Identifier _), None, None ->
    None, None
  | _, Some expr, _ ->
    let (_, t), _ as init_ast = expression cx expr in
    let r = mk_expression_reason expr in
    Some (t, r), Some init_ast
  | (ploc, _), None, Some f ->
    let t = f ploc in
    let r = reason_of_t t in
    Some (t, r), None
  | (ploc, _), None, None ->
    let t = VoidT.at ploc |> with_trust bogus_trust in
    let r = reason_of_t t in
    Some (t, r), None
  in

  let id_reason = match id with
  | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ } ->
    let id_loc, { Ast.Identifier.name; _ } = name in
    mk_reason (RIdentifier name) id_loc
  | ploc, _ ->
    mk_reason RDestructuring ploc
  in

  let annot = type_of_pattern id in
  let annot_t, annot_ast = Anno.mk_type_annotation cx SMap.empty id_reason annot in

  let has_anno = match annot with
  | Ast.Type.Missing _ -> false
  | Ast.Type.Available _ -> true
  in

  let id_ast = match id with
  | ploc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot=_; optional } ->
    let id_loc, { Ast.Identifier.name; comments } = name in
    (* move const/let bindings from undeclared to declared *)
    declare_var cx name id_loc;
    Env.unify_declared_type cx name annot_t;
    Option.iter init_opt ~f:(fun (init_t, init_reason) ->
      let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
      init_var cx ~use_op name ~has_anno init_t id_loc
    );
    Type_inference_hooks_js.(dispatch_lval_hook cx name id_loc (Val annot_t));
    ((ploc, annot_t), Ast.Pattern.Identifier { Ast.Pattern.Identifier.
      name = ((id_loc, annot_t), { Ast.Identifier.name; comments });
      annot = annot_ast;
      optional;
    })
  | _ ->
    Option.iter init_opt ~f:(fun (init_t, init_reason) ->
      let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
      Flow.flow cx (init_t, UseT (use_op, annot_t))
    );
    let init = Destructuring.empty ?init annot_t in
    Destructuring.pattern cx ~expr:expression init id ~f:(fun ~use_op loc name default t ->
      let reason = mk_reason (RIdentifier name) loc in
      declare_var cx name loc;
      (* The bindings introduced by destructuring an annotation should themselves behave
       * like annotations. That is, subsequent writes to this binding should be compatible
       * with the relevant part of the annotation. *)
      let t =
        if has_anno then
          AnnotT (reason, Tvar.mk_where cx reason (fun t' ->
            Flow.flow cx (t, BecomeT (reason, t'))
          ), false)
        else t
      in
      let () =
        if has_anno then begin
          Env.unify_declared_type cx name t;
          Env.pseudo_init_declared_type cx name loc;
        end else
          init_var cx ~use_op name ~has_anno t loc;
      in
      Flow.flow cx (t, AssertImportIsValueT (reason, name));
      Option.iter default ~f:(fun d ->
        let default_t = Flow.mk_default cx reason d in
        Flow.flow cx (default_t, UseT (use_op, t))
      )
    )
  in

  (id_ast, init_ast)
)

and expression_or_spread cx = Ast.Expression.(function
  | Expression e ->
    let (_, t), _ as e' = expression cx e in
    Arg t, Expression e'
  | Spread (loc, { SpreadElement.argument }) ->
    let (_, t), _ as e' = expression cx argument in
    SpreadArg t, Spread (loc, { SpreadElement.argument = e' })
)

and expression_or_spread_list cx undef_loc = Ast.Expression.(
  Fn.compose List.split (Core_list.map ~f:(function
  | Some (Expression e) ->
    let (_, t), _ as e = expression cx e in
    UnresolvedArg t, Some (Expression e)
  | None ->
    UnresolvedArg (EmptyT.at undef_loc |> with_trust bogus_trust), None
  | Some (Spread (loc, { SpreadElement.argument })) ->
    let (_, t), _ as argument = expression cx argument in
    UnresolvedSpreadArg t,
    Some (Spread (loc, { SpreadElement.argument = argument }))
  ))
)

(* can raise Abnormal.(Exn (Stmt _, _)) *)
and expression ?(is_cond=false) cx (loc, e) =
  expression_ ~is_cond cx loc e

and this_ cx loc = Ast.Expression.(
  match Refinement.get cx (loc, This) loc with
  | Some t -> t
  | None -> Env.var_ref cx (internal_name "this") loc
)

and super_ cx loc =
  Env.var_ref cx (internal_name "super") loc

and expression_ ~is_cond cx loc e : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t =
  let make_trust = Context.trust_constructor cx in
  let ex = (loc, e) in Ast.Expression.(match e with

  | Ast.Expression.Literal lit ->
      (loc, literal cx loc lit), Ast.Expression.Literal lit

  (* Treat the identifier `undefined` as an annotation for error reporting
   * purposes. Like we do with other literals. Otherwise we end up pointing to
   * `void` in `core.js`. While possible to re-declare `undefined`, it is
   * unlikely. The tradeoff is worth it. *)
  | Identifier (id_loc, ({ Ast.Identifier.name= "undefined"; comments= _ } as name)) ->
      let t = mod_reason_of_t annot_reason (identifier cx name loc) in
      (loc, t), Identifier ((id_loc, t), name)

  | Identifier (id_loc, name) ->
      let t = identifier cx name loc in
      (loc, t), Identifier ((id_loc, t), name)

  | This ->
      let t = this_ cx loc in
      (loc, t), This

  | Super ->
      (loc, identifier cx (mk_ident ~comments:None "super") loc), Super

  | Unary u ->
      let t, u = unary cx loc u in
      (loc, t), Unary u

  | Update u ->
      let t, u = update cx loc u in
      (loc, t), Update u

  | Binary b ->
      let t, b = binary cx loc b in
      (loc, t), Binary b

  | Logical l ->
      let t, l = logical cx loc l in
      (loc, t), Logical l

  | TypeCast { TypeCast.expression = e; annot } ->
      let t, annot' = Anno.mk_type_available_annotation cx SMap.empty annot in
      let (_, infer_t), _ as e' = expression cx e in
      let use_op = Op (Cast {
        lower = mk_expression_reason e;
        upper = reason_of_t t;
      }) in
      Flow.flow cx (infer_t, UseT (use_op, t));
      (loc, t),
      TypeCast { TypeCast.expression = e'; annot = annot' }

  | Member _ -> subscript ~is_cond cx ex

  | OptionalMember _ -> subscript ~is_cond cx ex

  | Object { Object.properties; comments } ->
    let reason = mk_reason RObjectLit loc in
    let t, properties = object_ cx reason properties in
    (loc, t), Object { Object.properties; comments }

  | Array { Array.elements; comments } -> (
    let reason = mk_reason RArrayLit loc in
    match elements with
    | [] ->
        (* empty array, analogous to object with implicit properties *)
        let element_reason = mk_reason Reason.unknown_elem_empty_array_desc loc in
        let elemt = Tvar.mk cx element_reason in
        let reason = replace_reason_const REmptyArrayLit reason in
        (loc, DefT (reason, make_trust (), ArrT (ArrayAT (elemt, Some [])))),
        Array { Array.elements = []; comments }
    | elems ->
        let elem_spread_list, elements = expression_or_spread_list cx loc elems in
        (
          loc,
          Tvar.mk_where cx reason (fun tout ->
            let reason_op = reason in
            let element_reason =
              replace_reason_const Reason.inferred_union_elem_array_desc reason_op in
            let elem_t = Tvar.mk cx element_reason in
            let resolve_to = (ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout)) in

            Flow.resolve_spread_list cx ~use_op:unknown_use ~reason_op elem_spread_list resolve_to
          )
        ),
        Array { Array.elements; comments }
    )

  | New {
      New.callee = callee_loc, Identifier (id_loc, ({ Ast.Identifier.name= "Function"; comments= _ } as name));
      targs;
      arguments
    } -> (
      let targts_opt = Option.map targs (fun (targts_loc, args) ->
        targts_loc, convert_tparam_instantiations cx SMap.empty args
      ) in
      let argts, arges = arguments
        |> Core_list.map ~f:(expression_or_spread cx)
        |> List.split in
      let id_t = identifier cx name callee_loc in
      let callee_annot = callee_loc, id_t in
      match targts_opt with
      | None ->
        List.iter (function Arg t | SpreadArg t ->
          Flow.flow_t cx (t, StrT.at loc |> with_trust bogus_trust)
        ) argts;
        let reason = mk_reason (RCustom "new Function(..)") loc in
        let proto = ObjProtoT reason in
        (
          loc,
          DefT (reason, bogus_trust (), FunT (
            dummy_static reason,
            dummy_prototype,
            mk_functiontype reason
              [] ~rest_param:None ~def_reason:reason ~params_names:[] proto
          ))
        ),
        New {
          New.callee = callee_annot, Identifier ((id_loc, id_t), name);
          targs = None;
          arguments = arges
        }
      | Some (targts_loc, targts) ->
        Flow.add_output cx Error_message.(ECallTypeArity {
          call_loc = loc;
          is_new = true;
          reason_arity = Reason.(locationless_reason (RType "Function"));
          expected_arity = 0;
        });
        (loc, AnyT.at AnyError loc),
        New {
          New.callee = callee_annot, Identifier ((id_loc, id_t), name);
          targs = Some (targts_loc, snd targts);
          arguments = arges
        }
    )

  | New {
      New.callee = callee_loc, Identifier (id_loc, ({ Ast.Identifier.name= "Array" as n; comments= _ } as name));
      targs;
      arguments
    } -> (
      let targts = Option.map targs (fun (loc, args) ->
        loc, (convert_tparam_instantiations cx SMap.empty args)
      ) in
      let args = Core_list.map ~f:(expression_or_spread cx) arguments in
      let result = match targts, args with
      | Some (loc, ([t], [elem_t])), [Arg argt, arg] -> Ok (Some (loc, elem_t, t), argt, arg)
      | None, [Arg argt, arg] -> Ok (None, argt, arg)
      | None, _ -> Error (Error_message.EUseArrayLiteral loc)
      | Some _, _ ->
        Error Error_message.(ECallTypeArity {
          call_loc = loc;
          is_new = true;
          reason_arity = Reason.(locationless_reason (RType n));
          expected_arity = 1;
        })
      in
      match result with
      | Ok (targ_t, arg_t, arg) ->
        let reason = mk_reason (RCustom "new Array(..)") loc in
        let length_reason =
          replace_reason_const (RCustom "array length") reason in
        Flow.flow_t cx (arg_t, DefT (length_reason, bogus_trust (), NumT AnyLiteral));
        let t, targs = match targ_t with
        | Some (loc, ast, ExplicitArg t) -> t, Some (loc, [ast])
        | Some (_, _, ImplicitArg _)
        | None ->
          let element_reason =
            replace_reason_const (RCustom "array element") reason in
          Tvar.mk cx element_reason, None
        in
        let id_t = identifier cx name callee_loc in

        (* TODO - tuple_types could be undefined x N if given a literal *)
        (loc, DefT (reason, bogus_trust (), ArrT (ArrayAT (t, None)))),
        New { New.
          callee = (callee_loc, id_t), Identifier ((id_loc, id_t), name);
          targs;
          arguments = [arg];
        }
      | Error err ->
        Flow.add_output cx err;
        Tast_utils.error_mapper#expression ex
    )

  | New { New.callee; targs; arguments } ->
      let (_, class_), _ as callee_ast = expression cx callee in
      let targts, targs_ast = convert_targs cx targs in
      let argts, arguments_ast =
        arguments
        |> Core_list.map ~f:(expression_or_spread cx)
        |> List.split in
      let reason = mk_reason (RConstructorCall (desc_of_t class_)) loc in
      let use_op = Op (FunCall {
        op = mk_expression_reason ex;
        fn = mk_expression_reason callee;
        args = mk_initial_arguments_reason arguments;
        local = true;
      }) in
      (loc, new_call cx reason ~use_op class_ targts argts),
      New {New.
        callee = callee_ast;
        targs = targs_ast;
        arguments = arguments_ast;
      }

  | Call _ -> subscript ~is_cond cx ex

  | OptionalCall _ -> subscript ~is_cond cx ex

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason RConditional loc in
      let test, preds, not_preds, xtypes = predicates_of_condition cx test in
      let env =  Env.peek_env () in
      let oldset = Changeset.Global.clear () in

      let then_env = Env.clone_env env in
      Env.update_env cx loc then_env;
      let _ = Env.refine_with_preds cx loc preds xtypes in
      let ((_, t1), _ as consequent, then_abnormal)
        = Abnormal.catch_expr_control_flow_exception
            (fun () -> expression cx consequent) in

      let else_env = Env.clone_env env in
      Env.update_env cx loc else_env;
      let _ = Env.refine_with_preds cx loc not_preds xtypes in
      let ((_, t2), _ as alternate, else_abnormal)
        = Abnormal.catch_expr_control_flow_exception
            (fun () -> expression cx alternate) in

      let newset = Changeset.Global.merge oldset in

      let end_env, combined_type = match then_abnormal, else_abnormal with
      (* If one side throws (using invariant()) only refine with the other
         side.*)
      | Some Abnormal.Throw, None ->
        else_env, t2
      | None, Some Abnormal.Throw ->
        then_env, t1

      | Some Abnormal.Throw, Some Abnormal.Throw ->
        Env.merge_env cx loc (env, then_env, else_env) newset;
        env, EmptyT.at loc |> with_trust bogus_trust
        (* Both sides threw--see below for where we re-raise *)

      | None, None ->
        Env.merge_env cx loc (env, then_env, else_env)
        (Changeset.exclude_refines newset);
        env, UnionT (reason, UnionRep.make t1 t2 [])
        (* NOTE: In general it is dangerous to express the least upper bound of
           some types as a union: it might pin down the least upper bound
           prematurely (before all the types have been inferred), and when the
           union appears as an upper bound, it might lead to speculative matching.

           However, here a union is safe, because this union is guaranteed to only
           appear as a lower bound.

           In such "covariant" positions, avoiding unnecessary indirection via
           tvars is a good thing, because it improves precision. In particular, it
           enables more types to be fully resolvable, which improves results of
           speculative matching.

           It should be possible to do this more broadly and systematically. For
           example, results of operations on annotations (like property gets on
           objects, calls on functions) are often represented as unresolved tvars,
           where they could be pinned down to resolved types.
        *)

      | _ ->
        (* The only kind of abnormal control flow that should be raised from
           an expression is a Throw. The other kinds (return, break, continue)
           can only arise from statements, and while statements can appear within
           expressions (eg function expressions), any abnormals will be handled
           before they get here. *)
        assert_false "Unexpected abnormal control flow from within expression"
      in
      Env.update_env cx loc end_env;
      (* TODO call loc_of_predicate on some pred?
         t1 is wrong but hopefully close *)
      let ast =
        (loc, combined_type),
        Conditional { Conditional.
          test = test;
          consequent = consequent;
          alternate = alternate;
        } in

      (* handle control flow in cases where we've thrown from both sides *)
      begin match then_abnormal, else_abnormal with
      | Some then_exn, Some else_exn when then_exn = else_exn ->
        Abnormal.throw_expr_control_flow_exception loc ast then_exn

      | _ -> ast
      end

  | Assignment { Assignment.operator; left; right } ->
      let t, left, right = assignment cx loc (left, operator, right) in
      (loc, t), Assignment { Assignment.operator; left; right; }

  | Sequence { Sequence.expressions } ->
      let expressions = Core_list.map ~f:(expression cx) expressions in
      (* t = last element of ts. The parser guarantees sequence expressions are nonempty. *)
      let t = List.(expressions |> map snd_fst |> rev |> hd) in
      (loc, t), Sequence { Sequence.expressions }

  | Function func ->
      let {Ast.Function.id; predicate; sig_loc; _} = func in

      (match predicate with
      | Some (_, Ast.Type.Predicate.Inferred) ->
          Flow.add_output cx Error_message.(
            EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)
          )
      | _ -> ());

      let t, func = mk_function_expression id cx sig_loc func in
      (loc, t), Function func

  | ArrowFunction func ->
      let t, f = mk_arrow cx loc func in (loc, t), ArrowFunction f

  | TaggedTemplate {
      TaggedTemplate.tag;
      (* TODO: walk quasis? *)
      quasi = quasi_loc, { TemplateLiteral.quasis; expressions }
    } ->
      let expressions = Core_list.map ~f:(expression cx) expressions in
      let (_, t), _ as tag_ast = expression cx tag in
      let reason = mk_reason (RCustom "encaps tag") loc in
      let reason_array = replace_reason_const RArray reason in
      let ret = Tvar.mk cx reason in

      (* tag`a${b}c${d}` -> tag(['a', 'c'], b, d) *)
      let call_t =
        let args =
          let quasi_t = DefT (reason_array, bogus_trust (), ArrT (ArrayAT (StrT.why reason |> with_trust bogus_trust, None))) in
          let exprs_t = Core_list.map ~f:(fun ((_, t), _) -> Arg t) expressions in
          (Arg quasi_t)::exprs_t
        in
        let ft = mk_functioncalltype reason None args ret in
        let use_op = Op (FunCall {
          op = mk_expression_reason ex;
          fn = mk_expression_reason tag;
          args = [];
          local = true;
        }) in
        CallT (use_op, reason, ft)
      in
      Flow.flow cx (t, call_t);

      (loc, ret),
      TaggedTemplate { TaggedTemplate.
        tag = tag_ast;
        quasi = quasi_loc, { TemplateLiteral.quasis; expressions; };
      }

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      let t, expressions = match quasis with
      | [head] ->
          let elem_loc, { TemplateLiteral.Element.
            value = { TemplateLiteral.Element.raw; cooked; }; _
          } = head in
          let lit = { Ast.Literal.value = Ast.Literal.String cooked; raw; comments = Flow_ast_utils.mk_comments_opt () } in
          literal cx elem_loc lit, []
      | _ ->
          let t_out = StrT.at loc |> with_trust bogus_trust in
          let expressions = Core_list.map ~f:(fun expr ->
              let (_, t), _ as e = expression cx expr in
              Flow.flow cx (t, UseT (Op (Coercion {
                from = mk_expression_reason expr;
                target = reason_of_t t_out;
              }), t_out));
              e
          ) expressions in
          t_out, expressions
      in
      (loc, t), TemplateLiteral { TemplateLiteral.quasis; expressions }

  | JSXElement e ->
      let t, e = jsx cx loc e in
      (loc, t), JSXElement e

  | JSXFragment f ->
      let t, f = jsx_fragment cx loc f in
      (loc, t), JSXFragment f

  | Class c ->
      let class_loc = loc in
      let (name_loc, name) = extract_class_name class_loc c in
      let reason = mk_reason (RIdentifier name) class_loc in
      (match c.Ast.Class.id with
      | Some _ ->
          let tvar = Tvar.mk cx reason in
          let scope = Scope.fresh () in
          Scope.(
            let kind = Entry.ClassNameBinding in
            let entry = Entry.(
              new_let tvar ~loc:name_loc ~state:State.Declared ~kind
            ) in
            add_entry name entry scope
          );
          Env.push_var_scope cx scope;
          let class_t, c = mk_class cx class_loc ~name_loc reason c in
          Env.pop_var_scope ();
          Flow.flow_t cx (class_t, tvar);
          (class_loc, class_t), Class c
      | None ->
          let class_t, c = mk_class cx class_loc ~name_loc reason c in
          (class_loc, class_t), Class c
      )

  | Yield { Yield.argument; delegate = false } ->
      let yield = Env.get_internal_var cx "yield" loc in
      let t, argument_ast = match argument with
      | Some expr ->
        let (_, t), _ as expr = expression cx expr in
        t, Some expr
      | None -> VoidT.at loc |> with_trust bogus_trust, None in
      Env.havoc_heap_refinements ();
      let use_op = Op (GeneratorYield {
        value = (match argument with
        | Some expr -> mk_expression_reason expr
        | None -> reason_of_t t);
      }) in
      Flow.flow cx (t, UseT (use_op, yield));
      (loc, Env.get_internal_var cx "next" loc),
      Yield { Yield.argument = argument_ast; delegate = false }

  | Yield { Yield.argument; delegate = true } ->
      let reason = mk_reason (RCustom "yield* delegate") loc in
      let next = Env.get_internal_var cx "next" loc in
      let yield = Env.get_internal_var cx "yield" loc in
      let t, argument_ast = match argument with
      | Some expr ->
        let (_, t), _ as expr = expression cx expr in
        t, Some expr
      | None -> assert_false "delegate yield without argument" in

      let ret_reason = replace_reason (fun desc -> RCustom (
        spf "return of child generator in %s" (string_of_desc desc)
      )) reason in
      let ret = Tvar.mk cx ret_reason in

      (* widen yield with the element type of the delegated-to iterable *)
      let iterable =
        let targs = [yield; ret; next] in
        if Env.in_async_scope () then
          let reason =
            mk_reason (RCustom "async iteration expected on AsyncIterable") loc
          in
          Flow.get_builtin_typeapp cx reason "$AsyncIterable" targs
        else
          Flow.get_builtin_typeapp cx
            (mk_reason (RCustom "iteration expected on Iterable") loc)
            "$Iterable" targs
      in
      Env.havoc_heap_refinements ();
      let use_op = Op (GeneratorYield {
        value = (match argument with
        | Some expr -> mk_expression_reason expr
        | None -> reason_of_t t);
      }) in
      Flow.flow cx (t, UseT (use_op, iterable));

      (loc, ret),
      Yield { Yield.argument = argument_ast; delegate = true }

  (* TODO *)
  | Comprehension _ ->
    Flow.add_output cx
      Error_message.(EUnsupportedSyntax (loc, ComprehensionExpression));
    Tast_utils.error_mapper#expression ex

  | Generator _ ->
    Flow.add_output cx
      Error_message.(EUnsupportedSyntax (loc, GeneratorExpression));
    Tast_utils.error_mapper#expression ex

  | MetaProperty _->
    Flow.add_output cx
      Error_message.(EUnsupportedSyntax (loc, MetaPropertyExpression));
    Tast_utils.error_mapper#expression ex

  | Import arg -> (
    match arg with
    | source_loc, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.String module_name; raw; comments = _;
      }
    | source_loc, TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = module_name; raw;
          }; _
        }];
        expressions = [];
      } ->

      let comments = match arg with
      | _, Ast.Expression.Literal { Ast.Literal.comments ; _} -> comments
      | _ -> None
      in

      let imported_module_t =
        let import_reason = mk_reason (RModule module_name) loc in
        Import_export.import_ns cx import_reason (source_loc, module_name) loc
      in

      let reason = annot_reason (mk_reason (RCustom "async import") loc) in
      let t = Flow.get_builtin_typeapp cx reason "Promise" [imported_module_t] in
      (loc, t), Import ((source_loc, t), Ast.Expression.Literal { Ast.Literal.
        value = Ast.Literal.String module_name;
        raw;
        comments;
      })
    | _ ->
      let ignore_non_literals =
        Context.should_ignore_non_literal_requires cx in
      if not ignore_non_literals
      then begin
        Flow.add_output cx
          Error_message.(EUnsupportedSyntax (loc, ImportDynamicArgument));
        Tast_utils.error_mapper#expression ex
      end
      else
        Tast_utils.unchecked_mapper#expression ex
  )
)

(* Handles subscript operations. Whereas `expression` recursively computes the
   type of the LHS, `subscript` instead walks the AST to first build up a
   representation of the property chain. We can then walk that representation
   and emit constraints as we go along and omit the recursion.
*)
and subscript =
  let open Ast.Expression in

  (* As long as we encounter AST nodes for optional subscript operations (which
     require recursion on the LHS), prepend those nodes to acc and recursively
     call `build_chain` on the LHS.

     We also handle non-optional subscript operations here in order to have them
     all in one place and so that the optional nodes can leverage the
     non-optional pattern matching.
  *)
  let rec build_chain ~is_cond cx ((loc, e) as ex) acc =
    let opt_state, e' = match e with
    | OptionalCall { OptionalCall.
        call = { Call.callee; targs = _; arguments = _ } as call;
        optional;
      } ->
        warn_or_ignore_optional_chaining optional cx loc;
        begin match callee with
        | _, Member _
        | _, OptionalMember _ ->
          Flow.add_output cx Error_message.(EOptionalChainingMethods loc)
        | _ -> ()
        end;
        let opt_state = if optional then NewChain else ContinueChain in
        opt_state, Call call

    | OptionalMember { OptionalMember.member; optional } ->
        warn_or_ignore_optional_chaining optional cx loc;
        let opt_state = if optional then NewChain else ContinueChain in
        opt_state, Member member
    | _ -> NonOptional, e
    in
    let call_ast call = match opt_state with
    | NewChain -> OptionalCall { OptionalCall.call; optional = true }
    | ContinueChain -> OptionalCall { OptionalCall.call; optional = false }
    | NonOptional -> Call call
    in
    let member_ast member = match opt_state with
    | NewChain -> OptionalMember { OptionalMember.member; optional = true }
    | ContinueChain -> OptionalMember { OptionalMember.member; optional = false }
    | NonOptional -> Member member
    in

    match e' with
    | Call {
        Call.callee = callee_loc, Identifier (id_loc, ({ Ast.Identifier.name= "require" as n; comments= _ } as name));
        targs;
        arguments;
      } when not (Env.local_scope_entry_exists n) ->
      let targs = Option.map targs (fun (args_loc, args) ->
        args_loc, snd (convert_tparam_instantiations cx SMap.empty args)
      ) in
      let lhs_t, arguments = (
        match targs, arguments with
        | None, [ Expression (source_loc, Ast.Expression.Literal {
            Ast.Literal.value = Ast.Literal.String module_name; _;
          } as lit_exp) ] ->
          Import_export.require cx (source_loc, module_name) loc,
          [ Expression (expression cx lit_exp) ]
        | None, [ Expression (source_loc, TemplateLiteral {
            TemplateLiteral.quasis = [ _, {
              TemplateLiteral.Element.value = {
                TemplateLiteral.Element.cooked = module_name; _;
              }; _;
            } ];
            expressions = [];
          } as lit_exp) ] ->
          Import_export.require cx (source_loc, module_name) loc,
          [ Expression (expression cx lit_exp) ]
        | Some _, _ ->
          List.iter (fun arg -> ignore (expression_or_spread cx arg)) arguments;
          Flow.add_output cx Error_message.(ECallTypeArity {
            call_loc = loc;
            is_new = false;
            reason_arity = Reason.(locationless_reason (RFunction RNormal));
            expected_arity = 0;
          });
          AnyT.at AnyError loc, List.map Tast_utils.error_mapper#expression_or_spread arguments
        | _ ->
          List.iter (fun arg -> ignore (expression_or_spread cx arg)) arguments;
          let ignore_non_literals =
            Context.should_ignore_non_literal_requires cx in
          if not ignore_non_literals
          then
            Flow.add_output cx
              Error_message.(EUnsupportedSyntax (loc, RequireDynamicArgument));
          AnyT.at AnyError loc, List.map Tast_utils.error_mapper#expression_or_spread arguments
      ) in
      let id_t = bogus_trust () |> MixedT.at callee_loc in
      ex, lhs_t, acc, (
        (loc, lhs_t),
        call_ast { Call.
          callee = (callee_loc, id_t), Identifier ((id_loc, id_t), name);
          targs;
          arguments;
        }
      )

    | Call {
        Call.callee = callee_loc, Identifier (id_loc, ({ Ast.Identifier.name= "requireLazy" as n; comments= _ } as name));
        targs;
        arguments;
      } when not (Env.local_scope_entry_exists n) ->
      let targs = Option.map targs (fun (loc, args) ->
        loc, snd (convert_tparam_instantiations cx SMap.empty args)
      ) in
      let lhs_t, arguments = (
        match targs, arguments with
        | None, [
            Expression(_, Array({Array.elements; comments = _ }) as elems_exp);
            Expression(callback_expr);
          ] ->
          (**
           * From a static perspective (and as long as side-effects aren't
           * considered in Flow), a requireLazy call can be viewed as an immediate
           * call to require() for each of the modules, and then an immediate call
           * to the requireLazy() callback with the results of each of the prior
           * calls to require().
           *
           * TODO: requireLazy() is FB-specific. Let's find a way to either
           *       generalize or toggle this only for the FB environment.
           *)

          let element_to_module_tvar tvars = (function
            | Some(Expression(source_loc, Ast.Expression.Literal {
                Ast.Literal.value = Ast.Literal.String module_name;
                _;
              })) ->
                let module_tvar = Import_export.require cx (source_loc, module_name) loc in
                module_tvar::tvars
            | _ ->
                Flow.add_output cx Error_message.(
                  EUnsupportedSyntax (loc, RequireLazyDynamicArgument)
                );
                tvars
          ) in
          let rev_module_tvars =
            List.fold_left element_to_module_tvar [] elements in
          let module_tvars = List.rev_map (fun e -> Arg e) rev_module_tvars in

          let (_, callback_expr_t), _ as callback_ast = expression cx callback_expr in
          let reason = mk_reason (RCustom "requireLazy() callback") loc in
          let use_op = Op (FunCall {
            op = mk_expression_reason ex;
            fn = mk_expression_reason callback_expr;
            args = [];
            local = true;
          }) in
          let _ = func_call cx reason ~use_op callback_expr_t None module_tvars in
          NullT.at loc |> with_trust bogus_trust,
          [ Expression (expression cx elems_exp); Expression callback_ast ]

        | Some _, _ ->
          List.iter (fun arg -> ignore (expression_or_spread cx arg)) arguments;
          Flow.add_output cx Error_message.(ECallTypeArity {
            call_loc = loc;
            is_new = false;
            reason_arity = Reason.(locationless_reason (RFunction RNormal));
            expected_arity = 0;
          });
          AnyT.at AnyError loc,
          List.map Tast_utils.error_mapper#expression_or_spread arguments
        | _ ->
          List.iter (fun arg -> ignore (expression_or_spread cx arg)) arguments;
          Flow.add_output cx
            Error_message.(EUnsupportedSyntax (loc, RequireLazyDynamicArgument));
          AnyT.at AnyError loc,
          List.map Tast_utils.error_mapper#expression_or_spread arguments
      ) in
      let id_t = bogus_trust () |> MixedT.at callee_loc in
      ex, lhs_t, acc, (
        (loc, lhs_t),
        call_ast { Call.
          callee = (callee_loc, id_t), Identifier ((id_loc, id_t), name);
          targs;
          arguments;
        }
      )

    | Call {
        Call.callee = (callee_loc, Member {
          Member._object = (_, Identifier (_, { Ast.Identifier.name= "Object"; comments= _ }) as obj);
          property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments= _ } as id));
        } as expr);
        targs;
        arguments;
      } ->
        let (_, obj_t), _ as obj_ast = expression cx obj in
        let lhs_t, targs, arguments =
          static_method_call_Object cx loc callee_loc prop_loc expr obj_t name targs arguments
        in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          let t = bogus_trust () |> MixedT.at callee_loc in
          call_ast { Call.
            (* TODO(vijayramamurthy): what is the type of `Object.name` ? *)
            callee = (callee_loc, t), Member { Member.
              _object = obj_ast;
              property = Member.PropertyIdentifier ((prop_loc, t), id);
            };
            targs;
            arguments;
          }
        )

    | Call {
        Call.callee = (callee_loc, Member {
          Member._object = super_loc, Super;
          property = Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments= _ } as id));
        }) as callee;
        targs;
        arguments;
      } ->
        let reason = mk_reason (RMethodCall (Some name)) loc in
        let reason_lookup = mk_reason (RProperty (Some name)) callee_loc in
        let reason_prop = mk_reason (RProperty (Some name)) ploc in
        let super = super_ cx super_loc in
        let targts, targs = convert_targs cx targs in
        let argts, argument_asts = arguments
          |> Core_list.map ~f:(expression_or_spread cx)
          |> List.split in
        Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
        let prop_t = Tvar.mk cx reason_prop in
        let lhs_t = Tvar.mk_where cx reason (fun t ->
          let funtype = mk_methodcalltype super targts argts t in
          let use_op = Op (FunCallMethod {
            op = mk_expression_reason ex;
            fn = mk_expression_reason callee;
            prop = reason_prop;
            args = mk_initial_arguments_reason arguments;
            local = true;
          }) in
          Flow.flow cx (
            super,
            MethodT (use_op, reason, reason_lookup, Named (reason_prop, name),
              funtype, Some prop_t)
          )
        ) in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          call_ast { Call.
            callee = (callee_loc, prop_t), Member { Member.
              _object = (super_loc, super), Super;
              property = Member.PropertyIdentifier ((ploc, prop_t), id);
            };
            targs;
            arguments = argument_asts;
          }
        )

    | Call {
        Call.callee = (lookup_loc, Member { Member.
          _object;
          property;
        }) as callee;
        targs;
        arguments;
      } ->
        (* method call *)
        let (_, ot), _ as _object = expression cx _object in
        let targts, targs = convert_targs cx targs in
        let argts, argument_asts = arguments
          |> Core_list.map ~f:(expression_or_spread cx)
          |> List.split in
        let (prop_t, lhs_t), property = (match property with
        | Member.PropertyPrivateName (prop_loc, (name_loc, ({ Ast.Identifier.name; comments= _ } as id))) ->
          let reason_call = mk_reason (RMethodCall (Some name)) loc in
          let use_op = Op (FunCallMethod {
            op = mk_expression_reason ex;
            fn = mk_expression_reason callee;
            prop = mk_reason (RProperty (Some name)) prop_loc;
            args = mk_initial_arguments_reason arguments;
            local = true;
          }) in
          method_call cx reason_call ~use_op prop_loc (callee, ot, name) targts argts,
          Member.PropertyPrivateName (prop_loc, (name_loc, id))
        | Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments= _ } as id)) ->
          let reason_call = mk_reason (RMethodCall (Some name)) loc in
          let use_op = Op (FunCallMethod {
            op = mk_expression_reason ex;
            fn = mk_expression_reason callee;
            prop = mk_reason (RProperty (Some name)) prop_loc;
            args = mk_initial_arguments_reason arguments;
            local = true;
          }) in
          let (prop_t, _) as x =
            method_call cx reason_call ~use_op prop_loc (callee, ot, name) targts argts in
          x, Member.PropertyIdentifier ((prop_loc, prop_t), id)
        | Member.PropertyExpression expr ->
          let reason_call = mk_reason (RMethodCall None) loc in
          let reason_lookup = mk_reason (RProperty None) lookup_loc in
          let (_, elem_t), _ as expr = expression cx expr in
          (bogus_trust () |> MixedT.at lookup_loc,
          Tvar.mk_where cx reason_call (fun t ->
            let frame = Env.peek_frame () in
            let funtype = mk_methodcalltype ot targts argts t ~frame in
            Flow.flow cx (ot,
              CallElemT (reason_call, reason_lookup, elem_t, funtype))
          )),
          Member.PropertyExpression expr
        ) in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          call_ast { Call.
            callee = (lookup_loc, prop_t), Member { Member.
              _object;
              property;
            };
            targs;
            arguments = argument_asts;
          }
        )

    | Call {
        Call.callee = (super_loc, Super) as callee;
        targs;
        arguments;
      } ->
        let targts, targs = convert_targs cx targs in
        let argts, argument_asts = arguments
          |> Core_list.map ~f:(expression_or_spread cx)
          |> List.split in
        let reason = mk_reason (RFunctionCall RSuper) loc in

        (* switch back env entries for this and super from undefined *)
        define_internal cx reason "this";
        define_internal cx reason "super";

        let this = this_ cx loc in
        let super = super_ cx super_loc in
        let super_reason = reason_of_t super in
        let lhs_t = Tvar.mk_where cx reason (fun t ->
          let funtype = mk_methodcalltype this targts argts t in
          let propref = Named (super_reason, "constructor") in
          let use_op = Op (FunCall {
            op = mk_expression_reason ex;
            fn = mk_expression_reason callee;
            args = mk_initial_arguments_reason arguments;
            local = true;
          }) in
          Flow.flow cx (super, MethodT (use_op, reason, super_reason, propref, funtype, None))
        ) in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          call_ast { Call.
            callee = (super_loc, super), Super;
            targs;
            arguments = argument_asts;
          }
        )

    (******************************************)
    (* See ~/www/static_upstream/core/ *)

    | Call {
        Call.callee;
        targs;
        arguments;
      } when is_call_to_invariant callee ->
        (* TODO: require *)
        let (_, callee_t), _ as callee = expression cx callee in
        let targs = Option.map targs (fun (loc, args) ->
          loc, snd (convert_tparam_instantiations cx SMap.empty args)
        ) in
        (* NOTE: if an invariant expression throws abnormal control flow, the
            entire statement it was in is reconstructed in the typed AST as an
            expression statement containing just the invariant call. This should
            be ok for the most part since this is the most common way to call
            invariant. It's worth experimenting with whether people use invariant
            in other ways, and if not, restricting it to this pattern. *)
        let arguments = match targs, arguments with
          | None, [] ->
            (* invariant() is treated like a throw *)
            Env.reset_current_activation loc;
            Abnormal.save Abnormal.Throw;
            Abnormal.throw_expr_control_flow_exception loc
              ((loc, VoidT.at loc |> with_trust bogus_trust), (Ast.Expression.Call ( { Call.
                callee = callee;
                targs;
                arguments = [];
              })))
              Abnormal.Throw
          | None, (Expression (_, Ast.Expression.Literal {
              Ast.Literal.value = Ast.Literal.Boolean false; _
            } as lit_exp))::arguments ->
            (* invariant(false, ...) is treated like a throw *)
            let arguments =
              Core_list.map ~f:(Fn.compose snd (expression_or_spread cx)) arguments in
            Env.reset_current_activation loc;
            Abnormal.save Abnormal.Throw;
            let lit_exp = expression cx lit_exp in
            Abnormal.throw_expr_control_flow_exception loc
              ((loc, VoidT.at loc |> with_trust bogus_trust), (Ast.Expression.Call ( { Call.
                callee = callee;
                targs;
                arguments = Expression lit_exp :: arguments;
              })))
              Abnormal.Throw
          | None, (Expression cond)::arguments ->
            let arguments = Core_list.map ~f:(Fn.compose snd (expression_or_spread cx)) arguments in
            let ((_, cond_t), _ as cond), preds, _, xtypes = predicates_of_condition cx cond in
            let _ = Env.refine_with_preds cx loc preds xtypes in
            let reason = mk_reason (RFunctionCall (desc_of_t callee_t)) loc in
            Flow.flow cx (cond_t, InvariantT reason);
            Expression cond :: arguments
          | _, (Spread _)::_ ->
            ignore (Core_list.map ~f:(expression_or_spread cx) arguments);
            Flow.add_output cx
              Error_message.(EUnsupportedSyntax (loc, InvariantSpreadArgument));
            List.map Tast_utils.error_mapper#expression_or_spread arguments
          | Some _, _ ->
            ignore (Core_list.map ~f:(expression_or_spread cx) arguments);
            Flow.add_output cx Error_message.(ECallTypeArity {
              call_loc = loc;
              is_new = false;
              reason_arity = Reason.(locationless_reason (RFunction RNormal));
              expected_arity = 0;
            });
            List.map Tast_utils.error_mapper#expression_or_spread arguments
        in
        let lhs_t = VoidT.at loc |> with_trust bogus_trust in
        ex, lhs_t, acc, ((loc, lhs_t), call_ast { Call.callee; targs; arguments; })

    | Call { Call.callee; targs; arguments } ->
        begin match callee with
        | _, OptionalMember _ ->
          Flow.add_output cx Error_message.(EOptionalChainingMethods loc)
        | _ -> ()
        end;
        let targts, targs = convert_targs cx targs in
        let argts, argument_asts = arguments
          |> Core_list.map ~f:(expression_or_spread cx)
          |> List.split in
        let use_op = Op (FunCall {
          op = mk_expression_reason ex;
          fn = mk_expression_reason callee;
          args = mk_initial_arguments_reason arguments;
          local = true;
        }) in
        let exp callee = call_ast { Call.callee; targs; arguments = argument_asts } in
        begin match opt_state with
        | NonOptional ->
          let (_, f), _ as callee = expression cx callee in
          let reason = mk_reason (RFunctionCall (desc_of_t f)) loc in
          let lhs_t = func_call cx reason ~use_op f targts argts in
          ex, lhs_t, acc, ((loc, lhs_t), exp callee)
        | NewChain ->
          let (_, lhs_t), _ as calleee = expression cx callee in
          let reason = mk_reason (RFunctionCall (desc_of_t lhs_t)) loc in
          let tout = Tvar.mk cx reason in
          let opt_use = func_call_opt_use reason ~use_op targts argts in
          callee, lhs_t, ref (loc, opt_use, tout) :: acc, ((loc, tout), exp calleee)
        | ContinueChain ->
          (* Hacky reason handling *)
          let reason = mk_reason ROptionalChain loc in
          let tout = Tvar.mk cx reason in
          let opt_use = func_call_opt_use reason ~use_op targts argts in
          let step = ref (loc, opt_use, tout) in
          let lhs, lhs_t, chain, ((_, f), _ as callee) =
            build_chain ~is_cond cx callee (step :: acc) in
          let reason = replace_reason_const (RFunctionCall (desc_of_t f)) reason in
          let tout = mod_reason_of_t (Fn.const reason) tout in
          let opt_use = mod_reason_of_opt_use_t (Fn.const reason) opt_use in
          step := (loc, opt_use, tout);
          lhs, lhs_t, chain, ((loc, tout), exp callee)
        end

    | Member {
        Member._object;
        property = Member.PropertyExpression index;
      } ->
        let reason = mk_reason (RProperty None) loc in
        let (_, tind), _ as index = expression cx index in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        let opt_use = OptGetElemT (use_op, reason, tind) in
        begin match opt_state with
        | NonOptional ->
          let (_, tobj), _ as _object_ast = expression cx _object in
          let lhs_t = (match Refinement.get cx (loc, e) loc with
          | Some t -> t
          | None ->
            Tvar.mk_where cx reason (fun t ->
              let use = apply_opt_use opt_use t in
              Flow.flow cx (tobj, use)
            )
          ) in
          ex, lhs_t, acc, (
            (loc, lhs_t),
            member_ast { Member.
              _object = _object_ast;
              property = Member.PropertyExpression index;
            }
          )
        | NewChain ->
          let tout = Tvar.mk cx reason in
          let (_, lhs_t), _ as _object_ast = expression cx _object in
          _object, lhs_t, ref (loc, opt_use, tout) :: acc, (
            (loc, tout),
            member_ast { Member.
              _object = _object_ast;
              property = Member.PropertyExpression index;
            }
          )
        | ContinueChain ->
          let tout = Tvar.mk cx reason in
          let lhs, lhs_t, chain, _object_ast =
            build_chain ~is_cond cx _object (ref (loc, opt_use, tout) :: acc) in
          lhs, lhs_t, chain, (
            (loc, tout),
            member_ast { Member.
              _object = _object_ast;
              property = Member.PropertyExpression index;
            }
          )
        end

    | Member {
        Member._object = object_loc, Identifier (id_loc, (
          { Ast.Identifier.name = "module"; comments = _ } as id_name
        ));
        property = Member.PropertyIdentifier (ploc, (
          { Ast.Identifier.name = "exports"; comments = _ } as exports_name
        ));
      } ->
        let lhs_t = Import_export.get_module_exports cx loc in
        let module_reason = mk_reason (RCustom "module") object_loc in
        let module_t = MixedT.why module_reason |> with_trust bogus_trust in
        let _object =
          (object_loc, module_t),
          Ast.Expression.Identifier ((id_loc, module_t), id_name)
        in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          member_ast { Member.
            _object;
            property = Member.PropertyIdentifier ((ploc, lhs_t), exports_name);
          }
        )

    | Member {
        Member._object =
          object_loc, Identifier (id_loc, { Ast.Identifier.name= ("ReactGraphQL" | "ReactGraphQLLegacy"); comments= _ });
        property = Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name= "Mixin"; comments= _ } as name));
      } ->
        let reason = mk_reason (RCustom "ReactGraphQLMixin") loc in
        let lhs_t = Flow.get_builtin cx "ReactGraphQLMixin" reason in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          (* TODO(vijayramamurthy) what's the type of "ReactGraphQL"? *)
          let t = AnyT.at Untyped object_loc in
          let property = Member.PropertyIdentifier ((ploc, t), name) in
          member_ast { Member.
            _object = (object_loc, t), Identifier ((id_loc, t), name);
            property;
          }
        )

    | Member {
        Member._object = super_loc, Super;
        property = Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments= _ } as id));
      } ->
        let super = super_ cx super_loc in
        let expr_reason = mk_reason (RProperty (Some name)) loc in
        let lhs_t = match Refinement.get cx (loc, e) loc with
        | Some t -> t
        | None ->
          let prop_reason = mk_reason (RProperty (Some name)) ploc in
          if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
          then Unsoundness.at InferenceHooks ploc
          else Tvar.mk_where cx expr_reason (fun tvar ->
            let use_op = Op (GetProperty (mk_expression_reason ex)) in
            Flow.flow cx (
              super, GetPropT (use_op, expr_reason, Named (prop_reason, name), tvar)
            )
          )
        in
        let property = Member.PropertyIdentifier ((ploc, super), id) in
        ex, lhs_t, acc, (
          (loc, lhs_t),
          member_ast { Member.
            _object = (super_loc, super), Super;
            property;
          }
        )

    | Member {
        Member._object;
        property = Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments= _ } as id));
      } ->
        let expr_reason = mk_expression_reason ex in
        let prop_reason = mk_reason (RProperty (Some name)) ploc in
        let use_op = Op (GetProperty expr_reason) in
        begin match opt_state with
        | NonOptional ->
          let (_, tobj), _ as _object_ast = expression cx _object in
          let lhs_t = if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
          then Unsoundness.at InferenceHooks ploc
          else begin match Refinement.get cx (loc, e) loc with
          | Some t -> t
          | None ->
            get_prop ~is_cond cx expr_reason ~use_op tobj (prop_reason, name)
          end in
          let property = Member.PropertyIdentifier ((ploc, lhs_t), id) in
          ex, lhs_t, acc, (
            (loc, lhs_t),
            member_ast { Member._object = _object_ast; property; }
          )
        | NewChain ->
          let (_, lhs_t), _ as _object_ast = expression cx _object in
          let tout = if Type_inference_hooks_js.dispatch_member_hook cx name ploc lhs_t
            then Unsoundness.at InferenceHooks ploc else Tvar.mk cx expr_reason in
          let opt_use = get_prop_opt_use ~is_cond expr_reason ~use_op (prop_reason, name) in
          let property = Member.PropertyIdentifier ((ploc, tout), id) in
          _object, lhs_t, ref (loc, opt_use, tout) :: acc, (
            (loc, tout),
            member_ast { Member._object = _object_ast; property; }
          )
        | ContinueChain ->
          let tout = bogus_trust () |> MixedT.at ploc in
          let opt_use = get_prop_opt_use ~is_cond expr_reason ~use_op (prop_reason, name) in
          let step = ref (loc, opt_use, tout) in
          let lhs, lhs_t, chain, ((_, tobj), _ as _object_ast) =
            build_chain ~is_cond cx _object (step :: acc) in
          let tout = if (Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj)
            then tout
            else let tout = Tvar.mk cx expr_reason in step := (loc, opt_use, tout); tout
          in
          let property = Member.PropertyIdentifier ((ploc, tout), id) in
          lhs, lhs_t, chain, (
            (loc, tout),
            member_ast { Member._object = _object_ast; property; }
          )
        end

    | Member {
        Member._object;
        property = Member.PropertyPrivateName (ploc, (_, { Ast.Identifier.name; comments= _ })) as property;
      } ->
        let expr_reason = mk_reason (RPrivateProperty name) loc in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        begin match opt_state with
        | NonOptional ->
          let (_, tobj), _ as _object_ast = expression cx _object in
          let lhs_t = (
            match Refinement.get cx (loc, e) loc with
            | Some t -> t
            | None ->
              if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
              then Unsoundness.at InferenceHooks ploc
              else get_private_field cx expr_reason ~use_op tobj name
          ) in
          ex, lhs_t, acc, (
            (loc, lhs_t),
            member_ast { Member._object = _object_ast; property; }
          )
        | NewChain ->
          let (_, lhs_t), _ as _object_ast = expression cx _object in
          let tout = if Type_inference_hooks_js.dispatch_member_hook cx name ploc lhs_t
            then Unsoundness.at InferenceHooks ploc else Tvar.mk cx expr_reason in
          let opt_use = get_private_field_opt_use expr_reason ~use_op name in
          _object, lhs_t, ref (loc, opt_use, tout) :: acc, (
            (loc, tout),
            member_ast { Member._object = _object_ast; property; }
          )
        | ContinueChain ->
          let tout = bogus_trust () |> MixedT.at ploc in
          let opt_use = get_private_field_opt_use expr_reason ~use_op name in
          let step = ref (loc, opt_use, tout) in
          let lhs, lhs_t, chain, ((_, tobj), _ as _object_ast) =
            build_chain ~is_cond cx _object (step :: acc) in
          let tout = if (Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj)
            then tout
            else let tout = Tvar.mk cx expr_reason in step := (loc, opt_use, tout); tout
          in
          lhs, lhs_t, chain, (
            (loc, tout),
            member_ast { Member._object = _object_ast; property; }
          )
        end

    | _ ->
        let (_, lhs_t), _ as ast = expression cx ex in
        ex, lhs_t, acc, ast
    in

  fun ~is_cond cx ex ->
    let lhs, lhs_t, chain, ast = build_chain ~is_cond cx ex [] in
    begin match chain with
    | [] -> ()
    | hd :: tl ->
      let (hd_loc, _, _) = !hd in
      let chain = Nel.map (fun step ->
        let (_, use, t) = !step in
        use, t
      ) (hd, tl) in
      let reason = mk_reason ROptionalChain hd_loc in
      let lhs_reason = mk_expression_reason lhs in
      Flow.flow cx (lhs_t, OptionalChainT (reason, lhs_reason, chain));
    end;
    ast

(* Handles function calls that appear in conditional contexts. The main
   distinction from the case handled in `expression_` is that we also return
   the inferred types for the call receiver and the passed arguments, and
   potenially the keys that correspond to the supplied arguments.
*)
and predicated_call_expression cx loc call =
  let f, argks, argts, t, call =
    predicated_call_expression_ cx loc call in
  f, argks, argts, t, call

(* Returns a quadruple containing:
   - the function type
   - argument keys
   - the arguments types
   - the returned type
*)
and predicated_call_expression_ cx loc { Ast.Expression.Call.callee; targs; arguments } =
  let targts, targ_asts = convert_targs cx targs in
  let args = arguments |> Core_list.map ~f:(function
    | Ast.Expression.Expression e -> e
    | _ -> Utils_js.assert_false "No spreads should reach here"
  ) in
  let (_, f), _ as callee_ast = expression cx callee in
  let reason = mk_reason (RFunctionCall (desc_of_t f)) loc in
  let arg_asts = Core_list.map ~f:(expression cx) args in
  let argts = Core_list.map ~f:snd_fst arg_asts in
  let argks = Core_list.map ~f:Refinement.key args in
  let use_op = Op (FunCall {
    op = reason;
    fn = mk_expression_reason callee;
    args = mk_initial_arguments_reason arguments;
    local = true;
  }) in
  let t = func_call cx reason ~use_op f targts (Core_list.map ~f:(fun e -> Arg e) argts) in
  f, argks, argts, t, { Ast.Expression.Call.
    callee = callee_ast;
    targs = targ_asts;
    arguments = Core_list.map ~f:(fun e -> Ast.Expression.Expression e) arg_asts;
  }

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx reason ~use_op class_ targs args =
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (class_, ConstructorT (use_op, reason, targs, args, t));
  )

and func_call_opt_use reason ~use_op ?(call_strict_arity=true) targts argts =
  Env.havoc_heap_refinements ();
  let frame = Env.peek_frame () in
  let opt_app = mk_opt_functioncalltype reason targts argts frame call_strict_arity in
  OptCallT (use_op, reason, opt_app)

and func_call cx reason ~use_op ?(call_strict_arity=true) func_t targts argts =
  let opt_use = func_call_opt_use reason ~use_op ~call_strict_arity targts argts in
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (func_t, apply_opt_use opt_use t)
  )

(* returns (type of method itself, type returned from method) *)
and method_call cx reason ~use_op ?(call_strict_arity=true) prop_loc
    (expr, obj_t, name) targts argts =
  Type_inference_hooks_js.dispatch_call_hook cx name prop_loc obj_t;
  let expr_loc, _ = expr in
  (match Refinement.get cx expr (aloc_of_reason reason) with
  | Some f ->
      (* note: the current state of affairs is that we understand
         member expressions as having refined types, rather than
         understanding receiver objects as carrying refined properties.
         generalizing this properly is a todo, and will deliver goodness.
         meanwhile, here we must hijack the property selection normally
         performed by the flow algorithm itself. *)
      Env.havoc_heap_refinements ();
      f,
      Tvar.mk_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let app =
          mk_methodcalltype obj_t targts argts t ~frame ~call_strict_arity in
        Flow.flow cx (f, CallT (use_op, reason, app));
      )
  | None ->
      Env.havoc_heap_refinements ();
      let reason_prop = mk_reason (RProperty (Some name)) prop_loc in
      let prop_t = Tvar.mk cx reason_prop in
      prop_t,
      Tvar.mk_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let reason_expr = mk_reason (RProperty (Some name)) expr_loc in
        let app =
          mk_methodcalltype obj_t targts argts t ~frame ~call_strict_arity in
        let propref = Named (reason_prop, name) in
        Flow.flow cx (obj_t, MethodT (use_op, reason, reason_expr, propref, app, Some prop_t))
      )
  )

and identifier_ cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then Unsoundness.at InferenceHooks loc
  else (
    let t = Env.var_ref ~lookup_mode:ForValue cx name loc in
    (* We want to make sure that the reason description for the type we return
     * is always `RIdentifier name`. *)
    match desc_of_t t with
    | RIdentifier name' when name = name' -> t
    | _ ->
      (match t with
      (* If this is an `OpenT` we can change its reason description directly. *)
      | OpenT _ -> mod_reason_of_t (replace_reason_const (RIdentifier name)) t
      (* If this is not an `OpenT` then create a new type variable with our
       * desired reason and unify it with our type. This adds a level of
       * indirection so that we don't modify the underlying reason of our type. *)
      | _ ->
        let reason = mk_reason (RIdentifier name) loc in
        Tvar.mk_where cx reason (Flow.unify cx t)
      )
  )

and identifier cx { Ast.Identifier.name; comments= _ } loc =
  let t = identifier_ cx name loc in
  t

(* traverse a literal expression, return result type *)
and literal cx loc lit =
  let make_trust = Context.trust_constructor cx in
  Ast.Literal.(match lit.Ast.Literal.value with
  | String s -> begin
    match Context.haste_module_ref_prefix cx with
    | Some prefix when String_utils.string_starts_with s prefix ->
      let m = String_utils.lstrip s prefix in
      let t = Import_export.require cx (loc, m) loc in
      let reason = mk_reason (RCustom "module reference") loc in
      Flow.get_builtin_typeapp cx reason "$Flow$ModuleRef" [t]
    | _ ->
      (* It's too expensive to track literal information for large strings.*)
      let max_literal_length = Context.max_literal_length cx in
      let lit, r_desc =
        if max_literal_length = 0 || String.length s < max_literal_length
        then Literal (None, s), RString
        else AnyLiteral, RLongStringLit (max_literal_length)
      in
      DefT (annot_reason (mk_reason r_desc loc), make_trust (), StrT lit)
  end

  | Boolean b ->
      DefT (annot_reason (mk_reason RBoolean loc), make_trust (), BoolT (Some b))

  | Null ->
      NullT.at loc |> with_trust make_trust

  | Number f ->
      DefT (annot_reason (mk_reason RNumber loc), make_trust (), NumT (Literal (None, (f, lit.raw))))

  | BigInt _ ->
      let reason = annot_reason (mk_reason (RBigIntLit lit.raw) loc) in
      Flow.add_output cx (Error_message.EBigIntNotYetSupported reason);
      AnyT.why AnyError reason

  | RegExp _ ->
      Flow.get_builtin_type cx (annot_reason (mk_reason RRegExp loc)) "RegExp"
)

(* traverse a unary expression, return result type *)
and unary cx loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; comments } ->
      let (_, arg), _ as argument = expression cx argument in
      let reason = mk_reason (RUnaryOperator ("not", desc_of_t arg)) loc in
      Tvar.mk_where cx reason (fun t -> Flow.flow cx (arg, NotT (reason, t))),
      { operator = Not; argument; comments }

  | { operator = Plus; argument; comments } ->
      let argument = expression cx argument in
      NumT.at loc |> with_trust literal_trust, { operator = Plus; argument; comments }

  | { operator = Minus; argument; comments } ->
      let (_, argt), _ as argument = expression cx argument in
      begin match argt with
      | DefT (reason, trust, NumT (Literal (sense, (value, raw)))) ->
        (* special case for negative number literals, to avoid creating an unnecessary tvar. not
           having a tvar allows other special cases that match concrete lower bounds to proceed
           (notably, Object.freeze upgrades literal props to singleton types, and a tvar would
           make a negative number not look like a literal.) *)
        let reason = repos_reason loc ~annot_loc:loc reason in
        let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
        DefT (reason, trust, NumT (Literal (sense, (value, raw))))
      | arg ->
        let reason = mk_reason (desc_of_t arg) loc in
        Tvar.mk_derivable_where cx reason (fun t ->
          Flow.flow cx (arg, UnaryMinusT (reason, t));
        )
      end,
      { operator = Minus; argument; comments }

  | { operator = BitNot; argument; comments } ->
      let t = NumT.at loc |> with_trust literal_trust in
      let (_, argt), _ as argument = expression cx argument in
      Flow.flow_t cx (argt, t);
      t, { operator = BitNot; argument; comments}

  | { operator = Typeof; argument; comments } ->
      let argument = expression cx argument in
      StrT.at loc |> with_trust literal_trust, { operator = Typeof; argument; comments }

  | { operator = Void; argument; comments } ->
      let argument = expression cx argument in
      VoidT.at loc |> with_trust literal_trust, { operator = Void; argument; comments }

  | { operator = Delete; argument; comments } ->
      let argument = expression cx argument in
      BoolT.at loc |> with_trust literal_trust, { operator = Delete; argument; comments }

  | { operator = Await; argument; comments } ->
    (** TODO: await should look up Promise in the environment instead of going
        directly to the core definition. Otherwise, the following won't work
        with a polyfilled Promise! **)
    (* see declaration of $await in core.js:
       if argument is a Promise<T>, then (await argument) returns T.
       otherwise it just returns the argument type.
       TODO update this comment when recursive unwrapping of
       Promise is done.
     *)
    let reason = mk_reason (RCustom "await") loc in
    let await = Flow.get_builtin cx "$await" reason in
    let (_, arg), _ as argument_ast = expression cx argument in
    let use_op = Op (FunCall {
      op = reason;
      fn = reason_of_t await;
      args = [mk_expression_reason argument];
      local = true;
    }) in
    func_call cx reason ~use_op await None [Arg arg],
    { operator = Await; argument = argument_ast; comments }
)

(* numeric pre/post inc/dec *)
and update cx loc expr = Ast.Expression.Update.(
  let reason = mk_reason (RCustom "update") loc in
  let result_t = NumT.at loc |> with_trust literal_trust in
  result_t,
  (match expr.argument with
  | arg_loc, Ast.Expression.Identifier (id_loc, ({ Ast.Identifier.name; comments= _ } as id_name)) ->
    Flow.flow cx (identifier cx id_name id_loc, AssertArithmeticOperandT reason);
    (* enforce state-based guards for binding update, e.g., const *)
    let use_op = Op (AssignVar {
      var = Some (mk_reason (RIdentifier name) id_loc);
      init = reason_of_t result_t;
    }) in
    ignore (Env.set_var cx ~use_op name result_t id_loc);
    let t = NumT.at arg_loc |> with_trust bogus_trust in
    { expr with
        argument = (arg_loc, t), Ast.Expression.Identifier ((id_loc, t), id_name) }
  | argument ->
    let (_, arg_t), _ as arg_ast = expression cx argument in
    Flow.flow cx (arg_t, AssertArithmeticOperandT reason);
    { expr with argument = arg_ast }
  )
)

(* traverse a binary expression, return result type *)
and binary cx loc { Ast.Expression.Binary.operator; left; right } =
  let open Ast.Expression.Binary in
  match operator with
  | Equal
  | NotEqual ->
      let (_, t1), _ as left = expression cx left in
      let (_, t2), _ as right = expression cx right in
      let desc = RBinaryOperator (
        (match operator with
        | Equal -> "=="
        | NotEqual -> "!="
        | _ -> failwith "unreachable"),
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, EqT (reason, false, t2));
      BoolT.at loc |> with_trust literal_trust, { operator; left; right; }

  | In ->
      let (loc1, _) = left in
      let (loc2, _) = right in
      let (_, t1), _ as left = expression cx left in
      let (_, t2), _ as right = expression cx right in
      let reason_lhs = mk_reason (RCustom "LHS of `in` operator") loc1 in
      let reason_rhs = mk_reason (RCustom "RHS of `in` operator") loc2 in
      Flow.flow cx (t1, AssertBinaryInLHST reason_lhs);
      Flow.flow cx (t2, AssertBinaryInRHST reason_rhs);
      BoolT.at loc |> with_trust literal_trust, { operator; left; right; }

  | StrictEqual
  | StrictNotEqual
  | Instanceof ->
      let left = expression cx left in
      let right = expression cx right in
      BoolT.at loc |> with_trust literal_trust, { operator; left; right; }

  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual ->
      let (_, t1), _ as left = expression cx left in
      let (_, t2), _ as right = expression cx right in
      let desc = RBinaryOperator (
        (match operator with
        | LessThan -> "<"
        | LessThanEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanEqual -> ">="
        | _ -> failwith "unreachable"),
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, ComparatorT (reason, false, t2));
      BoolT.at loc |> with_trust literal_trust, { operator; left; right; }

  | LShift
  | RShift
  | RShift3
  | Minus
  | Mult
  | Exp
  | Div
  | Mod
  | BitOr
  | Xor
  | BitAnd ->
      let reason = mk_reason (RCustom "arithmetic operation") loc in
      let (_, t1), _ as left = expression cx left in
      let (_, t2), _ as right = expression cx right in
      Flow.flow cx (t1, AssertArithmeticOperandT reason);
      Flow.flow cx (t2, AssertArithmeticOperandT reason);
      NumT.at loc |> with_trust literal_trust, { operator; left; right; }

  | Plus ->
      let (_, t1), _ as left_ast = expression cx left in
      let (_, t2), _ as right_ast = expression cx right in
      let desc = RBinaryOperator (
        "+",
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Tvar.mk_where cx reason (fun t ->
        let use_op = Op (Addition {
          op = reason;
          left = mk_expression_reason left;
          right = mk_expression_reason right;
        }) in
        Flow.flow cx (t1, AdderT (use_op, reason, false, t2, t));
      ),
      { operator; left = left_ast; right = right_ast }

  | Pipeline ->
      warn_or_ignore_fsharp_pipeline_operator cx loc;
      let (_, arg), _ as arg_ast = expression cx left in
      let (_, f), _ as callee = expression cx right in
      let open Ast.Expression in
      let f = match callee with
      | _, Unary { Unary.operator = Unary.Await; _ } ->
        let reason = mk_reason (RCustom "await") loc in
        Flow.get_builtin cx "$await" reason
      | _ -> f
      in
      let use_op = Op (FunCall {
        op = mk_reason (RCustom "|>") loc;
        fn = mk_expression_reason right;
        args = mk_initial_arguments_reason [Expression left];
        local = true;
      }) in
      let reason = mk_reason (RFunctionCall (desc_of_t f)) loc in
      let lhs_t = func_call cx reason ~use_op f None [Arg arg] in
      lhs_t, { operator; left = arg_ast; right = callee }

and logical cx loc { Ast.Expression.Logical.operator; left; right } =
  let open Ast.Expression.Logical in
  match operator with
  | Or ->
      let () = check_default_pattern cx left right in
      let ((_, t1), _ as left), _, not_map, xtypes = predicates_of_condition cx left in
      let (_, t2), _ as right = Env.in_refined_env cx loc not_map xtypes
        (fun () -> expression cx right)
      in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (t1, OrT (reason, t2, t));
      ),
      { operator = Or; left; right; }

  | And ->
      let ((_, t1), _ as left), map, _, xtypes = predicates_of_condition cx left in
      let (_, t2), _ as right = Env.in_refined_env cx loc map xtypes
        (fun () -> expression cx right)
      in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (t1, AndT (reason, t2, t));
      ),
      { operator = And; left; right; }
  | NullishCoalesce ->
      let (_, t1), _ as left = expression cx left in
      let (_, t2), _ as right = expression cx right in
      let reason = mk_reason (RLogical ("??", desc_of_t t1, desc_of_t t2)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (t1, NullishCoalesceT (reason, t2, t));
      ),
      { operator = NullishCoalesce; left; right; }

and assignment_lhs cx patt =
  match patt with
  | pat_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
      name = (loc, name);
      optional;
      annot;
    } ->
      let t = identifier cx name loc in
      ((pat_loc, t), Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (loc, t), name;
        annot = (match annot with
        | Ast.Type.Available annot ->
          Ast.Type.Available (Tast_utils.error_mapper#type_annotation annot)
        | Ast.Type.Missing hint ->
          Ast.Type.Missing (hint, AnyT.locationless Untyped));
        optional;
      })

  | loc, Ast.Pattern.Expression ((_, Ast.Expression.Member _) as m) ->
      let (_, t), _ as m = expression cx m in
      ((loc, t), Ast.Pattern.Expression m)

  (* TODO: object, array and non-member expression patterns are invalid
     (should be a parse error but isn't yet) *)
  | lhs_loc, Ast.Pattern.Object _
  | lhs_loc, Ast.Pattern.Array _
  | lhs_loc, Ast.Pattern.Expression _ ->
      Flow.add_output cx (Error_message.EInvalidLHSInAssignment lhs_loc);
      Tast_utils.error_mapper#pattern patt

(* traverse simple assignment expressions (`lhs = rhs`) *)
and simple_assignment cx _loc lhs rhs =
  let open Ast.Expression in
  let (_, t), _ as typed_rhs = expression cx rhs in

  (* update env, add constraints arising from LHS structure,
     handle special cases, etc. *)
  let lhs = match lhs with

    (* module.exports = e *)
    | lhs_loc, Ast.Pattern.Expression (pat_loc, Member {
        Member.
        _object = object_loc, Ast.Expression.Identifier (id_loc, ({
          Ast.Identifier.name = "module"; comments= _
        } as mod_name));
        property = Member.PropertyIdentifier (ploc, ({
          Ast.Identifier.name = "exports"; comments= _
        } as name));
      }) ->
        Import_export.cjs_clobber cx lhs_loc t;
        let module_reason = mk_reason (RCustom "module") object_loc in
        let module_t = MixedT.why module_reason |> with_trust bogus_trust in
        let _object =
          (object_loc, module_t),
          Ast.Expression.Identifier ((id_loc, module_t), mod_name)
        in
        let property = Member.PropertyIdentifier ((ploc, t), name) in
        (lhs_loc, t), Ast.Pattern.Expression ((pat_loc, t), Member { Member.
          _object;
          property;
        })

    (* super.name = e *)
    | lhs_loc, Ast.Pattern.Expression (pat_loc, Member {
        Member._object = super_loc, Super;
        property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments=_ } as id));
      }) ->
        let reason =
          mk_reason (RPropertyAssignment (Some name)) lhs_loc in
        let prop_reason = mk_reason (RProperty (Some name)) prop_loc in
        let super = super_ cx lhs_loc in
        let prop_t = Tvar.mk cx prop_reason in
        let use_op = Op (SetProperty {
          lhs = reason;
          prop = mk_reason (desc_of_reason (mk_pattern_reason lhs)) prop_loc;
          value = mk_expression_reason rhs;
        }) in
        Flow.flow cx (super, SetPropT (
          use_op, reason, Named (prop_reason, name), Normal, t, Some prop_t
        ));
        let property = Member.PropertyIdentifier ((prop_loc, prop_t), id) in
        (lhs_loc, prop_t), Ast.Pattern.Expression ((pat_loc, prop_t), Member { Member.
          _object = (super_loc, super), Super;
          property;
        })

    (* _object.#name = e *)
    | lhs_loc, Ast.Pattern.Expression (pat_loc, Member {
        Member._object;
        property = Member.PropertyPrivateName (prop_loc, (_, { Ast.Identifier.name; comments= _ })) as property;
      }) ->
        let (_, o), _ as _object = expression cx _object in
        let prop_t =
        (* if we fire this hook, it means the assignment is a sham. *)
        if Type_inference_hooks_js.dispatch_member_hook cx name prop_loc o
        then Unsoundness.at InferenceHooks prop_loc
        else
          let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in

          (* flow type to object property itself *)
          let class_entries = Env.get_class_entries () in
          let prop_reason = mk_reason (RPrivateProperty name) prop_loc in
          let prop_t = Tvar.mk cx prop_reason in
          let use_op = Op (SetProperty {
            lhs = reason;
            prop = mk_reason (desc_of_reason (mk_pattern_reason lhs)) prop_loc;
            value = mk_expression_reason rhs;
          }) in
          Flow.flow cx (o, SetPrivatePropT (
            use_op, reason, name, class_entries, false, t, Some prop_t
          ));
          post_assignment_havoc ~private_:true name lhs prop_t t;
          prop_t
        in
        (lhs_loc, prop_t), Ast.Pattern.Expression ((pat_loc, prop_t), Member { Member.
          _object;
          property;
        })

    (* _object.name = e *)
    | lhs_loc, Ast.Pattern.Expression (pat_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments= _ } as id));
      }) ->
        let wr_ctx = match _object, Env.var_scope_kind () with
          | (_, This), Scope.Ctor -> ThisInCtor
          | _ -> Normal
        in
        let (_, o), _ as _object = expression cx _object in
        let prop_t =
        (* if we fire this hook, it means the assignment is a sham. *)
        if Type_inference_hooks_js.dispatch_member_hook cx name prop_loc o
        then Unsoundness.at InferenceHooks prop_loc
        else
          let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
          let prop_reason = mk_reason (RProperty (Some name)) prop_loc in

          (* flow type to object property itself *)
          let prop_t = Tvar.mk cx prop_reason in
          let use_op = Op (SetProperty {
            lhs = reason;
            prop = mk_reason (desc_of_reason (mk_pattern_reason lhs)) prop_loc;
            value = mk_expression_reason rhs;
          }) in
          Flow.flow cx (o, SetPropT (
            use_op, reason, Named (prop_reason, name), wr_ctx, t, Some prop_t
          ));
          post_assignment_havoc ~private_:false name lhs prop_t t;
          prop_t
        in
        let property = Member.PropertyIdentifier ((prop_loc, prop_t), id) in
        (lhs_loc, prop_t), Ast.Pattern.Expression ((pat_loc, prop_t), Member { Member.
          _object;
          property;
        })

    (* _object[index] = e *)
    | lhs_loc, Ast.Pattern.Expression (pat_loc, Member {
        Member._object;
        property = Member.PropertyExpression ((iloc, _) as index);
      }) ->
        let reason = mk_reason (RPropertyAssignment None) lhs_loc in
        let (_, a), _ as _object = expression cx _object in
        let (_, i), _ as index = expression cx index in
        let use_op = Op (SetProperty {
          lhs = reason;
          prop = mk_reason (desc_of_reason (mk_pattern_reason lhs)) iloc;
          value = mk_expression_reason rhs;
        }) in
        Flow.flow cx (a, SetElemT (use_op, reason, i, t, None));

        (* types involved in the assignment itself are computed
           in pre-havoc environment. it's the assignment itself
           which clears refis *)
        Env.havoc_heap_refinements ();
        (lhs_loc, t), Ast.Pattern.Expression ((pat_loc, t), Member { Member.
          _object;
          property = Member.PropertyExpression index;
        })

    (* other r structures are handled as destructuring assignments *)
    | _ ->
        Destructuring.assignment cx ~expr:expression t rhs lhs
  in
  t, lhs, typed_rhs

(* traverse assignment expressions with operators (`lhs += rhs`, `lhs *= rhs`, etc) *)
and op_assignment cx loc lhs op rhs =
  let open Ast.Expression in
  match op with
  | Assignment.PlusAssign ->
      (* lhs += rhs *)
      let reason = mk_reason (RCustom "+=") loc in
      let (_, lhs_t), _ as lhs_ast = assignment_lhs cx lhs in
      let (_, rhs_t), _ as rhs_ast = expression cx rhs in
      let result_t = Tvar.mk cx reason in

      (* lhs = lhs + rhs *)
      let () =
        let use_op = Op (Addition {
          op = reason;
          left = mk_pattern_reason lhs;
          right = mk_expression_reason rhs;
        }) in
        Flow.flow cx (lhs_t, AdderT (use_op, reason, false, rhs_t, result_t))
      in
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = id_loc, { Ast.Identifier.name; comments= _ };
        _;
      } ->
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) id_loc);
          init = reason;
        }) in
        ignore Env.(set_var cx ~use_op name result_t id_loc)
      | _ -> ()
      );
      lhs_t, lhs_ast, rhs_ast

  | Assignment.MinusAssign
  | Assignment.MultAssign
  | Assignment.ExpAssign
  | Assignment.DivAssign
  | Assignment.ModAssign
  | Assignment.LShiftAssign
  | Assignment.RShiftAssign
  | Assignment.RShift3Assign
  | Assignment.BitOrAssign
  | Assignment.BitXorAssign
  | Assignment.BitAndAssign
    ->
      (* lhs (numop)= rhs *)
      let reason = mk_reason (RCustom "(numop)=") loc in
      let (_, lhs_t), _ as lhs_ast = assignment_lhs cx lhs in
      let (_, rhs_t), _ as rhs_ast = expression cx rhs in
      (* lhs = lhs (numop) rhs *)
      Flow.flow cx (lhs_t, AssertArithmeticOperandT reason);
      Flow.flow cx (rhs_t, AssertArithmeticOperandT reason);
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = id_loc, { Ast.Identifier.name; comments= _ };
        _;
      } ->
        let t = NumT.at loc |> with_trust literal_trust in
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) id_loc);
          init = reason_of_t t;
        }) in
        ignore Env.(set_var cx ~use_op name t id_loc)
      | _ -> ()
      );
      lhs_t, lhs_ast, rhs_ast

(* traverse assignment expressions *)
and assignment cx loc (lhs, op, rhs) =
  match op with
  | None -> simple_assignment cx loc lhs rhs
  | Some op -> op_assignment cx loc lhs op rhs

and clone_object cx reason this that use_op =
  Tvar.mk_where cx reason (fun tvar ->
    let u = ObjRestT (reason, [], tvar) in
    let t = Flow.tvar_with_constraint cx u in
    Flow.flow cx (
      this,
      ObjAssignToT (use_op, reason, that, t, default_obj_assign_kind)
    )
  )

and collapse_children cx (children_loc, children):
    Type.unresolved_param list *
    (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list) =
  let open Ast.JSX in
  let unresolved_params, children' =
    children
    |> List.fold_left (fun (unres_params, children) -> function
      | ExpressionContainer.(
          loc,
          ExpressionContainer { expression = EmptyExpression empty_loc }
        ) ->
        unres_params, (loc,
          ExpressionContainer.(ExpressionContainer {
            expression = EmptyExpression empty_loc
          })
        )::children
      | child ->
        let unres_param_opt, child = jsx_body cx child in
        Option.value_map unres_param_opt
          ~default:unres_params ~f:(fun x -> x::unres_params),
        child::children
    ) ([], [])
    |> map_pair List.rev List.rev
  in
  (unresolved_params, (children_loc, children'))

and jsx cx expr_loc e: Type.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.element = Ast.JSX.(
  let { openingElement; children; closingElement } = e in
  let (children_loc, _) = children in
  let locs =
    let open_, _ = openingElement in
    match closingElement with
    | Some _ ->
      expr_loc,
      open_,
      children_loc
    | _ -> open_, open_, open_
  in
  let unresolved_params, children = collapse_children cx children in
  let t, openingElement, closingElement =
    jsx_title cx openingElement closingElement unresolved_params locs in
  t, { openingElement; children; closingElement }
)

and jsx_fragment cx expr_loc fragment: Type.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.fragment =
  let open Ast.JSX in
  let { frag_openingElement; frag_children; frag_closingElement } = fragment in
  let (children_loc, _) = frag_children in
  let locs =
    let open_ = frag_openingElement in
    expr_loc,
    open_,
    children_loc
  in
  let _, loc_opening, _ = locs in
  let unresolved_params, frag_children = collapse_children cx frag_children in
  let fragment_t =
    let reason = mk_reason (RIdentifier "React.Fragment") loc_opening in
    let react = Env.var_ref ~lookup_mode:ForValue cx "React" loc_opening in
    let use_op = Op (GetProperty reason) in
    get_prop ~is_cond:false cx reason ~use_op react (reason, "Fragment")
  in
  let t = jsx_desugar cx "React.Fragment" fragment_t (NullT.at loc_opening |> with_trust bogus_trust) []
    unresolved_params locs in
  t, { frag_openingElement; frag_children; frag_closingElement }

and jsx_title cx openingElement closingElement children locs = Ast.JSX.(
  let make_trust = Context.trust_constructor cx in
  let loc_element, _, _ = locs in
  let loc, { Opening.name; attributes; selfClosing } = openingElement in
  let facebook_fbs = Context.facebook_fbs cx in
  let facebook_fbt = Context.facebook_fbt cx in
  let jsx_mode = Context.jsx cx in

  let t, name, attributes = match (name, jsx_mode, (facebook_fbs, facebook_fbt)) with
  | Identifier (loc_id, ({ Identifier.name = "fbs"} as id)), _, (Some custom_jsx_type, _)
  | Identifier (loc_id, ({ Identifier.name = "fbt"} as id)), _, (_, Some custom_jsx_type) ->
    let fbt_reason = mk_reason RFbt loc_element in
    let t = Flow.get_builtin_type cx fbt_reason custom_jsx_type in
    let name = Identifier ((loc_id, t), id) in
    let attributes = List.map Tast_utils.error_mapper#jsx_opening_attribute attributes in
    t, name, attributes

  | Identifier (loc, { Identifier.name }), Options.Jsx_react, _ ->
    if Type_inference_hooks_js.dispatch_id_hook cx name loc then
      let t = Unsoundness.at InferenceHooks loc_element in
      let name = Identifier ((loc, t), { Identifier.name }) in
      let attributes = List.map Tast_utils.error_mapper#jsx_opening_attribute attributes in
      t, name, attributes
    else
      let reason = mk_reason (RReactElement (Some name)) loc_element in
      let c =
        if name = String.capitalize_ascii name then
          identifier cx (mk_ident ~comments:None name) loc
        else
          DefT (mk_reason (RIdentifier name) loc, make_trust (), SingletonStrT name)
      in
      let o, attributes' = jsx_mk_props cx reason c name attributes children in
      let t = jsx_desugar cx name c o attributes children locs in
      let name = Identifier ((loc, t), { Identifier.name }) in
      t, name, attributes'

  | Identifier (loc, { Identifier.name }), Options.Jsx_pragma _, _ ->
    if Type_inference_hooks_js.dispatch_id_hook cx name loc then
      let t = Unsoundness.at InferenceHooks loc_element in
      let name = Identifier ((loc, t), { Identifier.name }) in
      let attributes = List.map Tast_utils.error_mapper#jsx_opening_attribute attributes in
      t, name, attributes
    else
      let reason = mk_reason (RJSXElement (Some name)) loc_element in
      let c =
        if name = String.capitalize_ascii name then
          identifier cx (mk_ident ~comments:None name) loc
        else
          DefT (mk_reason (RIdentifier name) loc, make_trust (), StrT (Literal (None, name)))
      in
      let o, attributes' = jsx_mk_props cx reason c name attributes children in
      let t = jsx_desugar cx name c o attributes children locs in
      let name = Identifier ((loc, t), { Identifier.name }) in
      t, name, attributes'

  | Identifier (loc, { Identifier.name }), Options.Jsx_csx, _ ->
    (**
     * It's a bummer to duplicate this case, but CSX does not want the
     * "if name = String.capitalize name" restriction.
     *)
    if Type_inference_hooks_js.dispatch_id_hook cx name loc
    then
      let t = Unsoundness.at InferenceHooks loc_element in
      let name = Identifier ((loc, t), { Identifier.name }) in
      let attributes' = List.map Tast_utils.error_mapper#jsx_opening_attribute attributes in
      t, name, attributes'
    else
      let reason = mk_reason (RJSXElement (Some name)) loc_element in
      let c = identifier cx (mk_ident ~comments:None name) loc in
      let o, attributes' = jsx_mk_props cx reason c name attributes children in
      let t = jsx_desugar cx name c o attributes children locs in
      let name = Identifier ((loc, t), { Identifier.name }) in
      t, name, attributes'

  | MemberExpression member, Options.Jsx_react, _ ->
    let name = jsx_title_member_to_string member in
    let el = RReactElement (Some name) in
    let reason = mk_reason el loc_element in
    let m_expr = jsx_title_member_to_expression member in
    let (m_loc, t), m_expr' = expression cx m_expr in
    let c = mod_reason_of_t (replace_reason_const (RIdentifier name)) t in
    let o, attributes' = jsx_mk_props cx reason c name attributes children in
    let t = jsx_desugar cx name c o attributes children locs in
    let member' = match expression_to_jsx_title_member m_loc m_expr' with
    | Some member -> member
    | None -> Tast_utils.error_mapper#jsx_member_expression member
    in
    (t, MemberExpression member', attributes')

  | _ ->
      (* TODO? covers namespaced names as element names *)
    let t = Unsoundness.at InferenceHooks loc_element in
    let name = Tast_utils.error_mapper#jsx_name name in
    let attributes = List.map Tast_utils.error_mapper#jsx_opening_attribute attributes in
    t, name, attributes
  in

  let closingElement =
    match closingElement with
    | Some (c_loc, { Closing.name = cname }) ->
      Some (c_loc, { Closing.name = jsx_match_closing_element name cname })
    | None -> None
  in
  t, (loc, { Opening.name; selfClosing; attributes; }), closingElement
)

and jsx_match_closing_element =
  let match_identifiers o_id c_id =
    let (_, t), _ = o_id in
    let loc, name = c_id in
    (loc, t), name
  in
  let rec match_member_expressions o_mexp c_mexp =
    let open Ast.JSX.MemberExpression in
    let _, { _object = o_obj; property = o_prop; } = o_mexp in
    let loc, { _object = c_obj; property = c_prop; } = c_mexp in
    let _object = match_objects o_obj c_obj in
    let property = match_identifiers o_prop c_prop in
    loc, { _object; property; }

  and match_objects o_obj c_obj =
    match o_obj, c_obj with
    | Ast.JSX.MemberExpression.Identifier o_id,
      Ast.JSX.MemberExpression.Identifier c_id ->
      Ast.JSX.MemberExpression.Identifier (match_identifiers o_id c_id)
    | Ast.JSX.MemberExpression.MemberExpression o_exp,
      Ast.JSX.MemberExpression.MemberExpression c_exp ->
      Ast.JSX.MemberExpression.MemberExpression (match_member_expressions o_exp c_exp)
    | _, _ -> Tast_utils.error_mapper#jsx_member_expression_object c_obj
  in
  let match_namespaced_names o_id c_id =
    let _, { Ast.JSX.NamespacedName.namespace = o_ns; name = o_name } = o_id in
    let loc, { Ast.JSX.NamespacedName.namespace = c_ns; name = c_name } = c_id in
    let namespace = match_identifiers o_ns c_ns in
    let name = match_identifiers o_name c_name in
    loc, { Ast.JSX.NamespacedName.namespace; name; }
  in
  (* Transfer open types to close types *)
  fun o_name c_name -> Ast.JSX.(
    match o_name, c_name with
    | Identifier o_id, Identifier c_id ->
        Identifier (match_identifiers o_id c_id)
    | NamespacedName o_nname, NamespacedName c_nname ->
        NamespacedName (match_namespaced_names o_nname c_nname)
    | MemberExpression o_mexp, MemberExpression c_mexp ->
        MemberExpression (match_member_expressions o_mexp c_mexp)
    | _, _ ->
        Tast_utils.error_mapper#jsx_name c_name
  )

and jsx_mk_props cx reason c name attributes children = Ast.JSX.(
  let is_react = Context.jsx cx = Options.Jsx_react in
  let reason_props = replace_reason_const
    (if is_react then RReactProps else RJSXElementProps name)
    reason in
  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let proto = (ObjProtoT reason_props) in
  (* Return an object with specified sealing. *)
  let mk_object ?(sealed=false) props =
    Obj_type.mk_with_proto cx reason_props ~sealed ~props proto
  in
  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj ~assert_exact  =
    let use_op = Op (ObjectSpread {op = (reason_of_t from_obj)}) in
    Tvar.mk_where cx reason_props (fun t ->
      Flow.flow cx (to_obj,
        ObjAssignToT (use_op, reason_props, from_obj, t, ObjAssign { assert_exact }));
    )
  in
  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_props ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object map) result ~assert_exact:false
        else result
      in
      if not sealed then result else
        Tvar.mk_where cx reason_props (fun t ->
          Flow.flow cx (result, ObjSealT (reason_props, t))
        )
  in

  let sealed, map, result, atts = List.fold_left (fun (sealed, map, result, atts) att ->
    match att with
    (* All attributes with a non-namespaced name that are not a react ignored
     * attribute. *)
    | Opening.Attribute (attr_loc, { Attribute.
        name = Attribute.Identifier (id_loc, { Identifier.name = aname });
        value
      }) ->
      (* Get the type for the attribute's value. *)
      let atype, value =
        if Type_inference_hooks_js.dispatch_jsx_hook cx aname attr_loc c
        then Unsoundness.at InferenceHooks attr_loc, None
        else
          match value with
            (* <element name="literal" /> *)
            | Some (Attribute.Literal (loc, lit)) ->
                let t = literal cx loc lit in
                t, Some (Attribute.Literal ((loc, t), lit))
            (* <element name={expression} /> *)
            | Some (Attribute.ExpressionContainer (ec_loc, {
                ExpressionContainer.expression =
                  ExpressionContainer.Expression (loc, e)
              })) ->
                let (_, t), _ as e = expression cx (loc, e) in
                t, Some (Attribute.ExpressionContainer ((ec_loc, t), {
                    ExpressionContainer.expression =
                      ExpressionContainer.Expression e
                  }))
            (* <element name={} /> *)
            | Some (Attribute.ExpressionContainer _ as ec) ->
                let t = EmptyT.at attr_loc |> with_trust bogus_trust in
                t, Some (Tast_utils.unchecked_mapper#jsx_attribute_value ec)
            (* <element name /> *)
            | None ->
                DefT (mk_reason RBoolean attr_loc, bogus_trust (), BoolT (Some true)), None
      in
      let p = Field (Some id_loc, atype, Polarity.Neutral) in
      let att = Opening.Attribute (attr_loc, { Attribute.
          name = Attribute.Identifier ((id_loc, atype), { Identifier.name = aname });
          value
        }) in
      (sealed, SMap.add aname p map, result, att::atts)
    (* Do nothing for namespaced attributes or ignored React attributes. *)
    | Opening.Attribute _ ->
        (* TODO: attributes with namespaced names *)
        (sealed, map, result, atts)
    (* <element {...spread} /> *)
    | Opening.SpreadAttribute (spread_loc, { SpreadAttribute.argument }) ->
        let (_, spread), _ as argument = expression cx argument in
        let obj = eval_props (map, result) in
        let result = mk_spread spread obj
                       ~assert_exact:(not (SMap.is_empty map && result = None)) in
        let att = Opening.SpreadAttribute (spread_loc, { SpreadAttribute.argument }) in
        sealed, SMap.empty, Some result, att::atts
  ) (true, SMap.empty, None, []) attributes in
  let attributes = List.rev atts in
  let map =
    match children with
    | [] -> map
    (* We add children to the React.createElement() call for React. Not to the
     * props as other JSX users may support. *)
    | _ when is_react -> map
    | _ ->
        let arr = Tvar.mk_where cx reason (fun tout ->
          let reason_op = reason in
          let element_reason =
            replace_reason_const Reason.inferred_union_elem_array_desc reason_op in
          let elem_t = Tvar.mk cx element_reason in
          Flow.resolve_spread_list
            cx
            ~use_op:unknown_use
            ~reason_op:reason
            children
            (ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout))
        ) in
        let p = Field (None, arr, Polarity.Neutral) in
        SMap.add "children" p map
  in
  let t = eval_props ~sealed (map, result) in
  t, attributes
)

and jsx_desugar cx name component_t props attributes children locs =
  let loc_element, loc_opening, loc_children = locs in
  match Context.jsx cx with
  | Options.Jsx_react ->
      let reason = mk_reason (RReactElement (Some name)) loc_element in
      let react = Env.var_ref ~lookup_mode:ForValue cx "React" loc_opening in
      let children = Core_list.map ~f:(function
        | UnresolvedArg a -> a
        | UnresolvedSpreadArg a ->
            Flow.add_output cx Error_message.(EUnsupportedSyntax (loc_children, SpreadArgument));
            reason_of_t a |> AnyT.error
      ) children in
      Tvar.mk_where cx reason (fun tvar ->
        let reason_createElement =
          mk_reason (RProperty (Some "createElement")) loc_element in
        let use_op = Op (ReactCreateElementCall {
          op = reason_createElement;
          component = reason_of_t component_t;
          children = loc_children;
        }) in
        Flow.flow cx (react, MethodT (
          use_op,
          reason,
          reason_createElement,
          Named (reason_createElement, "createElement"),
          mk_methodcalltype
            react
            None
            ([Arg component_t; Arg props] @ Core_list.map ~f:(fun c -> Arg c) children)
            tvar,
          None
        ))
      )
  | Options.Jsx_pragma (raw_jsx_expr, jsx_expr) ->
      let reason = mk_reason (RJSXFunctionCall raw_jsx_expr) loc_element in

      (* A JSX element with no attributes should pass in null as the second
       * arg *)
      let props = match attributes with
      | [] -> NullT.at loc_opening |> with_trust bogus_trust
      | _ -> props in
      let argts =
        [Arg component_t; Arg props] @
        (Core_list.map ~f:(function
          | UnresolvedArg c -> Arg c
          | UnresolvedSpreadArg c -> SpreadArg c
        ) children) in
      let use_op = Op (JSXCreateElement {
        op = reason;
        component = reason_of_t component_t;
      }) in
      Ast.Expression.(match jsx_expr with
      | _, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, { Ast.Identifier.name; comments= _ });
          _;
        } ->
          let ot = jsx_pragma_expression cx raw_jsx_expr loc_element _object in
          snd (method_call cx reason ~use_op ~call_strict_arity:false prop_loc
            (jsx_expr, ot, name) None argts)
      | _ ->
          let f = jsx_pragma_expression cx raw_jsx_expr loc_element jsx_expr in
          func_call cx reason ~use_op ~call_strict_arity:false f None argts
      )
  | Options.Jsx_csx ->
      let reason = mk_reason (RJSXFunctionCall name) loc_element in
      let use_op = Op (JSXCreateElement {
        op = reason;
        component = reason_of_t component_t;
      }) in
      func_call cx reason ~use_op ~call_strict_arity:false component_t None [Arg props]

(* The @jsx pragma specifies a left hand side expression EXPR such that
 *
 * <Foo />
 *
 * is transformed into
 *
 * EXPR(Foo, props, child1, child2, etc)
 *
 * This means we need to process EXPR. However, EXPR is not inline in the code,
 * it's up in a comment at the top of the file. This means if we run into an
 * error, we're going to point at the comment at the top.
 *
 * We can cover almost all the cases by just explicitly handling identifiers,
 * since the common error is that the identifier is not in scope.
 *)
and jsx_pragma_expression cx raw_jsx_expr loc = Ast.Expression.(
  function
  | _, Identifier (_, { Ast.Identifier.name; comments= _ }) ->
      let desc = RJSXIdentifier (raw_jsx_expr, name) in
      Env.var_ref ~lookup_mode:ForValue cx name loc ~desc
  | expr ->
      (* Oh well, we tried *)
      let (_, t), _ = expression cx expr in
      t
)

and jsx_body cx (loc, child) = Ast.JSX.(
  let make_trust = Context.trust_constructor cx in
  match child with
  | Element e ->
    let t, e = jsx cx loc e in
    Some (UnresolvedArg t), (loc, Element e)
  | Fragment f ->
    let t, f = jsx_fragment cx loc f in
    Some (UnresolvedArg t), (loc, Fragment f)
  | ExpressionContainer ec ->
    let open ExpressionContainer in
    let { expression = ex } = ec in
    let unresolved_param, ex =
      match ex with
      | Expression e ->
        let (_, t), _ as e = expression cx e in
        UnresolvedArg t, Expression e
      | EmptyExpression loc ->
        let reason = mk_reason (RCustom "empty jsx body") loc in
        let t = DefT (reason, make_trust (), EmptyT Bottom) in
        UnresolvedArg t, EmptyExpression loc
    in
    Some unresolved_param, (loc, ExpressionContainer { expression = ex })
  | SpreadChild expr ->
    let (_, t), _ as e = expression cx expr in
    Some (UnresolvedSpreadArg t), (loc, SpreadChild e)
  | Text { Text.value; raw; } ->
    let unresolved_param_opt =
      match jsx_trim_text make_trust loc value with
      | Some c -> Some (UnresolvedArg c)
      | None -> None
    in
    unresolved_param_opt, (loc, Text { Text.value; raw; })
)

and jsx_trim_text make_trust loc value =
  match (Utils_jsx.trim_jsx_text (ALoc.to_loc_exn loc) value) with
  | Some (loc, trimmed) ->
    Some (DefT (mk_reason RJSXText (loc |> ALoc.of_loc), make_trust (), StrT (Type.Literal (None, trimmed))))
  | None -> None

and jsx_title_member_to_string (_, member) = Ast.JSX.MemberExpression.(
  let (_, { Ast.JSX.Identifier.name }) = member.property in
  match member._object with
  | MemberExpression member -> (jsx_title_member_to_string member) ^ "." ^ name
  | Identifier (_, { Ast.JSX.Identifier.name = obj }) -> obj ^ "." ^ name
)

and jsx_title_member_to_expression member =
  let (mloc, member) = member in
  let _object = Ast.JSX.MemberExpression.(
    match member._object with
    | MemberExpression member -> jsx_title_member_to_expression member
    | Identifier (loc, { Ast.JSX.Identifier.name }) ->
      (loc, Ast.Expression.Identifier (loc, mk_ident ~comments:None name))
  ) in
  let property = Ast.JSX.MemberExpression.(
    let (loc, { Ast.JSX.Identifier.name }) = member.property in
    (loc, mk_ident ~comments:None name)
  ) in
  Ast.Expression.Member.(
    (mloc, Ast.Expression.Member {
      _object;
      property = PropertyIdentifier property;
    })
  )

(* reverses jsx_title_member_to_expression *)
and expression_to_jsx_title_member = fun loc member ->
  match member with
  | Ast.Expression.Member.(
      Ast.Expression.Member {
        _object = (mloc, _), obj_expr;
        property = PropertyIdentifier (pannot, { Ast.Identifier.name; comments= _ });
      }) ->
    let _object = match obj_expr with
    | Ast.Expression.Identifier ((id_loc, t), { Ast.Identifier.name; comments= _ }) ->
      Some (
        Ast.JSX.MemberExpression.Identifier (
          (id_loc, t), { Ast.JSX.Identifier.name }
        )
      )
    | _ ->
      expression_to_jsx_title_member mloc obj_expr
      |> Option.map ~f:(fun e -> Ast.JSX.MemberExpression.MemberExpression e)
    in
    let property = pannot, { Ast.JSX.Identifier.name = name } in
    Option.map _object ~f:(fun _object ->
      (loc, Ast.JSX.MemberExpression.{ _object; property; }))
  | _ ->
    None

(* Given an expression found in a test position, notices certain
   type refinements which follow from the test's success or failure,
   and returns a 5-tuple:
   - result type of the test (not always bool)
   - map (lookup key -> type) of refinements which hold if
   the test is true
   - map of refinements which hold if the test is false
   - map of unrefined types for lvalues found in refinement maps
   - typed AST of the test expression
 *)
and predicates_of_condition cx e = Ast.(Expression.(
  (* refinement key if expr is eligible, along with unrefined type *)
  let refinable_lvalue e =
    Refinement.key e, condition cx e
  in

  (* package empty result (no refinements derived) from test type *)
  let empty_result test_tast =
    (test_tast, Key_map.empty, Key_map.empty, Key_map.empty)
  in

  let add_predicate key unrefined_t pred sense (test_tast, ps, notps, tmap) =
    let p, notp = if sense
      then pred, NotP pred
      else NotP pred, pred
    in
    (test_tast,
      Key_map.add key p ps,
      Key_map.add key notp notps,
      Key_map.add key unrefined_t tmap)
  in

  let flow_eqt ~strict loc (t1, t2) =
    if not strict then
      let reason = mk_reason (RCustom "non-strict equality comparison") loc in
      Flow.flow cx (t1, EqT (reason, false, t2))
  in

  (* package result quad from test typed ast, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_tast key unrefined_t pred sense =
    empty_result test_tast |> add_predicate key unrefined_t pred sense
  in

  (* a wrapper around `condition` (which is a wrapper around `expression`) that
     evaluates `expr`. if this is a sentinel property check (determined by
     a strict equality check against a member expression `_object.prop_name`),
     then also returns the refinement of `_object`.

     this is used by other tests such as `bool_test` such that if given
     `foo.bar === false`, `foo.bar` is refined to be `false` (by `bool_test`)
     and `foo` is refined to eliminate branches that don't have a `false` bar
     property (by this function). *)
  let condition_of_maybe_sentinel cx ~sense ~strict expr val_t =
    match strict, expr with
    | true,
      (expr_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name= prop_name; comments= _ } as id));
      }) ->
      (* use `expression` instead of `condition` because `_object` is the object
         in a member expression; if it itself is a member expression, it must
         exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here as
         `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
      let (_, obj_t), _ as _object_ast = expression cx _object in

      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let expr_reason = mk_expression_reason expr in
      let prop_t = match Refinement.get cx expr expr_loc with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then Unsoundness.at InferenceHooks prop_loc
        else
          let use_op = Op (GetProperty prop_reason) in
          get_prop ~is_cond:true cx
            expr_reason ~use_op obj_t (prop_reason, prop_name)
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      let refinement = match Refinement.key _object with
      | None -> None
      | Some name ->
          let pred = LeftP (SentinelProp prop_name, val_t) in
          Some (name, obj_t, pred, sense)
      in

      (* since we never called `expression cx expr`, we have to add to the
         type table ourselves *)
      let property = Member.PropertyIdentifier ((prop_loc, prop_t), id) in

      ( (expr_loc, prop_t),
        Member { Member.
          _object = _object_ast;
          property;
        }
      ), refinement
    | _ ->
      condition cx expr, None
  in

  (* inspect a null equality test *)
  let null_test loc ~sense ~strict e null_t reconstruct_ast =
    let ((_, t), _ as e_ast), sentinel_refinement =
      condition_of_maybe_sentinel cx ~sense ~strict e null_t in
    let ast = reconstruct_ast e_ast in
    flow_eqt ~strict loc (t, null_t);
    let out = match Refinement.key e with
    | None ->
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      empty_result ((loc, t_out), ast)
    | Some name ->
      let pred = if strict then NullP else MaybeP in
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      result ((loc, t_out), ast) name t pred sense
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  let void_test loc ~sense ~strict e void_t reconstruct_ast =
    (* if `void_t` is not a VoidT, make it one so that the sentinel test has a
       literal type to test against. It's not appropriate to call `void_test`
       with a `void_t` that you don't want to treat like an actual `void`! *)
    let void_t = match void_t with
    | DefT (_, _, VoidT) -> void_t
    | _ -> VoidT.why (reason_of_t void_t) |> with_trust bogus_trust
    in
    let ((_, t), _ as e_ast), sentinel_refinement =
      condition_of_maybe_sentinel cx ~sense ~strict e void_t in
    let ast = reconstruct_ast e_ast in
    flow_eqt ~strict loc (t, void_t);
    let out = match Refinement.key e with
    | None ->
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      empty_result ((loc, t_out), ast)
    | Some name ->
      let pred = if strict then VoidP else MaybeP in
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      result ((loc, t_out), ast) name t pred sense
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  (* inspect an undefined equality test *)
  let undef_test loc ~sense ~strict e void_t reconstruct_ast =
    (* if `undefined` isn't redefined in scope, then we assume it is `void` *)
    if Env.is_global_var cx "undefined"
    then void_test loc ~sense ~strict e void_t reconstruct_ast
    else
      let e_ast = expression cx e in
      empty_result ((loc, BoolT.at loc |> with_trust bogus_trust), reconstruct_ast e_ast)
  in

  let literal_test loc ~strict ~sense expr val_t pred reconstruct_ast =
    let ((_, t), _ as expr_ast), sentinel_refinement =
      condition_of_maybe_sentinel cx ~sense ~strict expr val_t in
    let ast = reconstruct_ast expr_ast in
    flow_eqt ~strict loc (t, val_t);
    let refinement = if strict then Refinement.key expr else None in
    let out = match refinement with
    | Some name ->
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      result ((loc, t_out), ast) name t pred sense
    | None ->
      let t = BoolT.at loc |> with_trust bogus_trust in
      empty_result ((loc, t), ast)
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  (* inspect a typeof equality test *)
  let typeof_test loc sense arg typename str_loc reconstruct_ast =
    let bool = BoolT.at loc |> with_trust bogus_trust in
    match refinable_lvalue arg with
    | Some name, ((_, t), _ as arg) ->
        let pred = match typename with
        | "boolean" -> Some BoolP
        | "function" -> Some FunP
        | "number" -> Some NumP
        | "object" -> Some ObjP
        | "string" -> Some StrP
        | "symbol" -> Some SymbolP
        | "undefined" -> Some VoidP
        | _ -> None
        in
        begin match pred with
        | Some pred -> result ((loc, bool), reconstruct_ast arg) name t pred sense
        | None ->
          Flow.add_output cx Error_message.(EInvalidTypeof (str_loc, typename));
          empty_result ((loc, bool), reconstruct_ast arg)
        end
    | None, arg -> empty_result ((loc, bool), reconstruct_ast arg)
  in

  let sentinel_prop_test loc ~sense ~strict expr val_t reconstruct_ast =
    let ((_, t), _ as expr_ast), sentinel_refinement =
      condition_of_maybe_sentinel cx ~sense ~strict expr val_t in
    let ast = reconstruct_ast expr_ast in
    flow_eqt ~strict loc (t, val_t);
    let t_out = BoolT.at loc |> with_trust bogus_trust in
    let out = empty_result ((loc, t_out), ast) in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  let eq_test loc ~sense ~strict left right reconstruct_ast =
    let is_number_literal node =
      match node with
      | Expression.Literal { Literal.value = Literal.Number _; _ }
      | Expression.Unary {
        Unary.operator = Unary.Minus;
        argument = _, Expression.Literal {
          Literal.value = Literal.Number _; _ };
        comments = _ }
        -> true
      | _ -> false
    in
    let extract_number_literal node =
      match node with
      | Expression.Literal { Literal.value = Literal.Number lit; raw; comments= _ } ->
        lit, raw
      | Expression.Unary { Unary.operator = Unary.Minus;
          argument = _, Expression.Literal {
            Literal.value = Literal.Number lit; raw; _ };
          comments = _ } ->
        -.lit, ("-" ^ raw)
      | _ -> Utils_js.assert_false "not a number literal"
    in

    match left, right with
    (* typeof expr ==/=== string *)
    (* this must happen before the case below involving Literal.String in order
       to match anything. *)
    | (typeof_loc, Expression.Unary { Unary.operator = Unary.Typeof; argument; comments }),
      (str_loc, (Expression.Literal { Literal.value = Literal.String s; _ } as lit_exp)) ->
      typeof_test loc sense argument s str_loc (fun argument ->
        let left_t = StrT.at typeof_loc |> with_trust bogus_trust in
        let left = (typeof_loc, left_t), Expression.Unary {
          Unary.operator = Unary.Typeof; argument; comments
        } in
        let right_t = StrT.at str_loc |> with_trust bogus_trust in
        let right = (str_loc, right_t), lit_exp in
        reconstruct_ast left right
      )
    | (str_loc, (Expression.Literal { Literal.value = Literal.String s; _ } as lit_exp)),
      (typeof_loc, Expression.Unary { Unary.operator = Unary.Typeof; argument; comments }) ->
      typeof_test loc sense argument s str_loc (fun argument ->
        let left_t = StrT.at str_loc |> with_trust bogus_trust in
        let left = (str_loc, left_t), lit_exp in
        let right_t = StrT.at typeof_loc |> with_trust bogus_trust in
        let right = (typeof_loc, right_t), Expression.Unary {
          Unary.operator = Unary.Typeof; argument; comments
        } in
        reconstruct_ast left right
      )
    | (typeof_loc, Expression.Unary { Unary.operator = Unary.Typeof; argument; comments }),
      (str_loc, (Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = s; _
          }; _
        }];
        expressions = [];
      } as lit_exp)) ->
      typeof_test loc sense argument s str_loc (fun argument ->
        let left_t = StrT.at typeof_loc |> with_trust bogus_trust in
        let left = (typeof_loc, left_t), Expression.Unary {
          Unary.operator = Unary.Typeof; argument; comments
        } in
        let right_t = StrT.at str_loc |> with_trust bogus_trust in
        let right = (str_loc, right_t), lit_exp in
        reconstruct_ast left right
      )
    | (str_loc, (Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = s; _
          }; _
        }];
        expressions = [];
      } as lit_exp)),
      (typeof_loc, Expression.Unary { Unary.operator = Unary.Typeof; argument; comments }) ->
      typeof_test loc sense argument s str_loc (fun argument ->
        let left_t = StrT.at str_loc |> with_trust bogus_trust in
        let left = (str_loc, left_t), lit_exp in
        let right_t = StrT.at typeof_loc |> with_trust bogus_trust in
        let right = (typeof_loc, right_t), Expression.Unary {
          Unary.operator = Unary.Typeof; argument; comments
        } in
        reconstruct_ast left right
      )

    (* special case equality relations involving booleans *)
    | (lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _}) as value,
      expr ->
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonBoolP (lit_loc, lit))
        (fun expr -> reconstruct_ast val_ast expr)
    | expr,
      ((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _}) as value) ->
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonBoolP (lit_loc, lit))
        (fun expr -> reconstruct_ast expr val_ast)

    (* special case equality relations involving strings *)
    | ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _}) as value),
      expr
    | ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [lit_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value), expr ->
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonStrP (lit_loc, sense, lit))
        (fun expr -> reconstruct_ast val_ast expr)
    | expr,
      ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _}) as value)
    | expr, ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [lit_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value) ->
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonStrP (lit_loc, sense, lit))
        (fun expr -> reconstruct_ast expr val_ast)

    (* special case equality relations involving numbers *)
    | ((lit_loc, number_literal) as value),
      expr when is_number_literal number_literal ->
      let lit, raw = extract_number_literal number_literal in
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonNumP (lit_loc, sense, (lit, raw)))
        (fun expr -> reconstruct_ast val_ast expr)
    | expr,
      ((lit_loc, number_literal) as value) when is_number_literal number_literal ->
      let lit, raw = extract_number_literal number_literal in
      let (_, val_t), _ as val_ast = expression cx value in
      literal_test loc ~sense ~strict expr val_t (SingletonNumP (lit_loc, sense, (lit, raw)))
        (fun expr -> reconstruct_ast expr val_ast)

    (* TODO: add Type.predicate variant that tests number equality *)

    (* expr op null *)
    | (_, Expression.Literal { Literal.value = Literal.Null; _ } as null), expr ->
      let (_, null_t), _ as null_ast = expression cx null in
      null_test loc ~sense ~strict expr null_t
        (fun expr -> reconstruct_ast null_ast expr)
    | expr, (_, Expression.Literal { Literal.value = Literal.Null; _ } as null) ->
      let (_, null_t), _ as null_ast = expression cx null in
      null_test loc ~sense ~strict expr null_t
        (fun expr -> reconstruct_ast expr null_ast)

    (* expr op undefined *)
    | (_, Identifier (_, { Ast.Identifier.name= "undefined"; comments= _ }) as void), expr ->
      let (_, void_t), _ as void_ast = expression cx void in
      undef_test loc ~sense ~strict expr void_t
        (fun expr -> reconstruct_ast void_ast expr)
    | expr, (_, Identifier (_, { Ast.Identifier.name= "undefined"; comments= _ }) as void) ->
      let (_, void_t), _ as void_ast = expression cx void in
      undef_test loc ~sense ~strict expr void_t
        (fun expr -> reconstruct_ast expr void_ast)

    (* expr op void(...) *)
    | (_, Unary ({ Unary.operator = Unary.Void; _ }) as void), expr ->
      let (_, void_t), _ as void_ast = expression cx void in
      void_test loc ~sense ~strict expr void_t
        (fun expr -> reconstruct_ast void_ast expr)
    | expr, (_, Unary ({ Unary.operator = Unary.Void; _ }) as void) ->
      let (_, void_t), _ as void_ast = expression cx void in
      void_test loc ~sense ~strict expr void_t
        (fun expr -> reconstruct_ast expr void_ast)

    (* fallback case for equality relations involving sentinels (this should be
       lower priority since it refines the object but not the property) *)
    | (_, Expression.Member _ as expr), value ->
      let (_, value_t), _ as value_ast = expression cx value in
      sentinel_prop_test loc ~sense ~strict expr value_t
        (fun expr -> reconstruct_ast expr value_ast)
    | value, (_, Expression.Member _ as expr) ->
      let (_, value_t), _ as value_ast = expression cx value in
      sentinel_prop_test loc ~sense ~strict expr value_t
        (fun expr -> reconstruct_ast value_ast expr)

    (* for all other cases, walk the AST but always return bool *)
    | expr, value ->
      let (_, t1), _ as expr = expression cx expr in
      let (_, t2), _ as value = expression cx value in
      flow_eqt ~strict loc (t1, t2);
      let ast = reconstruct_ast expr value in
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      empty_result ((loc, t_out), ast)
  in

  let mk_and map1 map2 = Key_map.merge
    (fun _ p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> Some p
      | (Some p1, Some p2) -> Some (AndP(p1,p2))
    )
    map1 map2
  in

  let mk_or map1 map2 = Key_map.merge
    (fun _ p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some _, None)
      | (None, Some _) -> None
      | (Some p1, Some p2) -> Some (OrP(p1,p2))
    )
    map1 map2
  in

  (* main *)
  match e with

  (* member expressions *)
  | loc, Member {
      Member._object;
      property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name= prop_name; comments= _ } as id));
    } ->
      let (_, obj_t), _ as _object_ast = match _object with
      | super_loc, Super ->
          let t = super_ cx super_loc in
          (super_loc, t), Super

      | _ ->
          (* use `expression` instead of `condition` because `_object` is the
             object in a member expression; if it itself is a member expression,
             it must exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here
             as `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
          expression cx _object
      in
      let expr_reason = mk_expression_reason e in
      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let t = match Refinement.get cx e loc with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then Unsoundness.at InferenceHooks prop_loc
        else
          let use_op = Op (GetProperty (mk_expression_reason e)) in
          get_prop ~is_cond:true cx
            expr_reason ~use_op obj_t (prop_reason, prop_name)
      in
      let property = Member.PropertyIdentifier ((prop_loc, t), id) in
      let ast = (loc, t), Member { Member._object = _object_ast; property; } in

      (* since we never called `expression cx e`, we have to add to the
         type table ourselves *)

      let out = match Refinement.key e with
      | Some name -> result ast name t (ExistsP (Some loc)) true
      | None -> empty_result ast
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      begin match Refinement.key _object with
      | Some name ->
        let predicate = PropExistsP (prop_name, Some prop_loc) in
        out |> add_predicate name obj_t predicate true
      | None ->
        out
      end

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let (_, expr), _ as tast = expression cx e in
      let id = id.Ast.Pattern.Identifier.name in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result tast name expr (ExistsP (Some loc)) true
      | None, _ -> empty_result tast
    )

  (* expr instanceof t *)
  | loc, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      let bool = BoolT.at loc |> with_trust bogus_trust in
      let name_opt, ((_, left_t), _ as left_ast) = refinable_lvalue left in
      let (_, right_t), _ as right_ast = expression cx right in
      let ast =
        (loc, bool),
        Binary { Binary.
          operator = Binary.Instanceof;
          left = left_ast;
          right = right_ast;
        }
      in
      match name_opt with
      | Some name ->
          let pred = LeftP (InstanceofTest, right_t) in
          result ast name left_t pred true
      | None ->
          empty_result ast
    )

  (* expr op expr *)
  | loc, Binary { Binary.operator = Binary.Equal; left; right; } ->
      eq_test loc ~sense:true ~strict:false left right
      (fun left right -> Binary { Binary.operator = Binary.Equal; left; right; })
  | loc, Binary { Binary.operator = Binary.StrictEqual; left; right; } ->
      eq_test loc ~sense:true ~strict:true left right
      (fun left right -> Binary { Binary.operator = Binary.StrictEqual; left; right; })
  | loc, Binary { Binary.operator = Binary.NotEqual; left; right; } ->
      eq_test loc ~sense:false ~strict:false left right
      (fun left right -> Binary { Binary.operator = Binary.NotEqual; left; right; })
  | loc, Binary { Binary.operator = Binary.StrictNotEqual; left; right; } ->
      eq_test loc ~sense:false ~strict:true left right
      (fun left right -> Binary { Binary.operator = Binary.StrictNotEqual; left; right; })

  (* Array.isArray(expr) *)
  | loc, Call {
      Call.callee = callee_loc, Member {
        Member._object = (_, Identifier (_, { Ast.Identifier.name= "Array"; comments= _ }) as o);
        property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name= "isArray"; comments= _ } as id));
      };
      targs;
      arguments = [Expression arg];
    } -> (
      Option.iter targs ~f:(fun _ ->
        Flow.add_output cx Error_message.(ECallTypeArity {
          call_loc = loc;
          is_new = false;
          reason_arity = Reason.(locationless_reason (RFunction RNormal));
          expected_arity = 0;
        }));
      (* get Array.isArray in order to populate the type tables, but we don't
         care about the result. *)
      (* TODO: one day we can replace this with a call to `method_call`, and
         then discard the result. currently MethodT does not update type_table
         properly. *)
      let (_, obj_t), _ as _object = expression cx o in
      let reason = mk_reason (RCustom "`Array.isArray(...)`") callee_loc in
      let fn_t = Tvar.mk_where cx reason (fun t ->
        let prop_reason = mk_reason (RProperty (Some "isArray")) prop_loc in
        let use_op = Op (GetProperty (mk_expression_reason e)) in
        Flow.flow cx (obj_t, GetPropT (use_op, reason, Named (prop_reason, "isArray"), t))
      ) in

      let bool = BoolT.at loc |> with_trust bogus_trust in
      let name_opt, ((_, t), _ as arg) = refinable_lvalue arg in
      let property = Member.PropertyIdentifier ((prop_loc, fn_t), id) in
      let ast =
        (loc, bool),
        Call { Call.
          callee = (callee_loc, fn_t), Member { Member._object; property; };
          targs = None;
          arguments = [ Expression arg ];
        }
      in
      match name_opt with
      | Some name ->
          result ast name t ArrP true
      | None ->
          empty_result ast
    )

  (* test1 && test2 *)
  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let ((_, t1), _ as left_ast), map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let ((_, t2), _ as right_ast), map2, not_map2, xts2 = Env.in_refined_env cx loc map1 xts1
        (fun () -> predicates_of_condition cx right) in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      let t_out = Tvar.mk_where cx reason (fun t -> Flow.flow cx (t1, AndT (reason, t2, t))) in
      ((loc, t_out), Logical { Logical.
        operator = Logical.And;
        left = left_ast;
        right = right_ast;
      }),
      mk_and map1 map2,
      mk_or not_map1 not_map2,
      Key_map.union xts1 xts2

  (* test1 || test2 *)
  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let () = check_default_pattern cx left right in
      let ((_, t1), _ as left_ast), map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let ((_, t2), _ as right_ast), map2, not_map2, xts2 = Env.in_refined_env cx loc not_map1 xts1
        (fun () -> predicates_of_condition cx right) in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      let t_out = Tvar.mk_where cx reason (fun t -> Flow.flow cx (t1, OrT (reason, t2, t))) in
      ((loc, t_out), Logical { Logical.
        operator = Logical.Or;
        left = left_ast;
        right = right_ast;
      }),
      mk_or map1 map2,
      mk_and not_map1 not_map2,
      Key_map.union xts1 xts2

  (* !test *)
  | loc, Unary { Unary.operator = Unary.Not; argument; comments } ->
      let (arg, map, not_map, xts) = predicates_of_condition cx argument in
      let ast' = Unary { Unary.operator = Unary.Not; argument = arg; comments } in
      let t_out = BoolT.at loc |> with_trust bogus_trust in
      let ast = (loc, t_out), ast' in
      (ast, not_map, map, xts)

  (* ids *)
  | loc, This
  | loc, Identifier _
  | loc, Member _ -> (
      match refinable_lvalue e with
      | Some name, ((_, t), _ as e) -> result e name t (ExistsP (Some loc)) true
      | None, e -> empty_result e
    )

  (* e.m(...) *)
  (* TODO: Don't trap method calls for now *)
  | _, Call { Call.callee = (_, Member _); _ } ->
      empty_result (expression cx e)

  (* f(...) *)
  (* The concrete predicate is not known at this point. We attach a "latent"
     predicate pointing to the type of the function that will supply this
     predicated when it is resolved. *)
  | loc, Call ({ Call.arguments; _ } as call) ->
      let is_spread = function | Spread _ -> true | _ -> false in
      if List.exists is_spread arguments then
        empty_result (expression cx e)
      else
        let fun_t, keys, arg_ts, ret_t, call_ast =
          predicated_call_expression cx loc call in
        let ast = (loc, ret_t), Call call_ast in
        let args_with_offset = ListUtils.zipi keys arg_ts in
        let emp_pred_map = empty_result ast in
        List.fold_left (fun pred_map arg_info -> match arg_info with
          | (idx, Some key, unrefined_t) ->
              let pred = LatentP (fun_t, idx+1) in
              add_predicate key unrefined_t pred true pred_map
          | _ ->
              pred_map
        ) emp_pred_map args_with_offset

  (* fallthrough case: evaluate test expr, no refinements *)
  | e ->
      empty_result (expression cx e)
))


(* Conditional expressions are checked like expressions, except that property
   accesses are provisionally allowed even when such properties do not exist.
   This accommodates the common JavaScript idiom of testing for the existence
   of a property before using that property. *)
and condition cx e : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t =
  expression ~is_cond:true cx e

and get_private_field_opt_use reason ~use_op name =
  let class_entries = Env.get_class_entries () in
  OptGetPrivatePropT (use_op, reason, name, class_entries, false)

and get_private_field cx reason ~use_op tobj name =
  Tvar.mk_where cx reason (fun t ->
    let opt_use = get_private_field_opt_use reason ~use_op name in
    let get_prop_u = apply_opt_use opt_use t in
    Flow.flow cx (tobj, get_prop_u)
  )

(* Property lookups become non-strict when processing conditional expressions
   (see above).

   TODO: It should be possible to factor the processing of LHS / reference
   expressions out of `expression`, somewhat like what assignment_lhs does. That
   would make everything involving Refinement be in the same place.
*)
and get_prop_opt_use ~is_cond reason ~use_op (prop_reason, name) =
  if is_cond
  then OptTestPropT (reason, mk_id (), Named (prop_reason, name))
  else OptGetPropT (use_op, reason, Named (prop_reason, name))

and get_prop ~is_cond cx reason ~use_op tobj (prop_reason, name) =
  let opt_use = get_prop_opt_use ~is_cond reason ~use_op (prop_reason, name) in
  Tvar.mk_where cx reason (fun t ->
    let get_prop_u = apply_opt_use opt_use t in
    Flow.flow cx (tobj, get_prop_u)
  )

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx loc callee_loc prop_loc expr obj_t m targs args =
  let open Ast.Expression in

  let reason = mk_reason (RCustom (spf "`Object.%s`" m)) loc in
  match (m, targs, args) with
  | "create", None, [Expression e] ->
    let (_, e_t), _ as e_ast = expression cx e in
    let proto =
      let reason = mk_reason RPrototype (fst e) in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (e_t, ObjTestProtoT (reason, t))
      )
    in
    Obj_type.mk_with_proto cx reason proto,
    None,
    [Expression e_ast]

  | "create", None, [Expression e; Expression (obj_loc, Object { Object.properties; comments })] ->
    let (_, e_t), _ as e_ast = expression cx e in
    let proto =
      let reason = mk_reason RPrototype (fst e) in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (e_t, ObjTestProtoT (reason, t))
      )
    in
    let pmap, properties = prop_map_of_object cx properties in
    let props = SMap.fold (fun x p acc ->
      let loc = Property.read_loc p in
      match Property.read_t p with
      | None ->
        (* Since the properties object must be a literal, and literal objects
           can only ever contain neutral fields, this should not happen. *)
        Flow.add_output cx Error_message.(
          EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)
        );
        acc
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let t = Tvar.mk_where cx reason (fun tvar ->
          Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar))
        ) in
        let p = Field (loc, t, Polarity.Neutral) in
        SMap.add x p acc
    ) pmap SMap.empty in
    Obj_type.mk_with_proto cx reason ~props proto,
    None,
    [
      Expression e_ast;
      (* TODO(vijayramamurthy) construct object type *)
      Expression ((obj_loc, AnyT.at Untyped obj_loc), Object { Object.properties; comments })
    ]

  | ("getOwnPropertyNames" | "keys"), None, [Expression e] ->
    let arr_reason = mk_reason RArrayType loc in
    let (_, o), _ as e_ast = expression cx e in
    DefT (arr_reason, bogus_trust (), ArrT (
      ArrayAT (
        Tvar.mk_where cx arr_reason (fun tvar ->
          let keys_reason = replace_reason (fun desc ->
            RCustom (spf "element of %s" (string_of_desc desc))
          ) reason in
          Flow.flow cx (o, GetKeysT (keys_reason, UseT (unknown_use, tvar)));
        ),
        None
      )
    )),
    None,
    [Expression e_ast]

  | "defineProperty", None, [
      Expression e;
      Expression ((ploc, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.String x; _ }
      ) as key);
      Expression config;
    ] ->
    let (_, o), _ as e_ast = expression cx e in
    let key_ast = expression cx key in
    let (_, spec), _ as config_ast = expression cx config in
    let tvar = Tvar.mk cx reason in
    let prop_reason = mk_reason (RProperty (Some x)) ploc in
    Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar));
    let prop_t = Tvar.mk cx prop_reason in
    Flow.flow cx (o, SetPropT (
      unknown_use, reason, Named (prop_reason, x), Normal, tvar, Some prop_t
    ));
    o,
    None,
    [Expression e_ast; Expression key_ast; Expression config_ast]

  | "defineProperties", None, [Expression e; Expression (obj_loc, Object { Object.properties; comments })] ->
    let (_, o), _ as e_ast = expression cx e in
    let pmap, properties = prop_map_of_object cx properties in
    pmap |> SMap.iter (fun x p ->
      match Property.read_t p with
      | None ->
        (* Since the properties object must be a literal, and literal objects
           can only ever contain neutral fields, this should not happen. *)
        Flow.add_output cx Error_message.(
          EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)
        );
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let tvar = Tvar.mk cx reason in
        Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar));
        Flow.flow cx (o, SetPropT (
          unknown_use, reason, Named (reason, x), Normal, tvar, None
        ));
    );
    o,
    None,
    [
      Expression e_ast;
      (* TODO(vijayramamurthy) construct object type *)
      Expression ((obj_loc, AnyT.at Untyped obj_loc), Object { Object.properties; comments })
    ]

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | "freeze", None, [Expression ((arg_loc, Object _) as e)] ->
    let (_, arg_t), _ as e_ast = expression cx e in

    let arg_t = Object_freeze.freeze_object cx arg_loc arg_t in

    let reason = mk_reason (RMethodCall (Some m)) loc in
    snd (method_call cx reason prop_loc ~use_op:unknown_use (expr, obj_t, m) None [Arg arg_t]),
    None,
    [Expression e_ast]

  | ( "create"
    | "getOwnPropertyNames"
    | "keys"
    | "defineProperty"
    | "defineProperties"
    | "freeze"  ),
    Some (targs_loc, targs),
    _ ->
    let targs = snd (convert_tparam_instantiations cx SMap.empty targs) in
    let args = Core_list.map ~f:(fun arg -> snd (expression_or_spread cx arg)) args in
    Flow.add_output cx Error_message.(ECallTypeArity {
      call_loc = loc;
      is_new = false;
      reason_arity = Reason.(locationless_reason (RFunction RNormal));
      expected_arity = 0;
    });
    AnyT.at AnyError loc,
    Some (targs_loc, targs),
    args

  (* TODO *)
  | _ ->
    let targts, targ_asts = convert_targs cx targs in
    let argts, arg_asts =
      args
      |> Core_list.map ~f:(expression_or_spread cx)
      |> List.split in
    let reason = mk_reason (RMethodCall (Some m)) loc in
    let use_op = Op (FunCallMethod {
      op = reason;
      fn = mk_reason (RMethod (Some m)) callee_loc;
      prop = mk_reason (RProperty (Some m)) prop_loc;
      args = mk_initial_arguments_reason args;
      local = true;
    }) in
    snd (method_call cx reason ~use_op prop_loc (expr, obj_t, m) targts argts),
    targ_asts,
    arg_asts

and extract_class_name class_loc  = Ast.Class.(function {id; _;} ->
  match id with
  | Some(name_loc, { Ast.Identifier.name; comments= _ }) -> (name_loc, name)
  | None -> (class_loc, "<<anonymous class>>")
)

and check_properties_initialized_before_use cx class_ast: unit =
  let (_, body) = class_ast.Ast.Class.body in
  let elements = body.Ast.Class.Body.body in

  let uninitialized: ALoc.t Class_property_map.t =
    List.fold_left (fun uninited -> Ast.Class.(function
      | Body.Property (_, {
          Property.key = Ast.Expression.Object.Property.Identifier (
            loc, { Ast.Identifier.name; _ }
          );
          Property.value = None;
          Property.static = false;
          _
        }) -> Class_property_map.add (Class_property.Public name) loc uninited
      | Body.PrivateField (_, {
          PrivateField.key = (loc, ident);
          PrivateField.value = None;
          PrivateField.static = false;
          _
        }) -> Class_property_map.add (Class_property.Private (ident_name ident)) loc uninited
      | _ -> uninited
      )
    ) Class_property_map.empty elements
  in

  uninitialized
  |> Class_property_map.iter (fun _ loc ->
    Flow.add_output cx Error_message.(
      EUninitializedInstanceProperty (loc, PropertyNotDefinitivelyInitialized)
    )
  )

and mk_class cx class_loc ~name_loc reason c =
  let def_reason = repos_reason class_loc reason in
  let this_in_class = Class_stmt_sig.This.in_class c in
  let self = Tvar.mk cx reason in
  let class_sig, class_ast_f = mk_class_sig cx name_loc reason self c in
  class_sig |> Class_stmt_sig.generate_tests cx (fun class_sig ->
    Class_stmt_sig.check_super cx def_reason class_sig;
    Class_stmt_sig.check_implements cx def_reason class_sig;
    if this_in_class || not (Class_stmt_sig.This.is_bound_to_empty class_sig) then
      Class_stmt_sig.toplevels cx class_sig
      ~decls:toplevel_decls
      ~stmts:toplevels
      ~expr:expression;
  );
  check_properties_initialized_before_use cx c;
  let class_t = Class_stmt_sig.classtype cx class_sig in
  Flow.unify cx self class_t;
  class_t, class_ast_f class_t


(* Process a class definition, returning a (polymorphic) class type. A class
   type is a wrapper around an instance type, which contains types of instance
   members, a pointer to the super instance type, and a container for types of
   static members. The static members can be thought of as instance members of a
   "metaclass": thus, the static type is itself implemented as an instance
   type. *)
and mk_class_sig =
  let open Class_stmt_sig in

  (*  Given information about a field, returns:
      - Class_sig.field representation of this field
      - typed AST of the field's type annotation
      - a function which will return a typed AST of the field's initializer expression.
        Function should only be called after Class_sig.toplevels has been called on a
        Class_sig.t containing this field, as that is when the initializer expression
        gets checked.
  *)
  let mk_field cx tparams_map reason annot init =
    let annot_t, annot_ast = Anno.mk_type_annotation cx tparams_map reason annot in
    let field, get_init =
      match init with
      | None -> Annot annot_t, Fn.const None
      | Some expr ->
        let value_ref : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option ref = ref None in
        Infer (
          Func_stmt_sig.field_initializer tparams_map reason expr annot_t,
          (fun (_, _, value_opt) -> value_ref := Some (Option.value_exn value_opt))
        ),
        (fun () -> Some (Option.value (!value_ref)
          ~default:(Tast_utils.error_mapper#expression expr)))
    in
    field, annot_t, annot_ast, get_init
  in

  let mk_method = mk_func_sig in

  let mk_extends cx tparams_map = function
    | None -> Implicit { null = false }, None
    | Some (loc, { Ast.Class.Extends.expr; targs }) ->
      let (_, c), _ as expr = expression cx expr in
      let t, targs = Anno.mk_super cx tparams_map loc c targs in
      Explicit t, Some (loc, { Ast.Class.Extends.expr; targs })
  in

  let warn_or_ignore_decorators cx = function
  | [] -> []
  | decorators ->
    match Context.esproposal_decorators cx with
    | Options.ESPROPOSAL_ENABLE -> failwith "Decorators cannot be enabled!"
    | Options.ESPROPOSAL_IGNORE ->
      Core_list.map ~f:Tast_utils.unchecked_mapper#class_decorator decorators
    | Options.ESPROPOSAL_WARN ->
      List.iter (fun (loc, _) ->
        Flow.add_output cx (Error_message.EExperimentalDecorators loc)
      ) decorators;
      Core_list.map ~f:Tast_utils.error_mapper#class_decorator decorators
  in

  let warn_or_ignore_class_properties cx ~static loc =
    let config_setting =
      if static
      then Context.esproposal_class_static_fields cx
      else Context.esproposal_class_instance_fields cx
    in
    match config_setting with
    | Options.ESPROPOSAL_ENABLE
    | Options.ESPROPOSAL_IGNORE -> ()
    | Options.ESPROPOSAL_WARN ->
      Flow.add_output cx
        (Error_message.EExperimentalClassProperties (loc, static))
  in

  fun cx name_loc reason self { Ast.Class.
    id;
    body = (body_loc, { Ast.Class.Body.body = elements });
    tparams;
    extends;
    implements;
    classDecorators;
  } ->

  let classDecorators_ast = warn_or_ignore_decorators cx classDecorators in

  let tparams, tparams_map, tparams_ast =
    Anno.mk_type_param_declarations cx tparams
  in

  let self', tparams, tparams_map =
    add_this self cx reason tparams tparams_map
  in

  let class_sig, extends_ast, implements_ast =
    let id = name_loc in
    let extends, extends_ast = mk_extends cx tparams_map extends in
    let implements, implements_ast = implements |> Core_list.map ~f:(fun (loc, i) ->
      let { Ast.Class.Implements.id = (id_loc, ({ Ast.Identifier.name; comments= _ } as id)); targs } = i in
      let c = Env.get_var ~lookup_mode:Env.LookupMode.ForType cx name id_loc in
      let typeapp, targs = match targs with
      | None -> (loc, c, None), None
      | Some (targs_loc, targs) ->
        let ts, targs_ast = Anno.convert_list cx tparams_map targs in
        (loc, c, Some ts), Some (targs_loc, targs_ast)
      in
      typeapp, (loc, { Ast.Class.Implements.id = (id_loc, c), id; targs })
    ) |> List.split in
    let super = Class { extends; mixins = []; implements } in
    empty id reason tparams tparams_map super, extends_ast, implements_ast
  in

  (* In case there is no constructor, pick up a default one. *)
  let class_sig =
    if extends <> None
    then
      (* Subclass default constructors are technically of the form (...args) =>
         { super(...args) }, but we can approximate that using flow's existing
         inheritance machinery. *)
      (* TODO: Does this distinction matter for the type checker? *)
      class_sig
    else
      let reason = replace_reason_const RDefaultConstructor reason in
      add_default_constructor reason class_sig
  in

  (* All classes have a static "name" property. *)
  let class_sig = add_name_field class_sig in

  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  (* NOTE: field initializer expressions and method bodies don't get checked
      until Class_sig.toplevels is called on class_sig. For this reason rather
      than returning a typed AST, we'll return a function which returns a typed
      AST, and this function shouldn't be called until after Class_sig.toplevels
      has been called.

      If a field/method ever gets shadowed later in the class, then its
      initializer/body (respectively) will not get checked, and the corresponding
      nodes of the typed AST will be filled in with error nodes.
  *)
  let class_sig, rev_elements = List.fold_left Ast.Class.(fun (c, rev_elements) -> function
    (* instance and static methods *)
    | Body.Property (_, {
        Property.key = Ast.Expression.Object.Property.PrivateName _;
        _
      }) -> failwith "Internal Error: Found non-private field with private name"

    | Body.Method (_, {
        Method.key = Ast.Expression.Object.Property.PrivateName _;
        _
      }) -> failwith "Internal Error: Found method with private name"

    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (id_loc, ({ Ast.Identifier.name; comments= _ } as id));
        value = (func_loc, func);
        kind;
        static;
        decorators;
      }) ->

      Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;
      let decorators = warn_or_ignore_decorators cx decorators in

      (match kind with
      | Method.Get | Method.Set -> Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc)
      | _ -> ());

      let method_sig, reconstruct_func = mk_method cx tparams_map loc func in
      (*  The body of a class method doesn't get checked until Class_sig.toplevels
          is called on the class sig (in this case c). The order of how the methods
          were arranged in the class is lost by the time this happens, so rather
          than attempting to return a list of method bodies from the Class_sig.toplevels
          function, we have it place the function bodies into a list via side effects.
          We use a similar approach for method types *)
      let params_ref : (ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t option ref = ref None in
      let body_ref : (ALoc.t, ALoc.t * Type.t) Ast.Function.body option ref = ref None in
      let set_asts (params_opt, body_opt, _) =
        params_ref := Some (Option.value_exn params_opt);
        body_ref := Some (Option.value_exn body_opt)
      in
      let func_t_ref : Type.t option ref = ref None in
      let set_type t = func_t_ref := Some t in
      let get_element () =
        let params = Option.value (!params_ref)
          ~default:(Tast_utils.error_mapper#function_params func.Ast.Function.params) in
        let body = Option.value (!body_ref)
          ~default:(Tast_utils.error_mapper#function_body func.Ast.Function.body) in
        let func_t = Option.value (!func_t_ref) ~default:(EmptyT.at id_loc |> with_trust bogus_trust ) in
        let func = reconstruct_func params body func_t in
        Body.Method ((loc, func_t), { Method.
          key = Ast.Expression.Object.Property.Identifier ((id_loc, func_t), id);
          value = func_loc, func;
          kind;
          static;
          decorators;
        })
      in
      let add = match kind with
      | Method.Constructor -> add_constructor (Some id_loc)
      | Method.Method -> add_method ~static name id_loc
      | Method.Get -> add_getter ~static name id_loc
      | Method.Set -> add_setter ~static name id_loc
      in
      add method_sig ~set_asts ~set_type c, get_element::rev_elements

    (* fields *)
    | Body.PrivateField(loc, {
        PrivateField.key = (_, (id_loc, { Ast.Identifier.name; comments= _ })) as key;
        annot;
        value;
        static;
        variance;
      }) ->
        Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;

        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let reason = mk_reason (RProperty (Some name)) loc in
        let polarity = Anno.polarity variance in
        let field, annot_t, annot_ast, get_value = mk_field cx tparams_map reason annot value in
        let get_element () = Body.PrivateField ((loc, annot_t), { PrivateField.
          key;
          annot = annot_ast;
          value = get_value ();
          static;
          variance;
        }) in
        add_private_field ~static name id_loc polarity field c, get_element::rev_elements

    | Body.Property (loc, {
      Property.key = Ast.Expression.Object.Property.Identifier (id_loc, ({ Ast.Identifier.name; comments= _ } as id));
        annot;
        value;
        static;
        variance;
      }) ->
        Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;

        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let reason = mk_reason (RProperty (Some name)) loc in
        let polarity = Anno.polarity variance in
        let field, annot_t, annot, get_value = mk_field cx tparams_map reason annot value in
        let get_element () = Body.Property ((loc, annot_t), { Property.
          key = Ast.Expression.Object.Property.Identifier ((id_loc, annot_t), id);
          annot;
          value = get_value ();
          static;
          variance;
        }) in
        add_field ~static name id_loc polarity field c, get_element::rev_elements

    (* literal LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Literal _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Literal _;
        _
      }) as elem ->
        Flow.add_output cx
          Error_message.(EUnsupportedSyntax (loc, ClassPropertyLiteral));
        c, (fun () -> Tast_utils.error_mapper#class_element elem)::rev_elements

    (* computed LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Computed _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Computed _;
        _
      }) as elem ->
        Flow.add_output cx
          Error_message.(EUnsupportedSyntax (loc, ClassPropertyComputed));
        c, (fun () -> Tast_utils.error_mapper#class_element elem)::rev_elements
  ) (class_sig, []) elements
  in
  let elements = List.rev rev_elements in
  class_sig,
  (fun class_t -> { Ast.Class.
    id = Option.map ~f:(fun (loc, name) -> (loc, class_t), name) id;
    body = (body_loc, self'), { Ast.Class.Body.
      body = Core_list.map ~f:(fun f -> f ()) elements;
    };
    tparams = tparams_ast;
    extends = extends_ast;
    implements = implements_ast;
    classDecorators = classDecorators_ast;
  })

and mk_func_sig =
  let function_kind ~async ~generator ~predicate =
    let open Func_sig in
    Ast.Type.Predicate.(match async, generator, predicate with
    | true, true, None -> AsyncGenerator
    | true, false, None -> Async
    | false, true, None -> Generator
    | false, false, None -> Ordinary
    | false, false, Some (_, Declared _) -> Predicate
    | false, false, Some (_ , Ast.Type.Predicate.Inferred) -> Predicate
    | _, _, _ -> Utils_js.assert_false "(async || generator) && pred")
  in

  let id_param cx tparams_map id mk_reason =
    let { Ast.Pattern.Identifier.name; annot; optional } = id in
    let (id_loc, ({ Ast.Identifier.name; comments = _ } as id)) = name in
    let reason = mk_reason name in
    let t, annot = Anno.mk_type_annotation cx tparams_map reason annot in
    let name = (id_loc, t), id in
    t, { Ast.Pattern.Identifier.name; annot; optional }
  in

  let mk_param cx tparams_map param =
    let (loc, { Ast.Function.Param.argument = (ploc, patt); default }) = param in
    let expr = expression in
    let t, pattern = match patt with
    | Ast.Pattern.Identifier id ->
      let t, id = id_param cx tparams_map id (fun name ->
        mk_reason (RParameter (Some name)) ploc
      ) in
      t, Func_stmt_config.Id id
    | Ast.Pattern.Object { Ast.Pattern.Object.annot; properties } ->
      let reason = mk_reason RDestructuring ploc in
      let t, annot = Anno.mk_type_annotation cx tparams_map reason annot in
      t, Func_stmt_config.Object { annot; properties }
    | Ast.Pattern.Array { Ast.Pattern.Array.annot; elements } ->
      let reason = mk_reason RDestructuring ploc in
      let t, annot = Anno.mk_type_annotation cx tparams_map reason annot in
      t, Func_stmt_config.Array { annot; elements; }
    | Ast.Pattern.Expression _ ->
      failwith "unexpected expression pattern in param"
    in
    Func_stmt_config.Param { t; loc; ploc; pattern; default; expr }
  in

  let mk_rest cx tparams_map rest =
    let (loc, { Ast.Function.RestParam.argument = (ploc, patt) }) = rest in
    match patt with
    | Ast.Pattern.Identifier id ->
      let t, id = id_param cx tparams_map id (fun name ->
        mk_reason (RRestParameter (Some name)) ploc
      ) in
      Ok (Func_stmt_config.Rest { t; loc; ploc; id })
    | Ast.Pattern.Object _
    | Ast.Pattern.Array _
    | Ast.Pattern.Expression _ ->
      (* TODO: this should be a parse error, unrepresentable AST *)
      Error (Error_message.(EInternal (ploc, RestParameterNotIdentifierPattern)))
  in

  let mk_params cx tparams_map (loc, { Ast.Function.Params.params; rest })  =
    let fparams = Func_stmt_params.empty (fun params rest ->
      Some (loc, { Ast.Function.Params.params; rest })
    ) in
    let fparams = List.fold_left (fun acc param ->
      Func_stmt_params.add_param (mk_param cx tparams_map param) acc
    ) fparams params in
    let fparams = Option.fold ~f:(fun acc rest ->
      match mk_rest cx tparams_map rest with
      | Ok rest -> Func_stmt_params.add_rest rest acc
      | Error err ->
        Flow_js.add_output cx err;
        acc
    ) ~init:fparams rest in
    fparams
  in

  fun cx tparams_map loc func ->
    let {Ast.Function.
      tparams;
      return;
      body;
      predicate;
      params;
      id;
      async;
      generator;
      sig_loc = _;
    } = func in
    let reason = func_reason ~async ~generator loc in
    let kind = function_kind ~async ~generator ~predicate in
    let tparams, tparams_map, tparams_ast =
      Anno.mk_type_param_declarations cx ~tparams_map tparams
    in
    let fparams = mk_params cx tparams_map params in
    let body = Some body in
    let ret_reason = mk_reason RReturn (Func_sig.return_loc func) in
    let return_t, return =
      let has_nonvoid_return = might_have_nonvoid_return loc func in
      let definitely_returns_void =
        kind = Func_sig.Ordinary &&
        not has_nonvoid_return
      in
      Anno.mk_return_type_annotation cx tparams_map ret_reason ~definitely_returns_void return
    in
    let return_t, predicate = Ast.Type.Predicate.(match predicate with
      | None ->
          return_t, None
      | Some (loc, Ast.Type.Predicate.Inferred) ->
          (* Restrict the fresh condition type by the declared return type *)
          let fresh_t, _ = Anno.mk_type_annotation cx tparams_map ret_reason (Ast.Type.Missing loc) in
          Flow.flow_t cx (fresh_t, return_t);
          fresh_t, Some (loc, Ast.Type.Predicate.Inferred)
      | Some ((loc, Declared _) as pred) ->
          Flow_js.add_output cx Error_message.(
            EUnsupportedSyntax (loc, PredicateDeclarationForImplementation)
          );
          fst (Anno.mk_type_annotation cx tparams_map ret_reason (Ast.Type.Missing loc)),
          Some (Tast_utils.error_mapper#type_predicate pred)
    ) in
    let knot = Tvar.mk cx reason in
    {Func_stmt_sig.reason; kind; tparams; tparams_map; fparams; body; return_t; knot},
    (fun params body fun_type -> { func with Ast.Function.
      id = Option.map ~f:(fun (id_loc, name) -> (id_loc, fun_type), name) id;
      params;
      body;
      predicate;
      return;
      tparams = tparams_ast;
    })

(* Given a function declaration and types for `this` and `super`, extract a
   signature consisting of type parameters, parameter types, parameter names,
   and return type, check the body against that signature by adding `this`
   and super` to the environment, and return the signature. *)
and function_decl id cx loc func this super =
  let func_sig, reconstruct_func = mk_func_sig cx SMap.empty loc func in
  let save_return = Abnormal.clear_saved Abnormal.Return in
  let save_throw = Abnormal.clear_saved Abnormal.Throw in
  let (params_ast, body_ast, _) = func_sig |> Func_stmt_sig.generate_tests cx (
    Func_stmt_sig.toplevels id cx this super
      ~decls:toplevel_decls
      ~stmts:toplevels
      ~expr:expression
  ) in
  ignore (Abnormal.swap_saved Abnormal.Return save_return);
  ignore (Abnormal.swap_saved Abnormal.Throw save_throw);
  func_sig,
  reconstruct_func (Option.value_exn params_ast) (Option.value_exn body_ast)

(* Switch back to the declared type for an internal name. *)
and define_internal cx reason x =
  let ix = internal_name x in
  let loc = aloc_of_reason reason in
  Env.declare_let cx ix loc;
  let t = Env.get_var_declared_type cx ix loc in
  Env.init_let cx ~use_op:unknown_use ix ~has_anno:false t loc

(* Process a function declaration, returning a (polymorphic) function type. *)
and mk_function_declaration id cx loc func =
  mk_function id cx loc func

(* Process a function expression, returning a (polymorphic) function type. *)
and mk_function_expression id cx loc func =
  mk_function id cx loc func

(* Internal helper function. Use `mk_function_declaration` and `mk_function_expression` instead. *)
and mk_function id cx loc func =
  let this_t = Tvar.mk cx (mk_reason RThis loc) in
  let this = Scope.Entry.new_let this_t ~loc ~state:Scope.State.Initialized in
  (* Normally, functions do not have access to super. *)
  let super =
    let t = ObjProtoT (mk_reason RNoSuper loc) in
    Scope.Entry.new_let t ~loc ~state:Scope.State.Initialized
  in
  let func_sig, reconstruct_ast = function_decl id cx loc func this super in
  let fun_type = Func_stmt_sig.functiontype cx this_t func_sig in
  fun_type, reconstruct_ast fun_type

(* Process an arrow function, returning a (polymorphic) function type. *)
and mk_arrow cx loc func =
  let _, this = Env.find_entry cx (internal_name "this") loc in
  let _, super = Env.find_entry cx (internal_name "super") loc in
  let {Ast.Function.id; _} = func in
  let func_sig, reconstruct_ast = function_decl id cx loc func this super in
  (* Do not expose the type of `this` in the function's type. The call to
     function_decl above has already done the necessary checking of `this` in
     the body of the function. Now we want to avoid re-binding `this` to
     objects through which the function may be called. *)
  let fun_type = Func_stmt_sig.functiontype cx dummy_this func_sig in
  fun_type, reconstruct_ast fun_type

(* Transform predicate declare functions to functions whose body is the
   predicate declared for the funcion *)
(* Also returns a function for reversing this process, for the sake of
   typed AST construction. *)
and declare_function_to_function_declaration cx declare_loc func_decl =
  let { Ast.Statement.DeclareFunction.id; annot; predicate; } = func_decl in
  match predicate with
  | Some (loc, Ast.Type.Predicate.Inferred) ->
      Flow.add_output cx Error_message.(
        EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)
      );
      None

  | Some (loc, Ast.Type.Predicate.Declared e) -> begin
      match annot with
      | (annot_loc, (func_annot_loc, Ast.Type.Function
        { Ast.Type.Function.params = (params_loc, { Ast.Type.Function.Params.params; rest });
          Ast.Type.Function.return;
          Ast.Type.Function.tparams;
        })) ->
          let param_type_to_param = Ast.Type.Function.(
            fun (l, { Param.name; Param.annot; _ }) ->
              let name = match name with
              | Some name -> name
              | None ->
                  let name_loc = fst annot in
                  Flow.add_output cx Error_message.(EUnsupportedSyntax
                    (loc, PredicateDeclarationAnonymousParameters));
                  (name_loc, mk_ident ~comments:None "_")
              in
              let name' = ({ Ast.Pattern.Identifier.
                name;
                annot = Ast.Type.Available (fst annot, annot);
                optional = false;
              }) in
              (l, Ast.Pattern.Identifier name')
          ) in
          let params = Core_list.map ~f:(fun param ->
            let (loc, _) as argument = param_type_to_param param in
            (loc, { Ast.Function.Param.argument; default = None })
          ) params in
          let rest = Ast.Type.Function.(
            match rest with
            | Some (rest_loc, { RestParam.argument; }) ->
              let argument = param_type_to_param argument in
              Some (rest_loc, { Ast.Function.RestParam.argument; })
            | None -> None
          ) in
          let body = Ast.Function.BodyBlock (loc, {Ast.Statement.Block.body = [
              (loc, Ast.Statement.Return { Ast.Statement.Return.
                argument = Some e;
                comments = Flow_ast_utils.mk_comments_opt ();
              })
            ]}) in
          let return = Ast.Type.Available (loc, return) in
          Some (Ast.Statement.FunctionDeclaration { Ast.Function.
            id = Some id;
            params = (params_loc, { Ast.Function.Params.params; rest });
            body;
            async = false;
            generator = false;
            predicate = Some (loc, Ast.Type.Predicate.Inferred);
            return;
            tparams;
            sig_loc = declare_loc;
          }, function
          | _, Ast.Statement.FunctionDeclaration { Ast.Function.
              id = Some ((id_loc, fun_type), id_name);
              tparams;
              params = params_loc, { Ast.Function.Params.params; rest };
              return = Ast.Type.Available (_, return);
              body = Ast.Function.BodyBlock (pred_loc, { Ast.Statement.Block.
                body = [_, Ast.Statement.Return { Ast.Statement.Return.
                  argument = Some e;
                  comments = _;
                }]
              });
              _;
            } ->
              let param_to_param_type = function
                | (loc, t), Ast.Pattern.Identifier { Ast.Pattern.Identifier.
                    name = (name_loc, _), name;
                    annot = Ast.Type.Available (_, annot);
                    optional;
                  } ->
                  loc,
                  { Ast.Type.Function.Param.name = Some ((name_loc, t), name); annot; optional; }
                | _ -> assert_false "Function declaration AST has unexpected shape"
              in
              let params = Core_list.map ~f:(fun (_, { Ast.Function.Param.argument; default }) ->
                if default <> None then
                  assert_false "Function declaration AST has unexpected shape";
                param_to_param_type argument
              ) params in
              let rest = Option.map
              ~f:(fun (rest_loc, { Ast.Function.RestParam.argument }) ->
                rest_loc, { Ast.Type.Function.RestParam.argument = param_to_param_type argument }
              ) rest in
              let annot : (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation =
                annot_loc, (
                  (func_annot_loc, fun_type),
                  Ast.Type.Function { Ast.Type.Function.
                    params = params_loc, { Ast.Type.Function.Params.params; rest; };
                    return;
                    tparams;
                  }
                )
              in
              { Ast.Statement.DeclareFunction.
                id = (id_loc, fun_type), id_name;
                annot;
                predicate = Some (pred_loc, Ast.Type.Predicate.Declared e)
              }
          | _ -> failwith "Internal error: malformed predicate declare function"
          )

      | _ ->
        None
      end
  | _ ->
      None

and check_default_pattern cx left right =
  let left_loc = fst left in
  let right_loc = fst right in

  let update_excuses update_fun =
    let exists_excuses = Context.exists_excuses cx in
    let exists_excuse = Loc_collections.ALocMap.get left_loc exists_excuses
      |> Option.value ~default:ExistsCheck.empty
      |> update_fun in
    let exists_excuses = Loc_collections.ALocMap.add left_loc exists_excuse exists_excuses in
    Context.set_exists_excuses cx exists_excuses
  in

  match snd right with
    | Ast.Expression.Literal literal ->
      let open ExistsCheck in
      begin match literal.Ast.Literal.value with
        | Ast.Literal.String "" ->
          update_excuses (fun excuse -> {excuse with string_loc = Some right_loc})
        | Ast.Literal.Boolean false ->
          update_excuses (fun excuse -> {excuse with bool_loc = Some right_loc})
        | Ast.Literal.Number 0. ->
          update_excuses (fun excuse -> {excuse with number_loc = Some right_loc})
        (* There's no valid default value for mixed to create an excuse. *)
        | _ -> ()
      end
    | _ -> ()

and post_assignment_havoc ~private_ name patt orig_t t =
  (* types involved in the assignment are computed
     in pre-havoc environment. it's the assignment itself
     which clears refis *)
  Env.havoc_heap_refinements_with_propname ~private_ name;

  (* add type refinement if LHS is a pattern we handle *)
  match Refinement.key_of_pattern patt with
  | Some key ->
    (* NOTE: currently, we allow property refinements to propagate
       even if they may turn out to be invalid w.r.t. the
       underlying object type. If invalid, of course, they produce
       errors, but in the future we may want to prevent the
       invalid types from flowing downstream as well.
       Doing so would require that we defer any subsequent flow
       calls that are sensitive to the refined type until the
       object and refinement types - `o` and `t` here - are
       fully resolved.
     *)
    ignore Env.(set_expr key (fst patt) t orig_t)
  | None ->
    ()

and mk_initial_arguments_reason = Ast.Expression.(function
| [] -> []
| Expression x :: args -> mk_expression_reason x :: mk_initial_arguments_reason args
| Spread _ :: _ -> []
)

and warn_or_ignore_optional_chaining optional cx loc =
  if optional
  then match Context.esproposal_optional_chaining cx with
  | Options.ESPROPOSAL_ENABLE
  | Options.ESPROPOSAL_IGNORE -> ()
  | Options.ESPROPOSAL_WARN -> Flow.add_output cx (Error_message.EExperimentalOptionalChaining loc)
  else ()

and warn_or_ignore_fsharp_pipeline_operator cx loc =
  match Context.esproposal_fsharp_pipeline_operator cx with
  | Options.ESPROPOSAL_ENABLE
  | Options.ESPROPOSAL_IGNORE -> ()
  | Options.ESPROPOSAL_WARN -> Flow.add_output cx (Error_message.EExperimentalFSharpPipelineOperator loc)
