(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils

(* ESSENTIAL: Syntax type and related functions *)
type t =
  | Expr of (Loc.t, Loc.t) E.t'
  | Stmt of (Loc.t, Loc.t) S.t'
  | Empty

let str_of_syntax (s : t) : string =
  match s with
  | Expr e -> Utils.string_of_expr e
  | Stmt s -> Utils.string_of_stmt s
  | Empty -> ""

(* Make a literal expression.*)
let rec mk_literal_expr (t : (Loc.t, Loc.t) T.t') : (Loc.t, Loc.t) E.t' =
  match t with
  | T.Number ->
    E.Literal
      Ast.Literal.{ value = Number 1.1; raw = "1.1"; comments = Flow_ast_utils.mk_comments_opt () }
  | T.String ->
    E.Literal
      Ast.Literal.
        { value = String "foo"; raw = "\"foo\""; comments = Flow_ast_utils.mk_comments_opt () }
  | T.Boolean ->
    E.Literal
      Ast.Literal.
        { value = Boolean false; raw = "false"; comments = Flow_ast_utils.mk_comments_opt () }
  | T.Union (t1, t2, rest) ->
    let elements =
      t1 :: t2 :: rest
      |> Core_list.map ~f:snd
      |> Core_list.map ~f:mk_literal_expr
      |> Core_list.map ~f:(fun e -> Some (E.Expression (Loc.none, e)))
    in
    E.Array.(E.Array { elements; comments = Flow_ast_utils.mk_comments_opt () })
  | T.Object obj_t -> mk_obj_literal_expr obj_t
  | T.StringLiteral lit ->
    let value = Ast.StringLiteral.(lit.value) in
    let raw = Ast.StringLiteral.(lit.raw) in
    E.Literal
      Ast.Literal.{ value = String value; raw; comments = Flow_ast_utils.mk_comments_opt () }
  | T.NumberLiteral lit ->
    let value = Ast.NumberLiteral.(lit.value) in
    let raw = Ast.NumberLiteral.(lit.raw) in
    E.Literal
      Ast.Literal.{ value = Number value; raw; comments = Flow_ast_utils.mk_comments_opt () }
  | T.BooleanLiteral value ->
    let raw =
      if value then
        "true"
      else
        "false"
    in
    E.Literal
      Ast.Literal.{ value = Boolean value; raw; comments = Flow_ast_utils.mk_comments_opt () }
  | T.Tuple tlist ->
    let elements =
      Core_list.map
        ~f:(fun (_, tt) ->
          let e = mk_literal_expr tt in
          Some (E.Expression (Loc.none, e)))
        tlist
    in
    E.Array.(E.Array { elements; comments = Flow_ast_utils.mk_comments_opt () })
  | T.Array t -> mk_literal_expr (T.Tuple [t; t; t; t; t])
  | _ ->
    E.Literal
      Ast.Literal.{ value = Null; raw = "null"; comments = Flow_ast_utils.mk_comments_opt () }

(* Make an object literal based on its type *)
and mk_obj_literal_expr (t : (Loc.t, Loc.t) T.Object.t) : (Loc.t, Loc.t) E.t' =
  let prop_init_list =
    Core_list.map
      ~f:(fun p ->
        T.Object.Property.(
          match p with
          | T.Object.Property
              ( _,
                {
                  key = k;
                  value = Init (_, ptype);
                  optional = o;
                  static = _;
                  proto = _;
                  _method = _;
                  variance = _;
                } ) ->
            (k, o, mk_literal_expr ptype)
          | _ -> failwith "Unsupported property"))
      T.Object.(t.properties)
    (* Randomly remove some optional properties *)
    (* |> List.filter (fun (_, o, _) -> (not o) || Random.bool ()) *)
    |> Core_list.map ~f:(fun (key, _, expr_t) ->
           E.Object.Property.(
             E.Object.Property
               (Loc.none, Init { key; value = (Loc.none, expr_t); shorthand = false })))
  in
  E.Object.(E.Object { properties = prop_init_list; comments = Flow_ast_utils.mk_comments_opt () })

(* Check the expression is of the given type *)
let mk_runtime_check (expr : (Loc.t, Loc.t) E.t') (etype : (Loc.t, Loc.t) T.t') : t =
  (* Make a variable decalration first *)
  let callee = E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, "assert_type")) in
  let arguments =
    [E.Expression (Loc.none, expr); E.Expression (Loc.none, mk_literal_expr etype)]
  in
  let call = E.Call.(E.Call { callee = (Loc.none, callee); targs = None; arguments }) in
  Stmt S.Expression.(S.Expression { expression = (Loc.none, call); directive = None })

(* Check the expression is of the given type *)
let mk_check_opt_prop (expr : (Loc.t, Loc.t) E.t') (etype : (Loc.t, Loc.t) T.t') : t =
  (* Make a variable decalration first *)
  let callee = E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, "check_opt_prop")) in
  let rec get_obj (read : (Loc.t, Loc.t) E.t') (acc : (Loc.t, Loc.t) E.t' list) =
    E.Member.(
      match read with
      | E.Member { _object = (_, obj); property = _ } -> get_obj obj (obj :: acc)
      | _ -> List.rev acc)
  in
  (* We want to make sure the parent is not undefined *)
  let parent_array =
    let elements =
      get_obj expr [] |> Core_list.map ~f:(fun e -> Some (E.Expression (Loc.none, e)))
    in
    E.Array.(E.Array { elements; comments = Flow_ast_utils.mk_comments_opt () })
  in
  let arguments =
    [ E.Expression (Loc.none, parent_array);
      E.Expression (Loc.none, expr);
      E.Expression (Loc.none, mk_literal_expr etype) ]
  in
  let call = E.Call.(E.Call { callee = (Loc.none, callee); targs = None; arguments }) in
  Stmt S.Expression.(S.Expression { expression = (Loc.none, call); directive = None })

(* ESSENTIAL: functions for making syntax *)
let mk_expr_stmt (expr : (Loc.t, Loc.t) E.t') : (Loc.t, Loc.t) S.t' =
  S.Expression.(S.Expression { expression = (Loc.none, expr); directive = None })

let mk_ret_stmt (expr : (Loc.t, Loc.t) E.t') : t =
  Stmt
    (S.Return
       { S.Return.argument = Some (Loc.none, expr); comments = Flow_ast_utils.mk_comments_opt () })

let mk_func_def
    (fname : string)
    (pname : string)
    (ptype : (Loc.t, Loc.t) T.t')
    (body : t list)
    (rtype : (Loc.t, Loc.t) T.t') : t =
  (* Add a runtime check for the parameter *)
  let body =
    body
    @
    match ptype with
    | T.Function _ -> []
    | _ ->
      [mk_runtime_check (E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, pname))) ptype]
  in
  let body =
    S.Block.(
      let stmt_list =
        List.fold_left
          (fun acc s ->
            match s with
            | Stmt st -> (Loc.none, st) :: acc
            | Expr e -> (Loc.none, mk_expr_stmt e) :: acc
            | Empty -> acc)
          []
          body
      in
      { body = stmt_list })
  in
  let param =
    ( Loc.none,
      {
        Ast.Function.Param.argument =
          ( Loc.none,
            P.Identifier
              {
                P.Identifier.name = Flow_ast_utils.ident_of_source (Loc.none, pname);
                annot = T.Available (Loc.none, (Loc.none, ptype));
                optional = false;
              } );
        default = None;
      } )
  in
  let func =
    Ast.Function.
      {
        id = Some (Flow_ast_utils.ident_of_source (Loc.none, fname));
        params = (Loc.none, { Params.params = [param]; rest = None });
        body = Ast.Function.BodyBlock (Loc.none, body);
        async = false;
        generator = false;
        predicate = None;
        return = T.Available (Loc.none, (Loc.none, rtype));
        tparams = None;
        sig_loc = Loc.none;
      }
  in
  Stmt (S.FunctionDeclaration func)

let mk_func_call (fid : (Loc.t, Loc.t) E.t') (param : (Loc.t, Loc.t) E.t') : t =
  Expr
    E.Call.(
      E.Call
        { callee = (Loc.none, fid); targs = None; arguments = [E.Expression (Loc.none, param)] })

let mk_literal (t : (Loc.t, Loc.t) T.t') : t =
  match t with
  | T.Number ->
    let lit =
      Ast.Literal.{ value = Number 1.1; raw = "1.1"; comments = Flow_ast_utils.mk_comments_opt () }
    in
    Expr (E.Literal lit)
  | T.String ->
    let lit =
      Ast.Literal.
        { value = String "foo"; raw = "\"foo\""; comments = Flow_ast_utils.mk_comments_opt () }
    in
    Expr (E.Literal lit)
  | T.Boolean ->
    let lit =
      Ast.Literal.
        { value = Boolean false; raw = "false"; comments = Flow_ast_utils.mk_comments_opt () }
    in
    Expr (E.Literal lit)
  | _ -> failwith "Unsupported"

let mk_prop_read (obj_name : string) (prop_name : string) : t =
  E.Member.(
    Expr
      (E.Member
         {
           _object = (Loc.none, E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, obj_name)));
           property = PropertyIdentifier (Flow_ast_utils.ident_of_source (Loc.none, prop_name));
         }))

let mk_prop_write (oname : string) (pname : string) (expr : (Loc.t, Loc.t) E.t') : t =
  let read =
    match mk_prop_read oname pname with
    | Expr e -> e
    | _ -> failwith "This has to be an expression"
  in
  let left = P.Expression (Loc.none, read) in
  let right = expr in
  let assign =
    E.Assignment.(
      E.Assignment { operator = None; left = (Loc.none, left); right = (Loc.none, right) })
  in
  Stmt (mk_expr_stmt assign)

let mk_vardecl ?etype (vname : string) (expr : (Loc.t, Loc.t) E.t') : t =
  (* Make an identifier *)
  let t =
    match etype with
    | None -> T.Missing Loc.none
    | Some t -> T.Available (Loc.none, (Loc.none, t))
  in
  let id =
    P.Identifier.
      ( Loc.none,
        P.Identifier
          { name = Flow_ast_utils.ident_of_source (Loc.none, vname); annot = t; optional = false }
      )
  in
  (* get the expression and its dependencies *)
  let init = expr in
  (* Make a var declaration *)
  let decl = S.VariableDeclaration.Declarator.(Loc.none, { id; init = Some (Loc.none, init) }) in
  let var_decl = S.VariableDeclaration.{ declarations = [decl]; kind = Var } in
  Stmt (S.VariableDeclaration var_decl)

let mk_obj_lit (plist : (string * ((Loc.t, Loc.t) E.t' * (Loc.t, Loc.t) T.t')) list) : t =
  let props =
    Core_list.map
      ~f:(fun p ->
        let pname = fst p in
        let expr = fst (snd p) in
        E.Object.Property.(
          E.Object.Property
            ( Loc.none,
              Init
                {
                  key = Identifier (Flow_ast_utils.ident_of_source (Loc.none, pname));
                  value = (Loc.none, expr);
                  shorthand = false;
                } )))
      plist
  in
  E.Object.(Expr (E.Object { properties = props; comments = Flow_ast_utils.mk_comments_opt () }))

let combine_syntax (prog : t list) : string =
  String.concat
    ""
    ( List.filter
        (fun c ->
          match c with
          | Stmt _ -> true
          | Expr (E.Call _) -> true
          | _ -> false)
        prog
    |> Core_list.map ~f:(fun c ->
           match c with
           | Empty -> failwith "This cannot be empty"
           | Stmt _ -> c
           | Expr e ->
             S.Expression.(Stmt (S.Expression { expression = (Loc.none, e); directive = None })))
    |> List.rev
    |> Core_list.map ~f:str_of_syntax )
