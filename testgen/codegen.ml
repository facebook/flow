(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module S = Ast.Statement;;
module E = Ast.Expression;;
module T = Ast.Type;;
module P = Ast.Pattern;;
module F = Ast.Function;;
module Utils = Flowtestgen_utils;;
module FRandom = Utils.FRandom;;
module Syntax = Syntax_base;;
module Config = Flowtestgen_config;;

let string_of_prog (prog : Syntax.t list) : string =
  String.concat
    ""
    ((List.filter (fun c -> match c with
         | Syntax.Stmt _ -> true
         | Syntax.Expr (E.Call _) -> true
         | _ -> false) prog)
     |> (List.map (fun c -> match c with
         | Syntax.Empty -> failwith "This cannot be empty"
         | Syntax.Stmt _ -> c
         | Syntax.Expr e ->
           let open S.Expression in
           Syntax.Stmt
             (S.Expression {expression = (Loc.none, e);
                            directive = None})))
     |> List.rev |> (List.map Syntax.str_of_syntax))

(* Check the expression is of the given type *)
let mk_type_assertion (vname : string) (expr : E.t') : Syntax.t =
  (* Make a variable decalration first *)
  let callee = E.Identifier (Loc.none, "assert_type") in
  let expected = expr in
  let arguments =
    [E.Expression (Loc.none, E.Identifier (Loc.none, vname));
     E.Expression (Loc.none, expected)] in
  let call = let open E.Call in
    E.Call {callee = (Loc.none, callee); arguments} in
  Syntax.Stmt (S.Expression.(S.Expression {expression = (Loc.none, call);
                                           directive = None}))

(* Make a literal expression.*)
let rec mk_literal_expr (t : T.t') : E.t' =
  match t with
  | T.Number ->
    E.Literal (Ast.Literal.({value = Number 1.1; raw = "1.1"}))
  | T.String ->
    E.Literal (Ast.Literal.({value = String "foo"; raw = "\"foo\""}))
  | T.Boolean ->
    E.Literal (Ast.Literal.({value = Boolean false; raw = "false"}))
  | T.Union (t1, t2, rest) ->
    let all_types = (t1 :: t2 :: rest) |> (List.map snd) in
    let t = FRandom.rchoice (Array.of_list all_types) in
    mk_literal_expr t
  | T.Object obj_t -> mk_obj_literal_expr obj_t
  | T.StringLiteral lit ->
    let value = T.StringLiteral.(lit.value) in
    let raw = T.StringLiteral.(lit.raw) in
    E.Literal (Ast.Literal.({value = String value; raw}))
  | T.NumberLiteral lit ->
    let value = T.NumberLiteral.(lit.value) in
    let raw = T.NumberLiteral.(lit.raw) in
    E.Literal (Ast.Literal.({value = Number value; raw}))
  | T.BooleanLiteral lit ->
    let value = T.BooleanLiteral.(lit.value) in
    let raw = T.BooleanLiteral.(lit.raw) in
    E.Literal (Ast.Literal.({value = Boolean value; raw}))
  | T.Tuple tlist ->
    let elements = List.map (fun (_, tt) ->
        let e = mk_literal_expr tt in
        Some (E.Expression (Loc.none, e))) tlist in
    E.Array.(E.Array {elements})
  | T.Array t -> mk_literal_expr (T.Tuple [t; t; t; t; t;])
  | _ ->
    E.Literal (Ast.Literal.({value = Null; raw = "null"}))

(* Make an object literal based on its type *)
and mk_obj_literal_expr (t : T.Object.t) : E.t' =
  let prop_init_list =
    List.map (fun p ->
        let open T.Object.Property in
        match p with
        | T.Object.Property (_, {key = k;
                                 value = Init (_, ptype);
                                 optional = o;
                                 static = _;
                                 _method = _;
                                 variance = _}) -> (k, o, mk_literal_expr ptype)
        | _ -> failwith "Unsupported property") T.Object.(t.properties)
    (* Randomly remove some optional properties *)
    (* |> List.filter (fun (_, o, _) -> (not o) || FRandom.rbool ()) *)
    |> List.map (fun (key, _, expr_t) ->
       let open E.Object.Property in
       E.Object.Property (Loc.none, {key;
                                     value = Init (Loc.none, expr_t);
                                     _method = false;
                                     shorthand = false})) in
  E.Object.(E.Object {properties = prop_init_list})

(* Add runtime check assertions at the end of a program.
   The idea here is that for each variable declarations, we
   get the variable name and its type. Then at the end
   of a program, we check whether the variable contains
   the values for its type and all the required properties.

   If an unsound type conversion is run followed by an mutation,
   the runtime check should fail.
*)
let add_assert (prog : Syntax.t list) =
  (* a function for finding variable decalrations *)
  let is_target (decl : S.VariableDeclaration.Declarator.t) =
    let open S.VariableDeclaration.Declarator in
    match decl with
    | _, {id = i;
          init = Some (_, init)} -> Some (i, init)
    | _ -> None in

  (* For each var decl, we get its var name and the expression for
     that type. *)
  List.map (fun p -> match p with
      | Syntax.Stmt s -> (match s with
          | S.VariableDeclaration decl ->
            let open S.VariableDeclaration in
            let open P.Identifier in
            (match is_target (List.hd decl.declarations) with
            | None -> None
            | Some ((_, P.Identifier {name = (_, id);
                                      typeAnnotation = Some (_, (_, t));
                                      optional = _}), _) -> Some (id, mk_literal_expr t)
                                      (*
                                      optional = _}), init) -> Some (id, init)
                                         *)
            | Some _ -> None)
          | _ -> None)
      | _ -> None) prog

  (* We are only interested in object var decl for now *)
  |> List.filter (fun t -> match t with
      | None -> false
      | Some (_, init) -> (match init with
          | E.Object _ -> true
          | _ -> false))
  (* Make type assertions *)
  |> List.map (fun s -> match s with
      | Some (vname, expr) -> mk_type_assertion vname expr
      | None -> failwith "Cannot be none")

(* Check parameter types at runtime.*)
let add_param_assert (prog : Syntax.t list) =
  let check_param (func : F.t) : F.t =
    let body = match F.(func.body) with
      | F.BodyBlock (_, b) -> S.Block.(b.body)
      | _ -> failwith "has to be a body block" in

    (* get the list of parameters *)
    let param_list =
      let open P.Identifier in
      List.fold_right (fun p acc -> match p with
          | _, P.Identifier {name = (_, n);
                             typeAnnotation = Some (_, (_, t));
                             optional = _} -> (n, t) :: acc
          | _ -> acc) (fst F.(func.params)) [] in

    (* construct the actual runtime checks *)
    let checks =
      List.map (fun (pname, ptype) ->
          let expr = mk_literal_expr ptype in
          mk_type_assertion pname expr) param_list
      |> List.map (fun syn -> match syn with
          | Syntax.Stmt s -> (Loc.none, s)
          | _ -> failwith "Cannot be expression") in
    let new_body = let open S.Block in {body = checks @ body} in

    let new_func = F.({id = func.id;
                       params = func.params;
                       body = F.BodyBlock (Loc.none, new_body);
                       async = func.async;
                       generator = func.generator;
                       predicate = func.predicate;
                       expression = func.expression;
                       returnType = func.returnType;
                       typeParameters = func.typeParameters}) in
    new_func in

  List.map (fun p -> match p with
      | Syntax.Stmt s -> (match s with
          | S.FunctionDeclaration f -> Syntax.Stmt (S.FunctionDeclaration (check_param f))
          | _ -> p)
      | _ -> p) prog

let move_func (prog : Syntax.t list) =
  let is_func s = match s with
      | Syntax.Stmt (S.FunctionDeclaration _) -> true
      | _ -> false in

  let all_func = List.filter is_func prog in
  let all_non_func = List.filter (fun p -> not (is_func p)) prog in
  all_func @ all_non_func

(* Main entry functions for generating random code *)
let mk_random_code rule_iter =
  (*  TODO:
      Pick the right engine based on the config. I can't get this to
      compile at this point and I'll fix the problem later.The config
      for engine has already be set up though.

  let engine = match Config.(config.engine) with
    | "depth" -> new Ruleset_depth.ruleset_depth 0
    | "func" -> new Ruleset_depth.ruleset_func 0
    | "optional" -> new Ruleset_depth.ruleset_optional 0
    | "union" -> new Ruleset_union.ruleset_union 0
    | "exact" -> new Ruleset_union.ruleset_exact 0
    | _ -> new Ruleset_base.ruleset_base 0 in
     *)

  let base_engine = new Ruleset_base.ruleset_base 0 in
  let depth_engine = new Ruleset_depth.ruleset_depth 0 in
  let func_engine = new Ruleset_func.ruleset_func 0 in
  let optional_engine = new Ruleset_optional.ruleset_optional 0 in
  let exact_engine = new Ruleset_exact.ruleset_exact 0 in
  let union_engine = new Ruleset_union.ruleset_union 0 in
  ignore base_engine;
  ignore depth_engine;
  ignore func_engine;
  ignore optional_engine;
  ignore exact_engine;
  let engine = union_engine in
  let prog, env = engine#gen_prog [] rule_iter in
  (* We add type assertions at the end *)
  let prog = ((add_assert prog) @ (prog |> add_param_assert))
           |> move_func in
  Printf.sprintf "%s\n" ((string_of_prog prog) ^ (Ruleset_base.str_of_env env))
