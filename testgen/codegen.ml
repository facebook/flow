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

let move_func (prog : Syntax.t list) =
  let is_func s = match s with
      | Syntax.Stmt (S.FunctionDeclaration _) -> true
      | _ -> false in

  let all_func = List.filter is_func prog in
  let all_non_func = List.filter (fun p -> not (is_func p)) prog in
  all_func @ all_non_func

(* Main entry functions for generating code *)
let mk_code engine prog_num random =
  (if random
   then engine#gen_random_prog prog_num
   else engine#gen_prog prog_num)
  |> (List.map (fun (slist, env) ->
      (* We add type assertions at the end *)
      let prog = slist |> move_func in
      Printf.sprintf "%s\n" ((string_of_prog prog) ^ (Ruleset_base.str_of_env env))))
