(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is a module for generating code that only has object decl,
   property writes and property reads. The types for conditions and
   codes are still simple strings. This module is only used to
   demonstrate that the framework is working and we will extend the
   condition type or code type to have more complicated conditions
   such as type judgements *)

module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils

module Obj_rule = struct
  (* A condition is an assertion of an expression being of a type *)
  type cond_t = E.t' * T.t'

  type code_t = string

  type grammar_t = string

  type rule_t = {
    grammar: grammar_t;
    premises: cond_t list;
    cons: cond_t list;
  }

  let string_of_cond (cond : cond_t) : string =
    Utils.string_of_expr (Loc.none, fst cond) ^ " : " ^ Utils.string_of_type (Loc.none, snd cond)
    |> Str.global_replace (Str.regexp "\n") ""

  (* We don't do fix-point calculation yet *)
  let merge_cond (facts : cond_t list) (new_cond : cond_t list) : cond_t list = facts @ new_cond

  let mk_rule (grm : grammar_t) (pre : cond_t list) (post : cond_t list) : rule_t =
    { grammar = grm; premises = pre; cons = post }

  (* we simply check whether the state has the given condition. No *)
  (* inference or implication here *)
  let is_valid (facts : cond_t list) (cond : cond_t) : bool = List.mem cond facts

  (* Simply check whether the consequence of a rule has the given *)
  (* condition *)
  let cond_to_rule (all_rules : rule_t list) (cond : cond_t) : rule_t =
    List.filter (fun r -> List.mem cond r.cons) all_rules |> List.hd

  let exercise (rule : rule_t) : code_t * cond_t list = (rule.grammar, rule.cons)

  let combine_code (clist : code_t list) (new_code : code_t) : code_t =
    String.concat "\n" (clist @ [new_code])
end

(* This is the module used for generating code *)
module Gen = Gen_rule.Mk_Generator (Obj_rule)

(* functions for parsing type rules *)
module Type_parser = Type_parser.Type (Parser_flow.Parse)

let parse_helper f s =
  let env = Parser_env.init_env ~token_sink:None ~parse_options:None None s in
  let ((_, out), _) = Parser_flow.do_parse env f true in
  out

let parse_type_rule cons : Gen.cond_t =
  let (expr, etype) = cons in
  (parse_helper Parser_flow.Parse.expression expr, parse_helper Type_parser._type etype)

(* Make rules from strings *)
let mk_rule_from_string
    (grm : string) (pre : (string * string) list) (post : (string * string) list) : Gen.rule_t =
  let grammar = grm in
  let premises = Core_list.map ~f:parse_type_rule pre in
  let cons = Core_list.map ~f:parse_type_rule post in
  Obj_rule.{ grammar; premises; cons }

(* We set up three simple rules: object decl, prop read and prop write *)
let rules =
  [
    mk_rule_from_string "var o = {};" [] [("o", "{}")];
    mk_rule_from_string "o.p = 1;" [("o", "{}")] [("o", "{p : number}")];
    mk_rule_from_string "o.p;" [("o", "{p : number}")] [("o.p", "number")];
  ]

let main rule_index =
  let (code, facts) = Gen.gen_prog rules [] (List.nth rules rule_index) in
  Printf.printf "Exercising rule %d =========\n" rule_index;
  Printf.printf "Code ==============\n%s\n" code;
  Printf.printf "New conditions =============\n";
  List.iter (fun f -> print_string (Obj_rule.string_of_cond f ^ "\n")) facts;
  Printf.printf "\n"

;;
main 0

;;
main 1

;;
main 2
