(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This file defines the generation rule interface and the code
   generation functor. The goal here is to let users extend the
   generation rule interface where users can define what code to
   generate and during the code generation process what condition
   needs to satisfy.

   A good use case would be that users want to test Flow on object
   property read/write. Then the generator should produce code that
   type checks given users' own type rules.

   The generator can take a user-defined module that defines
   grammars for code generation and generation rules associated with
   each grammar. For example, a prop write grammar might look like
   this:

   <prop_write> :== o.p = 1

   And its associated rule might look like this:

   is_obj(o)       <- This is the premise of the rule
   ---------------
   prop(o.p)       <- This is the consequence of the rule

   With this, when the generator wants to generate property write, it
   will check whether it has all the premises satisfied. If not, it
   will first exercise the rules that satisfy the premises of property
   write and then exercise the rule for generating property write

   Ultimately, we want to generate code that satisfy three things:

   1. No syntax error
   2. No type error
   3. Has runtime error

   Before this week, everything was hard-coded and it would be hard to
   configure the generator without modifying the code. With this
   constraint-based code generator, users can easily extend the
   generator by writing a module and let it generate any code under
   some constraints. *)

(* Module type for code generation rules *)
module type Gen_rule_t = sig
  (* condition/constraint type *)
  type cond_t

  (* grammar type *)
  type grammar_t

  (* syntax element type. A syntax element is the thing that gets
     generated when we exercise the rule *)
  type code_t

  (* The rule itself *)
  type rule_t = {
    grammar: grammar_t;
    premises: cond_t list;
    cons: cond_t list;
  }

  (* Make a rule *)
  val mk_rule : grammar_t -> cond_t list -> cond_t list -> rule_t

  (* Given a set of conditions as facts, we test whether a new
     condition holds. *)
  val is_valid : cond_t list -> cond_t -> bool

  (* Get a rule out from all the rules based on a condition. The
     purpose of this function is to get a rule so that when we
     exercise the rule we will be satisfying the given condition. *)
  val cond_to_rule : rule_t list -> cond_t -> rule_t

  (* Exercise a rule and return the syntactic element and the
     consequent condition. *)
  val exercise : rule_t -> code_t * cond_t list

  (* Combine two syntactic elements together *)
  val combine_code : code_t list -> code_t -> code_t

  (* Merge a set of new conditions into a set of old conditions. We
     might need some inferences here to discover new conditions until we
     reach a fixed point *)
  val merge_cond : cond_t list -> cond_t list -> cond_t list
end

(* This functor makes a code generator with a given generation rule *)
(* module *)
module Mk_Generator (Gen_rule : Gen_rule_t) = struct
  (* Types for basic stuffs *)
  type cond_t = Gen_rule.cond_t

  type grammar_t = Gen_rule.grammar_t

  type code_t = Gen_rule.code_t

  type rule_t = Gen_rule.rule_t

  (* make a rule *)
  let mk_rule = Gen_rule.mk_rule

  (* This is the main entry function for generating a program. We need *)
  (* to give it all the rules it can use during code generation, a *)
  (* state containing all the conditions that holds already and the *)
  (* rule we want to execute. *)
  let rec gen_prog (all_rules : rule_t list) (state : cond_t list) (rule : rule_t) :
      code_t * cond_t list =
    (* get a list of premise conditions we need to satisfy *)
    let to_sat = List.filter (fun p -> not (Gen_rule.is_valid state p)) Gen_rule.(rule.premises) in
    (* all the code and conditions necessary to exercise the input
       rule *)
    let (pre_code, pre_cond) =
      if to_sat = [] then
        ([], [])
      else
        (* get their corresponding rules *)
        let new_rules =
          Core_list.map ~f:(fun cond -> Gen_rule.cond_to_rule all_rules cond) to_sat
        in
        (* exercise necessary rules *)
        let result =
          List.fold_left
            (fun acc r ->
              let (new_code, new_cond) = gen_prog all_rules (snd acc) r in
              (new_code :: fst acc, Gen_rule.merge_cond (snd acc) new_cond))
            ([], state)
            new_rules
        in
        (fst result |> List.rev, snd result)
    in
    (* We exercise the rule and return everything generated during
       this function call *)
    let (new_code, new_cond) = Gen_rule.exercise rule in
    (Gen_rule.combine_code pre_code new_code, pre_cond @ new_cond)
end
