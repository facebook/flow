(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Patcher
open Hh_match_utils

module List = Core_list

(* ========================= TYPES =========================== *)

(* for processing php identifiers such that we can use them
   to denote regex special characters *)
type id_type =
  (* no special pattern commands, just a normal identifier to match *)
  | Normal of string
  (* not and or are modifiers to an identifier, so the
     identifier is still relevant *)
  | Not of string
  | Or of string

  (* Identifier is not relevant for these *)
  | Wildcard
  | WildcardName
  | DoNotCare
  | RegExp

  | SkipAnyTok
  | KStar

type metavar_tgt =
  | NodeList of ast_node list
  | Literal of string

type skipany_ast_node =
  | SkipanyBlock of block
  | SkipanyExpr of expr

(* Put patches in a set because it is common for many of the same
   patch to be put in the environment when patterns include SkipAny
   or KStar *)
module PatchSet =
  Set.Make(
      struct
        type t = patch
        let compare =
          (fun p1 p2 ->
           if p1.Hh_match_utils.start_loc <> p2.Hh_match_utils.start_loc
           then p1.Hh_match_utils.start_loc - p2.Hh_match_utils.start_loc
           else p1.Hh_match_utils.end_loc - p2.Hh_match_utils.end_loc)
      end)

module MetavarMap = Map.Make(String)

(* Encapsulated global state of the matcher which can
   keep track of metavariables used in matching *)
type matcher_env = {
    (* references to the text file, source*)
    file : Relative_path.t;
    source : string;
    (* things to accumulate as we match *)
    uses_regexp : bool;
    comments : (Pos.t * string) list;
    metavars : metavar_tgt MetavarMap.t;
    (* optional pieces related to patching *)
    transformations : patch_maps;
    patches : PatchSet.t;
  }

(* We don't use a set for result because only time we get duplicates
   is when merging results from different branches, and there will be
   distinct matches starting at the same position with nontrivial operations
   to check for equality. *)
type match_result =
  | Matches of (ast_node * File_pos.t) list
  | NoMatch

let dummy_success_res = Matches [(DummyNode, File_pos.dummy)]

(* for SkipAny matching *)
let star_stmt =
  Ast.Expr (Pos.none, (String (Pos.none, "__KSTAR")))

let is_skip_any_stmt = function
  | Ast.Expr (_, (String (_, "__DUMMYSKIPANY"))) -> true
  | _ -> false

(* ======================== HELPERS ======================== *)

(* should merge the two environments (mainly for correct remembering of
   metavariable bindings with .* type matches and patches that need to
   be applied). May result in binding multiple values to the same
   metavariable. *)
let merge_envs
      (env1 : matcher_env)
      (env2 : matcher_env) : matcher_env =
  { env1 with patches = PatchSet.union env1.patches env2.patches }

(* small helper to return the correct env depending on whether
   the ret passed in is a fail or not (when matches fail we should revert
   the env to what it was before the failed match to make sure it is correct) *)
let revert_env_if_no_match
      (ret : match_result * matcher_env)
      (old_env : matcher_env) : (match_result * matcher_env) =
  match fst ret with
  | NoMatch -> (NoMatch, old_env)
  | _ -> ret

let _contains_match = function
  | Matches _ -> true
  | _ -> false

(* For when we concatenate matches from SkipAny or KStar where there may be
   duplicates.
   TODO: implement some kind of duplicate-removal system that works;
         position-based deduplication does not work *)
let concat_match_results_nodup
      (results : match_result list) : match_result =
    List.fold_left
    ~f:(fun res_acc new_res ->
        match res_acc, new_res with
        | Matches matches_so_far , Matches new_matches ->
          Matches (List.rev_append new_matches matches_so_far)
        | Matches _, NoMatch -> res_acc
        | NoMatch, Matches  _ -> new_res
        | NoMatch, NoMatch -> NoMatch)
    ~init:NoMatch
    results

let concat_match_results (results : match_result list) : match_result =
  List.fold_left
    ~f:(fun res_acc new_res ->
        match res_acc, new_res with
        | Matches matches_so_far , Matches new_matches ->
          Matches (matches_so_far @ new_matches)
        | Matches _, NoMatch -> res_acc
        | NoMatch, Matches  _ -> new_res
        | NoMatch, NoMatch -> NoMatch)
    ~init:NoMatch
    results

let find_substring to_search substring =
  let re = Str.regexp_string substring in
  try Str.search_forward re to_search 0
  with Not_found -> -1

let remove_keyword str keyword keyword_start =
  let keylen = String.length keyword in
  let len = String.length str in
  (String.sub str 0 keyword_start) ^
    (String.sub str (keyword_start + keylen) (len - keyword_start - keylen))

(* Returns a string corresponding to the metavariable name
   if the identifier is a metavariable *)
let check_mvar (id : string) : string option =
  let found_meta_key = find_substring id "META" in
  if found_meta_key = -1
  then None
  else
    (* get everything after the "META" *)
    Some (String.sub
            id (found_meta_key + 4)
            (String.length id - (found_meta_key + 4)))

(* Instantiates an identifier metavariable (or returns the identifier if it
   is not a metavar*)
let instantiate_mvar_id (env : matcher_env) (id : string) : string =
  let mvar_id = check_mvar id in
  match mvar_id with
  | None -> id
  | Some mvar_name ->
     try
     match MetavarMap.find mvar_name env.metavars with
     | Literal str -> str
     | _ -> id
     with Not_found -> failwith ("metavar " ^ mvar_name ^ " not found")

let add_mvar_id
      (env : matcher_env)
      (id : string)
      (binding : string) : matcher_env =
  match check_mvar id with
  | None -> env
  | Some mvar ->
    if env.uses_regexp
    then failwith
      "Regular expressions and metavariables can't be used in the same pattern"
    else
     (* Add binding if one does not exist,
        WILL NOT OVERWRITE EXISTING BINDING *)
     { env with
       metavars =
         if MetavarMap.mem mvar env.metavars then env.metavars
         else MetavarMap.add mvar (Literal binding) env.metavars }

(* Expr is a metavariable if it matches "__.*META.*" (can contain other
  keywords, the part after the meta is the metavariable name) *)
let is_meta_expr = function
  | (_, (String (_, str))) -> check_mvar str
  | _ -> None

(* NOTE: it is fine that meta exprs and stmts are the same because we check
   stmt before expr.*)
let is_meta_stmt = function
  | Ast.Expr e -> is_meta_expr e
  | _ -> None

let add_mvar_node
      (type a)
      (env : matcher_env)
      (is_meta_fn : a -> string option)
      (node : a)
      (binding : ast_node) : matcher_env =
  match is_meta_fn node with
  | None -> env
  | Some mvar_name ->
     if MetavarMap.mem mvar_name env.metavars then env
     else { env with
            metavars =
              MetavarMap.add mvar_name (NodeList [binding]) env.metavars }

(* Override the ast_constructor's relevant methods to instantiate metavariables
   whenever one is found.
   NOTE: will instantiate ALL metavariables that are a children of the node that
         you start on *)
class metavar_instantiating_ctr (env : matcher_env) = object
  inherit [unit] AstConstructor.ast_constructor as super

  val mvar_map = env.metavars;
  val match_env = env;

  method! on_stmt env s = begin
      match is_meta_stmt s with
      | Some mvar_name -> begin
         try
         let mval = MetavarMap.find mvar_name mvar_map in
         match mval with
         | NodeList vals ->
            (* Verify that the list is homogenous, If it is,
               replace the statement with it *)
            if List.exists
                 ~f:(function
                     | Hh_match_utils.Stmt _ -> false
                     | _ -> true) vals
            then super#on_stmt env s
            else
              List.map
                ~f:(function
                    | Hh_match_utils.Stmt mvar_val -> mvar_val
                    | _ ->
                       failwith ("should never happen unless you" ^
                             " metavariables are broken")) vals
         | _ -> super#on_stmt env s
         with Not_found -> super#on_stmt env s end
      | _ -> super#on_stmt env s
    end

  method! on_expr env e = begin
      match is_meta_expr e with
      | Some mvar_name -> begin
         try
         let mval = MetavarMap.find mvar_name mvar_map in
         match mval with
         | NodeList vals ->
            if List.exists
                 ~f:(function
                     | Hh_match_utils.Expr _ -> false
                     | _ -> true) vals
            then super#on_expr env e
            else
              List.map
                ~f:(function
                    | Hh_match_utils.Expr mvar_val -> mvar_val
                    | _ -> failwith ("should never happen unless you" ^
                             " metavariables are broken")) vals
         | _ -> super#on_expr env e
         with Not_found -> super#on_expr env e end
      | _ -> super#on_expr env e
    end

  method! on_id env id = begin
      let id = super#on_id env id in
      fst id, instantiate_mvar_id match_env (snd id)
    end

  method! on_pstring env pstr = begin
      let pstr = super#on_pstring env pstr in
      fst pstr, instantiate_mvar_id match_env (snd pstr)
    end
end

(* turns a target node into text that can be put into a patch, instantiating
   any metavariables contained and unparsing the stmt *)
let stmt_to_text env stmts =
  (* instantiate relevant metavariables *)
  stmts |>
    List.map
      ~f:((new metavar_instantiating_ctr env)#on_stmt ()) |>
    List.concat |> Unparser.Unparse.u_naked_block |>
    Unparsed.to_string

(* see comment on stmt_to_text *)
let expr_to_text env exprs =
  (* instantiate relevant metavariables *)
  exprs |>
    List.map
      ~f:((new metavar_instantiating_ctr env)#on_expr ()) |>
    List.concat |>
    (fun expr_list -> Expr_list expr_list) |>
    Unparser.Unparse.u_expr_ |>
    Unparsed.to_string

let instantiate_mvar_stmt env stm =
  match is_meta_stmt stm with
  | None -> stm
  | Some _ ->
     AstConstructor.list_to_single
       (new metavar_instantiating_ctr env)#on_stmt () stm

let instantiate_mvar_expr env exp =
  match is_meta_expr exp with
  | None -> exp
  | Some _ ->
     AstConstructor.list_to_single
       (new metavar_instantiating_ctr env)#on_expr () exp

(* Will get all non-metavariable keywords *)
let process_identifier (id : string) : id_type =
  let not_keyword = "__NOT" in
  let not_action = function key -> Not (remove_keyword id not_keyword key) in
  let or_keyword = "__OR" in
  let or_action = function key -> Or (remove_keyword id or_keyword key) in
  let regexp_keyword = "__REGEXP" in
  let regexp_action = function _ -> RegExp in
  let wildcard_keyword = "__ANY" in
  let wildcard_action = function _ -> Wildcard in
  let wildcard_name_keyword = "__SOMENAME" in
  let wildcard_name_action = function _ -> WildcardName in
  let do_not_care_keyword = "__SOMENODE" in
  let do_not_care_action = function _ -> DoNotCare in
  let skip_any_keyword = "__SKIPANY" in
  let skip_any_action = function _ -> SkipAnyTok in
  let star_keyword = "__KSTAR" in
  let star_action = function _ -> KStar in
  let actions =
    [not_keyword, not_action; or_keyword, or_action;
     regexp_keyword, regexp_action;
     wildcard_keyword, wildcard_action;
     wildcard_name_keyword, wildcard_name_action;
     do_not_care_keyword, do_not_care_action;
     skip_any_keyword, skip_any_action;
     star_keyword, star_action] in
  let rec check_keywords = function
    (* default to normal identifier *)
    | [] -> Normal id
    | (keyword, action) :: tail ->
       let substr_pos = find_substring id keyword in
       if substr_pos <> -1
       then (action substr_pos)
       else check_keywords tail in
  check_keywords actions

(* for readability these are left *)
let get_id_type (id : (Pos.t * string)) : id_type =
  process_identifier (snd id)

let match_string
      (t_s : string)
      (p_s : string)
      (env : matcher_env) : (match_result * matcher_env) =
  let env' = add_mvar_id env p_s t_s in
  let p_s = instantiate_mvar_id env' p_s in
  let match_strings str1 str2 =
    let remove_leading_slash str =
      let len = String.length str in
      if len > 1 && str.[0] = '\\'
      then String.sub str 1 (len - 1)
      else str in
    (remove_leading_slash str1) = (remove_leading_slash str2) in
  let is_match =
    match process_identifier p_s with
    | Wildcard | WildcardName | KStar -> true
    | Or p_str | Normal p_str -> match_strings t_s p_str
    | Not p_str -> not (match_strings t_s p_str)
    (* this should be handled by match_regexp *)
    | RegExp -> failwith "Regular expressions should be handled in match_regexp"
    (* haven't implemented these things *)
    | _ -> false in
  if is_match
  then dummy_success_res, env'
  else NoMatch, env

let match_regexp
      (t_id : Pos.t * string)
      (p_id : Pos.t * string)
      (env : matcher_env) : (match_result * matcher_env) =
    let env' = {env with uses_regexp = true} in
    if not (MetavarMap.is_empty env.metavars)
    then failwith
      "Regular expressions and metavariables can't be used in the same pattern"
    else
      let p_line = Pos.line (fst p_id) in
      let is_same_line_as_pattern comment = (Pos.line (fst comment) = p_line) in
      let comment = List.find env.comments is_same_line_as_pattern in
      let remove_leading_slash str =
        let len = String.length str in
        if len > 1 && str.[0] = '\\'
        then String.sub str 1 (len - 1)
        else str in
      let text = remove_leading_slash (snd t_id) in
      match comment with
      | None -> NoMatch, env'
      | Some (_, comment) ->
        if Str.string_match (Str.regexp comment) text 0
        then dummy_success_res, env'
        else NoMatch, env'


(* for use in matching lists and readibility in matching functions
   has a similar signature to other match methods *)
let match_id_res
      (t_id : Pos.t * string)
      (p_id : Pos.t * string)
      (env : matcher_env) : (match_result * matcher_env) =
  let res, env' =
    match process_identifier (snd p_id) with
    | RegExp -> match_regexp t_id p_id env
    | _ -> match_string (snd t_id) (snd p_id) env
  in
  match res with
  | Matches _ -> (Matches [(DummyNode, Pos.pos_start (fst t_id))], env')
  | NoMatch -> NoMatch, env

(* acc is the match results from the child node *)
let update_res_with
      (res : match_result)
      (node : ast_node)
      (node_pos : File_pos.t) : match_result =
  match res with
  | Matches so_far -> Matches ((node, node_pos) :: so_far)
  | NoMatch -> Matches [node, node_pos]

(* for finding the __SKIPANY string in the file *)
let is_skip_any_unproc (elem : stmt) =
  match elem with
  | Ast.Expr (_,(String pstr)) -> get_id_type pstr = SkipAnyTok
  | _ -> false

(* list of stmts that correspond to blocks *)
type block_accum = block list

(* return of on_block will be an accumulator of all the blocks in reverse
   order of how they were found (ending with the block given to it).
   i.e. order is last :: 2nd last :: ... :: block given *)
class block_finding_visitor () =
object
  inherit [block_accum] Ast_visitor.ast_visitor as super

  method! on_block acc block =
    begin
      let acc = block :: acc in
      super#on_block acc block;
    end
end

(* used to get (in reverse order) all expressions in a file *)
class expr_finding_visitor () =
object
  inherit [expr list] Ast_visitor.ast_visitor as super

  method! on_expr acc exp =
    begin
      let acc = exp :: acc in
      super#on_expr acc exp;
    end
end

(* these functions must be in this module because I am passing them arguments
   of multiple different types (match list is used on def list, stmt list etc)*)
module LM =
  struct
    (* TODO: implement disjunction matching correctly - compare with all
             possible elements of the disjunction, if any of them match,
             count as match. Simple, but requires preprocessing of the
             input string for disjunction handling (or just a special case
             that continues to consume until you are out of disjunction cases *)
    (* Try to match a list of ast nodes. If paired with the correct handle_sa_fn
       and handle_sa_hd_fn will also match SkipAny statements. *)
    let match_list_with_skips (type a)
          (is_star_fn : a -> bool)
          (* for stmts, exprs for being able to make mvars out of
             lists of nodes *)
          (is_meta_fn : (a -> string option) option)
          (to_node_fn : (a -> ast_node) option)
          (match_elem_fn : a -> a -> matcher_env -> match_result * matcher_env)
          (* handles checking for and preprocessing SkipAny tokens as well as
             matching processed __SKIPANYDUMMY tokens when they are removed from
             the pattern list *)
          (handle_sa_fn :
             (a list -> a list -> a list -> matcher_env ->
              match_result * matcher_env) ->
             a list -> a list -> a list -> matcher_env ->
             match_result * matcher_env)
          (* handles the case when our pattern starts with a SkipAny, like the
             case where our pattern starts with a KStar. None means the pattern
             is not a SkipAny, Some NoMatch means the pattern was a SkipAny that
             failed to match *)
          (handle_sa_hd_fn :
             (a list -> a list -> a list -> matcher_env ->
              match_result * matcher_env) ->
             a -> a list -> a list -> a list -> matcher_env ->
             (match_result * matcher_env) option)
          (t_list : a list)
          (p_list : a list)
          (env0 : matcher_env) : match_result * matcher_env =
      (* for case where we have some of both text and pattern left
         e.g. text = [stmt, ...] pattern = [stmt, ...]*)
      let rec try_match_heads
                (t_hd : a)
                (t_tl : a list)
                (p_hd : a)
                (p_tl : a list)
                (skipped_text : a list)
                (env : matcher_env) :
                match_result * matcher_env =
        (* If the pattern starts with a SkipAny, we want to handle that. *)
        let skipany_ret =
          handle_sa_hd_fn
            try_match_lists p_hd t_tl p_tl (t_hd :: skipped_text) env in
        match skipany_ret with
        (* if we handled a SkipAny, we want to pass that result back, it has
           already matched all the rest of the pattern (including the part
           after the SkipAny *)
        | Some ret -> ret
        (* Otherwise that call did nothing and we need to continue matching *)
        | None ->
           (* check for pattern, text match *)
           let (elt_match, env') = match_elem_fn t_hd p_hd env in
           if elt_match = NoMatch
           (* e.g. text = [stmt1, ...] pattern = [stmt2, ...] *)
           then NoMatch, env
           else
             let res, env' =
               handle_sa_fn try_match_lists t_tl p_tl skipped_text env' in
             match res with
             (* first elements don't match:
                  e.g. text = [stmt1, ...] pattern = [stmt2, ...] *)
             | NoMatch -> NoMatch, env
             (* first elements match:
                  e.g. text = [stmt1, ...] pattern = [stmt1, ...] *)
             | Matches _ ->
                concat_match_results_nodup [elt_match; res], env'

      (* ========== functions for if a kleene star is encountered ============*)
      (* consume all leading KStars from the pattern *)
      and consume_stars (p_list : a list) : a list * string option =
        match p_list with
        | hd :: tl when is_star_fn hd ->
           let res = consume_stars tl in
           let meta_keyword = match snd res with
             | Some key -> Some key
             | None ->
                match is_meta_fn with
                | Some is_meta_fn -> is_meta_fn hd
                | None -> None in
           fst res, meta_keyword
        | _ -> p_list, None

      (* try the rest of the pattern based off all possible amounts of text the
         kleene star could consume by recursing with it removing 0, 1, 2 ...
         elements from the text *)
      and handle_star_helper
            (t_list : a list)
            (p_list : a list)
            (skipped_text : a list)
            (env : matcher_env)
            (meta_key : string option) :
            match_result * matcher_env =
        let add_mvar (env : matcher_env) : matcher_env =
          match meta_key, to_node_fn with
          | Some key, Some to_node_fn ->
             if MetavarMap.mem key env.metavars then env
             else
               let mapped_nodes = List.map ~f:to_node_fn skipped_text in
               { env with
                 metavars =
                   MetavarMap.add
                     key (NodeList (List.rev mapped_nodes)) env.metavars }
          | _, _ -> env in
        match t_list with
        | [] ->
           revert_env_if_no_match
             (try_match_lists [] p_list skipped_text (add_mvar env)) env
        | t_hd :: t_tl ->
           let rec_res, rec_env =
             handle_star_helper
               t_tl p_list (t_hd :: skipped_text) env meta_key in
           let norec_res, norec_env =
             try_match_lists t_list p_list skipped_text (add_mvar env) in
           match rec_res, norec_res with
           | NoMatch, NoMatch -> NoMatch, env
           | Matches _, NoMatch -> rec_res, rec_env
           | NoMatch, Matches _ -> norec_res, norec_env
           | Matches _, Matches _ ->
              (concat_match_results_nodup [rec_res; norec_res],
               merge_envs rec_env norec_env)

      (* consumes the leading KStars from the pattern then tries to do matching,
         returning the results of all successful matches with different amounts
         of text consumed by the KStar *)
      and handle_star_match
            (t_list : a list)
            (p_list : a list)
            (_skipped_text : a list)
            (env : matcher_env) :
            match_result * matcher_env =
        let p_list, meta_key = consume_stars p_list in
        handle_star_helper t_list p_list [] env meta_key

      (*========================= end kstar fns ==========================*)

      (* handles trying to match the lists that are our pattern and text
         Used for base cases to terminate the recursion when we can determine
         from list structure if a match cannot be made. Otherwise calls the
         correct function to match the lists *)
      and try_match_lists
            (t_list : a list)
            (p_list : a list)
            (skipped_text : a list)
            (env : matcher_env) :
            match_result * matcher_env =
        match t_list, p_list with
        (* Pattern starst with a KStar
           e.g. text = [] pattern = [".*", ...] *)
        | _, p_hd :: _ when is_star_fn p_hd ->
           handle_star_match t_list p_list [] env
        (* we're out of text, pattern must be empty *)
        | [], [] -> dummy_success_res, env
        (* e.g. text = [] pattern = [stmt1, ...] *)
        | [], _ -> NoMatch, env
        (* more text, pattern must match *)
        (* e.g. text = [stmt1, ...] pattern = [] *)
        | _t_hd :: _t_tl, [] -> NoMatch, env
        (* normal case of having both text, pattern left *)
        | t_hd :: t_tl, p_hd :: p_tl ->
           revert_env_if_no_match
             (try_match_heads t_hd t_tl p_hd p_tl skipped_text env) env in
      revert_env_if_no_match
        (handle_sa_fn try_match_lists t_list p_list [] env0) env0

    (* these two functions are used for normal list matching that does not have
       to handle SkipAny tokens in the pattern *)
    let dummy_act_if_skipany
          (_try_match_star_fn :
             'a list -> 'a list -> 'a list -> matcher_env ->
             match_result * matcher_env)
          (_p_hd : 'a)
          (_t_tl : 'a list)
          (_p_tl : 'a list)
          (_skipped_text : 'a list)
          (_env : matcher_env) : (match_result * matcher_env) option =
      None

    let try_match_dummy
          (try_match_star_fn :
             'a list -> 'a list -> 'a list -> matcher_env ->
             match_result * matcher_env)
          (t_list : 'a list)
          (p_list : 'a list)
          (skipped_text : 'a list)
          (env : matcher_env) :
          match_result * matcher_env =
      try_match_star_fn t_list p_list skipped_text env

    (* normal list matching (not handling SkipAny cases *)
    let match_list (type a)
          (is_star_fn : a -> bool)
          (* for stmts, exprs for being able to make mvars out of
             lists of nodes *)
          ?(is_meta_fn = None)
          ?(to_node_fn: (a -> ast_node) option = None)
          (match_elem_fn : a -> a -> matcher_env -> match_result * matcher_env)
          (t_list : a list)
          (p_list : a list)
          (env0 : matcher_env) : match_result * matcher_env =
      match_list_with_skips
        is_star_fn
        is_meta_fn
        to_node_fn
        match_elem_fn
        try_match_dummy
        dummy_act_if_skipany
        t_list
        p_list
        env0

    let match_option
          (match_fn : 'a -> 'a -> matcher_env -> (match_result * matcher_env))
          (t_opt : 'a option)
          (p_opt : 'a option)
          (env : matcher_env) :
          (match_result * matcher_env) =
      match p_opt, t_opt with
      | Some p_opt, Some t_opt ->
         match_fn t_opt p_opt env
      | None, Some _
      | Some _, None -> (dummy_success_res, env)
      | None, None ->
         dummy_success_res, env

    (* because lists of pairs occur fairly often *)
    let match_pair_fn
          (match_fn1 : 'a -> 'a -> matcher_env -> (match_result * matcher_env))
          (match_fn2 : 'b -> 'b -> matcher_env -> (match_result * matcher_env)):
          ('a * 'b -> 'a * 'b -> matcher_env -> match_result * matcher_env) =
      (fun (t_vals : 'a * 'b) (p_vals : 'a * 'b) (env : matcher_env) ->
       let old_env = env in
       let t_v1, t_v2 = t_vals in
       let p_v1, p_v2 = p_vals in
       let v1_res, env = match_fn1 t_v1 p_v1 env in
       match v1_res with
       | NoMatch -> NoMatch, old_env
       | Matches _ ->
          let v2_res, env = match_fn2 t_v2 p_v2 env in
          match v2_res with
          | NoMatch -> NoMatch, old_env
          | Matches _ ->
             (concat_match_results [v1_res; v2_res]), env)

    (* matches all the different attributes of a node given a list, short
       circuits and returns NoMatch and the original environment if any of
       the matches fail *)
    let rec match_attributes
              (actions : (matcher_env -> match_result * matcher_env) list)
              (env0 : matcher_env) : match_result * matcher_env =
      match actions with
      | [] -> dummy_success_res, env0
      | action :: tl ->
         let res, env = action env0 in
         match res with
         | NoMatch -> NoMatch, env0
         | Matches _ ->
            let tl_res, env = match_attributes tl env in
            match tl_res with
            | NoMatch -> NoMatch, env0
            | Matches _ ->
               (concat_match_results [res; tl_res]), env

    (* checks if a patch should be created based on the env (transformation
       list), the pattern node and creates it if necessary *)
    let patch_if_necc
          (type a)
          (text_elem : a)
          (pat_elem : a)
          (adjust_fn : string -> int -> int -> int * int)
          (delete_list : a list)
          (* assoc list from pattern -> target *)
          (transf_list : (a * a list) list)
          (node_to_txt_fn : matcher_env -> a list -> string)
          (extent_find_fn : a -> File_pos.t * File_pos.t)
          (ret : match_result * matcher_env) : (match_result * matcher_env) =
      let res, env = ret in
      let create_patch result_str =
        let elem_ext = extent_find_fn text_elem in
        let newpatch =
          { start_loc = File_pos.offset (fst elem_ext);
            end_loc = File_pos.offset (snd elem_ext);
            result_str;
            range_adjustment_fn = adjust_fn } in
        res, { env with patches = PatchSet.add newpatch env.patches } in
      (* check if the element is in the list of nodes to be deleted*)
      if List.exists ~f:(fun to_del -> pat_elem == to_del) delete_list
      then
        create_patch ""
      (* check if we have a relevant transformation*)
      else
        try
          let tgt_node = List.assq pat_elem transf_list in
          create_patch (node_to_txt_fn env tgt_node)
        with Not_found -> ret
  end

(* =================== MATCHING FUNCTIONS ==================== *)
(* All match functions take 2 Ast nodes of the same type and try to match them.
   The matching is sensitive to wildcards and will recursively try to match
   child nodes on the ast.

   First it checks for wildcards, metavariables and other special cases that
   allow us to do regex-type matching.
   Then it matches by type: e.g. if we have two def nodes they should both be
   type Fun_ or both type Class_ etc.
   Then it checks if all attributes of each node are the same, calling the
   proper match methods if necessary. If any of these matches for its children
   fail, then the match returns failure.
   e.g. when matching a fun_ we check the name then match the parameters, if
   those succeed then we try matching the body etc.

   The result returned is the result which is a list of matches containing the
   node that was matched and the position of the beginning of the element
   matched. If part of the text was matched with a kleene star, no matches
   will be added to the list. (A dummy success value may be returned if
   necessary). The modified matching environemnt (or the original environment
   if it wasn't modified or the match failed) *)
let rec match_ast_nodes
      (text_node : ast_node)
      (pattern_node : ast_node)
      (env : matcher_env) :
          (match_result * matcher_env) =
  match text_node, pattern_node with
  | Program t_prog, Program p_prog ->
     match_program t_prog p_prog env
  | Def t_def, Def p_def ->
     match_def t_def p_def env
  | Fun_ t_fun_, Fun_ p_fun_->
     match_fun_ t_fun_ p_fun_ env
  | Tparam t_tparam, Tparam p_tparam ->
     match_tparam t_tparam p_tparam env
  | Fun_param t_fun_param, Fun_param p_fun_param ->
     match_fun_param t_fun_param p_fun_param env
  | Class_ t_class_, Class_ p_class_ ->
     match_class_ t_class_ p_class_ env
  | Class_elt t_class_elt, Class_elt p_class_elt ->
     match_class_elt t_class_elt p_class_elt env
  | Hh_match_utils.Method t_method_, Hh_match_utils.Method p_method_ ->
     match_method_ t_method_ p_method_ env
  | Hh_match_utils.Stmt t_stmt, Hh_match_utils.Stmt p_stmt ->
     match_stmt t_stmt p_stmt env
  | Hh_match_utils.Expr t_expr, Hh_match_utils.Expr p_expr ->
     match_expr t_expr p_expr env
  | DummyNode, _ | _, DummyNode | _, Nodes _ | Nodes _, _ ->
     failwith "cannot match a non-AST construct"
  | _, _ -> (NoMatch, env)

and match_program
      (t_program : program)
      (p_program : program)
      (env : matcher_env) : (match_result * matcher_env) =
  (* TODO add ability for def-lv skip_any *)
  LM.match_list is_star_def match_def t_program p_program env

and match_tconstraint
      (t_tconstraint : tconstraint)
      (p_tconstraint : tconstraint)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_option match_hint t_tconstraint p_tconstraint env

and match_typedef_kind
      (t_typedef_kind : typedef_kind)
      (p_typedef_kind : typedef_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_typedef_kind, p_typedef_kind with
  | Alias t_h, Alias p_h
  | NewType t_h, NewType p_h -> match_hint t_h p_h env
  | _, _ -> NoMatch, env

and match_expr_list el1 el2 env =
  LM.match_list
    is_star_expr
    match_expr
    ~is_meta_fn:(Some is_meta_expr)
    ~to_node_fn:(Some (fun exp -> Hh_match_utils.Expr exp))
    el1 el2 env

and match_user_attribute
      (t_user_attribute : user_attribute)
      (p_user_attribute : user_attribute)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_id_res t_user_attribute.ua_name p_user_attribute.ua_name;
     match_expr_list
       t_user_attribute.ua_params
       p_user_attribute.ua_params]
    env

(* TODO maybe allow wildcarding namespaces *)
(* ignores ns_uses because that doesn't seem relevant for the matcher*)
and match_namespace
      (t_namespace : Namespace_env.env)
      (p_namespace : Namespace_env.env)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_namespace.Namespace_env.ns_name,
        p_namespace.Namespace_env.ns_name with
  | Some t_name, Some p_name when t_name = p_name -> dummy_success_res, env
  | None, None -> dummy_success_res, env
  | _, _ -> NoMatch, env

and match_typedef
      (t_typedef : typedef)
      (p_typedef : typedef)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_id_res t_typedef.t_id p_typedef.t_id;
     LM.match_list
       is_star_tparam
       match_tparam
       t_typedef.t_tparams
       p_typedef.t_tparams;
     match_tconstraint t_typedef.t_constraint p_typedef.t_constraint;
     match_typedef_kind t_typedef.t_kind p_typedef.t_kind;
     LM.match_list
       is_star_user_attribute
       match_user_attribute
       t_typedef.t_user_attributes
       p_typedef.t_user_attributes;
     match_namespace t_typedef.t_namespace p_typedef.t_namespace]
    env

and match_cst_kind
      (t_cst_kind : cst_kind)
      (p_cst_kind : cst_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_cst_kind = p_cst_kind
  then dummy_success_res, env
  else NoMatch, env

and match_gconst
      (t_gconst : gconst)
      (p_gconst : gconst)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_cst_kind t_gconst.cst_kind p_gconst.cst_kind;
     match_id_res t_gconst.cst_name p_gconst.cst_name;
     LM.match_option match_hint t_gconst.cst_type p_gconst.cst_type;
     match_expr t_gconst.cst_value p_gconst.cst_value;
     match_namespace t_gconst.cst_namespace p_gconst.cst_namespace]
    env

and match_def
      (t_def : def)
      (p_def : def)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_def, p_def with
  | Fun t_fun_, Fun p_fun_ ->
     match_fun_ t_fun_ p_fun_ env
  | Class t_class_, Class p_class_ ->
     match_class_ t_class_ p_class_ env
  | Ast.Stmt t_stmt, Ast.Stmt p_stmt->
     match_stmt t_stmt p_stmt env
  | Typedef t_typedef, Typedef p_typedef ->
     match_typedef t_typedef p_typedef env
  | Constant t_gconst, Constant p_gconst ->
     match_gconst t_gconst p_gconst env
  | Namespace (t_id, t_program), Namespace (p_id, p_program) ->
     LM.match_attributes
       [match_id_res t_id p_id;
        match_program t_program p_program]
       env
  (* I spent the last 10 minutes trying to figure out WTF this matcher is doing
     and perhaps how to even just strip the namespace kind, but it doesn't
     typecheck and I don't care. I don't think anyone is even using this
     code. If you care, you can fix it. *)
  (*
  | NamespaceUse t_iil, NamespaceUse p_iil ->
     revert_env_if_no_match
       (LM.match_list
          (fun _ -> false)
          (LM.match_pair_fn match_id_res match_id_res)
          t_iil
          p_iil
          env)
       env
  *)
  | _, _ -> NoMatch, env

and match_bool
      (t_b : bool)
      (p_b : bool)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_b == p_b
  then dummy_success_res, env
  else NoMatch, env

(* ignores f_mode, f_mtime *)
and match_fun_
      (t_fun_ : fun_)
      (p_fun_ : fun_)
      (env : matcher_env) : (match_result * matcher_env) =
  let attributes =
    [match_id_res t_fun_.f_name p_fun_.f_name;
     LM.match_list
       is_star_tparam
       match_tparam
       t_fun_.f_tparams
       p_fun_.f_tparams;
     LM.match_option match_hint t_fun_.f_ret p_fun_.f_ret;
     match_bool t_fun_.f_ret_by_ref p_fun_.f_ret_by_ref;
     LM.match_list
       is_star_fun_param
       match_fun_param
       t_fun_.f_params
       p_fun_.f_params;
     LM.match_list
       is_star_user_attribute
       match_user_attribute
       t_fun_.f_user_attributes
       p_fun_.f_user_attributes;
     match_fun_kind t_fun_.f_fun_kind p_fun_.f_fun_kind;
     match_namespace t_fun_.f_namespace p_fun_.f_namespace;
     match_stmt (Block t_fun_.f_body) (Block p_fun_.f_body)] in
  match get_id_type t_fun_.f_name with
  | Wildcard -> dummy_success_res, env
  | DoNotCare -> match_stmt (Block t_fun_.f_body) (Block p_fun_.f_body) env
  | _ -> LM.match_attributes attributes env

and match_fun_kind
      (t_fun_kind : fun_kind)
      (p_fun_kind : fun_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_fun_kind = p_fun_kind
  then dummy_success_res, env
  else NoMatch, env

and match_variance
      (t_variance : variance)
      (p_variance : variance)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_variance = p_variance
  then dummy_success_res, env
  else NoMatch, env

and match_constraint_kind
      (t_ck : constraint_kind)
      (p_ck : constraint_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_ck = p_ck
  then dummy_success_res, env
  else NoMatch, env

and match_tparam
      (t_tparam : tparam)
      (p_tparam : tparam)
      (env : matcher_env) : (match_result * matcher_env) =
  let (t_variance, t_id, t_chopt) = t_tparam in
  let (p_variance, p_id, p_chopt) = p_tparam in
  LM.match_attributes
    [match_variance t_variance p_variance;
     match_id_res t_id p_id;
     LM.match_option
       (LM.match_pair_fn match_constraint_kind match_hint)
       t_chopt
       p_chopt]
    env

and match_kind
      (t_kind : kind)
      (p_kind : kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_kind = p_kind
  then dummy_success_res, env
  else NoMatch, env

and match_fun_param
      (t_fun_param : fun_param)
      (p_fun_param : fun_param)
      (env : matcher_env) : (match_result * matcher_env) =
  let attributes =
    [match_id_res t_fun_param.param_id p_fun_param.param_id;
       LM.match_option
         match_hint
         t_fun_param.param_hint
         p_fun_param.param_hint;
       match_bool t_fun_param.param_is_reference p_fun_param.param_is_reference;
       match_bool t_fun_param.param_is_variadic p_fun_param.param_is_variadic;
       LM.match_option
         match_expr
         t_fun_param.param_expr
         p_fun_param.param_expr;
       LM.match_option
         match_kind
         t_fun_param.param_modifier
         p_fun_param.param_modifier;
       LM.match_list
         is_star_user_attribute
         match_user_attribute
         t_fun_param.param_user_attributes
         p_fun_param.param_user_attributes] in
  match get_id_type p_fun_param.param_id with
  | DoNotCare | Wildcard -> dummy_success_res, env
  | _ -> LM.match_attributes attributes env

and match_class_kind
      (t_class_kind : class_kind)
      (p_class_kind : class_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_class_kind = p_class_kind
  then dummy_success_res, env
  else NoMatch, env

and match_enum_
      (t_enum_ : enum_)
      (p_enum_ : enum_)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_hint t_enum_.e_base p_enum_.e_base;
     LM.match_option match_hint t_enum_.e_constraint p_enum_.e_constraint]
    env

and match_class_
      (t_class_ : class_)
      (p_class_ : class_)
      (env0 : matcher_env) : (match_result * matcher_env) =
  let actions =
    [match_id_res t_class_.c_name p_class_.c_name;
     match_bool t_class_.c_final p_class_.c_final;
     match_bool t_class_.c_is_xhp p_class_.c_is_xhp;
     LM.match_list
       is_star_user_attribute
       match_user_attribute
       t_class_.c_user_attributes
       p_class_.c_user_attributes;
     match_class_kind t_class_.c_kind p_class_.c_kind;
     LM.match_list
       is_star_tparam
       match_tparam
       t_class_.c_tparams
       p_class_.c_tparams;
     LM.match_list
       is_star_hint
       match_hint
       t_class_.c_extends
       p_class_.c_extends;
     LM.match_list
       is_star_hint
       match_hint
       t_class_.c_implements
       p_class_.c_implements;
     match_namespace t_class_.c_namespace p_class_.c_namespace;
     LM.match_option match_enum_ t_class_.c_enum p_class_.c_enum;
     LM.match_list
       is_star_celt
       match_class_elt
       t_class_.c_body
       p_class_.c_body] in
  match get_id_type p_class_.c_name with
  | Wildcard -> dummy_success_res, env0
  | DoNotCare -> LM.match_list
                   is_star_celt
                   match_class_elt
                   t_class_.c_body
                   p_class_.c_body
                   env0
  | _ -> LM.match_attributes actions env0

and match_ca_type
      (t_cat : ca_type)
      (p_cat : ca_type)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_cat, p_cat with
  | CA_hint t_h, CA_hint p_h ->
     match_hint t_h p_h env
  | CA_enum t_sl, CA_enum p_sl ->
      revert_env_if_no_match
       (LM.match_list (fun _ -> false) match_string t_sl p_sl env)
       env
  | _, _ -> NoMatch, env

and match_ca_field
      (t_caf : ca_field)
      (p_caf : ca_field)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_ca_type t_caf.ca_type p_caf.ca_type;
     match_id_res t_caf.ca_id p_caf.ca_id;
     LM.match_option match_expr t_caf.ca_value p_caf.ca_value;
     match_bool t_caf.ca_required p_caf.ca_required]
    env

and match_typeconst
      (t_tc : typeconst)
      (p_tc : typeconst)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_id_res t_tc.tconst_name p_tc.tconst_name;
     match_bool t_tc.tconst_abstract p_tc.tconst_abstract;
     LM.match_option
       match_hint
       t_tc.tconst_constraint
       p_tc.tconst_constraint;
     LM.match_option match_hint t_tc.tconst_type p_tc.tconst_type]
    env

and match_class_attr
      (t_class_attr : class_attr)
      (p_class_attr : class_attr)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_class_attr, p_class_attr with
  | CA_name t_id, CA_name p_id ->
     match_id_res t_id p_id env
  | CA_field t_cf, CA_field p_cf ->
     match_ca_field t_cf p_cf env
  | _, _ -> NoMatch, env

and match_trait_req_kind
      (t_trk : trait_req_kind)
      (p_trk : trait_req_kind)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_trk = p_trk
  then dummy_success_res, env
  else NoMatch, env

and match_class_var
      (t_cv : class_var)
      (p_cv : class_var)
      (env : matcher_env) : (match_result * matcher_env) =
  LM.match_attributes
    [match_id_res (fst t_cv) (fst p_cv);
     LM.match_option match_expr (snd t_cv) (snd p_cv)]
    env

and match_class_elt
      (t_class_elt : class_elt)
      (p_class_elt : class_elt)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_class_elt, p_class_elt with
  | Ast.Method t_method_, Ast.Method p_method_ ->
     match_method_ t_method_ p_method_ env
  | Const (t_hopt, t_iel), Const (p_hopt, p_iel) ->
     LM.match_attributes
       [LM.match_option match_hint t_hopt p_hopt;
        LM.match_list
          (fun _ -> false)
          (LM.match_pair_fn match_id_res match_expr)
          t_iel
          p_iel]
       env
  | AbsConst (t_hopt, t_id), AbsConst (p_hopt, p_id) ->
     LM.match_attributes
       [LM.match_option match_hint t_hopt p_hopt;
        match_id_res t_id p_id]
       env
  | Attributes t_cal, Attributes p_cal ->
     revert_env_if_no_match
       (LM.match_list (fun _ -> false) match_class_attr t_cal p_cal env)
       env
  | TypeConst t_tc, TypeConst p_tc ->
     match_typeconst t_tc p_tc env
  | ClassUse t_h, ClassUse p_h
  | XhpAttrUse t_h, XhpAttrUse p_h ->
     match_hint t_h p_h env
  | ClassTraitRequire (t_trk, t_h), ClassTraitRequire (p_trk, p_h) ->
     LM.match_attributes
       [match_trait_req_kind t_trk p_trk;
        match_hint t_h p_h]
       env
  | ClassVars (t_kl, t_hopt, t_cvl), ClassVars (p_kl, p_hopt, p_cvl) ->
     LM.match_attributes
       [LM.match_list (fun _ -> false) match_kind t_kl p_kl;
        LM.match_option match_hint t_hopt p_hopt;
        LM.match_list is_star_class_var match_class_var t_cvl p_cvl]
       env
  | XhpAttr (t_kl, t_hopt, t_cvl, t_b, t_pelopt),
    XhpAttr (p_kl, p_hopt, p_cvl, p_b, p_pelopt) ->
     LM.match_attributes
       [LM.match_list (fun _ -> false) match_kind t_kl p_kl;
        LM.match_option match_hint t_hopt p_hopt;
        match_bool t_b p_b;
        LM.match_list is_star_class_var match_class_var t_cvl p_cvl;
        LM.match_option
          (LM.match_pair_fn
             (* we don't care about the position *)
             (fun _ _ env -> dummy_success_res, env)
             (* want to match the expr list correctly*)
             match_expr_list)
          t_pelopt
          p_pelopt]
       env
  | _, _ -> (NoMatch, env)

and match_method_
      (t_method_ : method_)
      (p_method_ : method_)
      (env : matcher_env) : (match_result * matcher_env) =
  let attributes =
    [match_id_res t_method_.m_name p_method_.m_name;
     LM.match_list
       (fun _ -> false)
       match_kind
       t_method_.m_kind
       p_method_.m_kind;
     LM.match_list
       (fun _ -> false)
       match_tparam
       t_method_.m_tparams
       p_method_.m_tparams;
     LM.match_list
       is_star_fun_param
       match_fun_param
       t_method_.m_params
       p_method_.m_params;
     LM.match_option match_hint t_method_.m_ret p_method_.m_ret;
     match_bool t_method_.m_ret_by_ref p_method_.m_ret_by_ref;
     LM.match_list
       is_star_user_attribute
       match_user_attribute
       t_method_.m_user_attributes
       p_method_.m_user_attributes;
     match_fun_kind t_method_.m_fun_kind p_method_.m_fun_kind;
     match_stmt
       (Block t_method_.m_body)
       (Block p_method_.m_body)] in
  match get_id_type p_method_.m_name with
  | Wildcard -> dummy_success_res, env
  | DoNotCare -> match_stmt
                   (Block t_method_.m_body)
                   (Block p_method_.m_body)
                   env
  | _ -> LM.match_attributes attributes env

and match_case
      (t_case : case)
      (p_case : case)
      (env : matcher_env) : (match_result * matcher_env) =
  (* maybe have TODO something to make sure wildcarding works correctly here *)
  match t_case, p_case with
  | Default t_b, Default p_b ->
     match_stmt (Block t_b) (Block p_b) env
  | Case (t_e, t_b), Case (p_e, p_b) ->
     LM.match_attributes
       [match_expr t_e p_e;
        match_stmt (Block t_b) (Block p_b)]
       env
  | _, _ -> NoMatch, env

and match_as_expr
      (t_as_expr : as_expr)
      (p_as_expr : as_expr)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_as_expr, p_as_expr with
  | As_v t_e, As_v p_e ->
     match_expr t_e p_e env
  | As_kv (t_e1, t_e2), As_kv (p_e1, p_e2) ->
     LM.match_attributes
       [match_expr t_e1 p_e1;
        match_expr t_e2 p_e2]
       env
  | _, _ -> NoMatch, env

and match_catch
      (t_catch : catch)
      (p_catch : catch)
      (env : matcher_env) : (match_result * matcher_env) =
  let (t_i1, t_i2, t_b) = t_catch in
  let (p_i1, p_i2, p_b) = p_catch in
  LM.match_attributes
    [match_id_res t_i1 p_i1;
     match_id_res t_i2 p_i2;
     match_stmt (Block t_b) (Block p_b)]
    env

(* def is KStar iff it is a class, or function or string literal with
   name "__KSTAR" *)
and is_star_def (p_def : def) : bool =
  match p_def with
  | Class p_class -> (get_id_type p_class.c_name) = KStar
  | Fun p_fun_ -> (get_id_type p_fun_.f_name) = KStar
  | Ast.Stmt p_stmt -> is_star_stmt p_stmt
  | _ -> false

(* TODO implement this correctly *)
and is_star_hint (_p_hint : hint) : bool = false

(* tparams cannot be KStar matched but this function is in case that changes *)
and is_star_tparam (_p_tparam : tparam) : bool = false

(* fun_params cannot be KStar matched but this function is in case
   that changes *)
and is_star_fun_param (_p_fun_param : fun_param) : bool = false

(* user_attributes cannot be KStar matched but this function
   allows us to change that later *)
and is_star_user_attribute (_p_uatt : user_attribute) : bool = false

(* doesn't make sense to star match a list of class variables *)
and is_star_class_var (_p_cvar : class_var) : bool = false

(* class_elt is KStar iff it is a method with name "__KSTAR" *)
and is_star_celt (celt : class_elt) : bool =
  match celt with
  | Ast.Method meth ->
     let id_type = (get_id_type meth.m_name) in
     id_type = KStar
  | _ -> false

(* stmt is KStar type iff it is the string literal "__KSTAR" *)
and is_star_stmt
      (p_stmt : stmt) : bool =
  match p_stmt with
  | Ast.Expr (_,(String pstr)) -> get_id_type pstr = KStar
  | _ -> false

(* stmt is KStar type iff it is the string literal "__KSTAR" *)
and is_star_expr (p_expr : expr) : bool =
  match snd p_expr with
  | String pstr -> get_id_type pstr = KStar
  | _ -> false

(* catch is KStar type iff the name of the variable the exception is bound
   to is "__KSTAR"
   e.g. catch (Exception $__KSTAR *)
and is_star_catch (p_catch : catch) : bool =
  let (_,ident,_) = p_catch in
  get_id_type ident = KStar

(* case is type KStar iff if the expression to match is "__KSTAR"
   e.g. case "__KSTAR": *)
and is_star_case (p_case : case) : bool =
  match p_case with
  | Case (exp,_) -> is_star_expr exp
  | Default _ -> false

(* is KStar type iff the value is "__KSTAR" *)
and is_star_afield (afl : afield) : bool =
  match afl with
  | AFvalue exp -> is_star_expr exp
  | AFkvalue (exp,_) -> is_star_expr exp

(* stmt is a wildcard iff it consists of just a wildcard expr:
   "__ANY"; *)
and is_wildcard_stmt (p_stm : stmt) : bool =
  match p_stm with
  | Ast.Expr (_,(String pstr)) -> (get_id_type pstr) = Wildcard
  | _ -> false

(* handle_sa_hd_fn for stmt list for SkipAny matching, see comment on
   match_list_with_skips *)
and act_if_skip_any
      (try_match_list_fn :
         stmt list -> stmt list -> stmt list -> matcher_env ->
         match_result * matcher_env)
      (p_hd : stmt)
      (t_tl : stmt list)
      (p_tl : stmt list)
      (skipped_text : stmt list)
      (env : matcher_env) : (match_result * matcher_env) option =
  if is_skip_any_stmt p_hd
  then Some
         (revert_env_if_no_match
            (try_match try_match_list_fn t_tl (p_hd :: p_tl) skipped_text env)
            env)
  else None

(* Tries to match the text with the pattern recursively, sensitive to KStars
   and SkipAnys
   handle_sa_fn for stmt list SkipAny matching,
   see comment on match_list_with_skips*)
and try_match
      (try_match_list_fn :
         stmt list -> stmt list -> stmt list -> matcher_env ->
         match_result * matcher_env)
      (t_list : stmt list)
      (p_list : stmt list)
      (skipped_text : stmt list)
      (env : matcher_env) :
      match_result * matcher_env =
    (* given a chunk of text that was skipped over by a SkipAny
       find all the possible chunks that could match the SkipAny pattern
       (all child blocks + self) *)
    let find_child_text (text_chunk : stmt list) : block list =
      let visitor = new block_finding_visitor () in
      visitor#on_block [] text_chunk in

    (* given an output of find_child_text, find all the matches,
       update env if necessary *)
    let match_child_text
          (child_text : block list)
          (pattern : stmt list)
          (env : matcher_env) : match_result * matcher_env =
      List.fold_left
        ~f:(fun (res_so_far, env) text_blk ->
            let text_res, env' =
              match_stmt (Block text_blk) (Block pattern) env in
            (concat_match_results_nodup [res_so_far; text_res]), env')
        ~init:(NoMatch, env)
        child_text in

    (* Try to match a SkipAny: match sa_pattern over all children of
       skipped_text, as well as making sure the rest of the pattern and
       text match *)
    let match_single_skipany t_list p_list skipped_text sa_pattern env =
      (* Find all blocks that are children of the relevant
         part of the text.
         NOTE skipped_text is reversed because of the way we appended to it, so
         we need to reverse it before finding children *)
      let sa_text = find_child_text (List.rev skipped_text) in
      (* look for matches over these child blocks *)
      let cur_res, env' = match_child_text sa_text sa_pattern env in
      (* check to see if remaining text matches remaining pattern (note these
         must be done in order so that the environment is correct - consistent
         with a linear pass through the file) *)
      let later_res, env2 = try_match try_match_list_fn t_list p_list [] env' in
      match cur_res, later_res with
      | NoMatch, NoMatch | Matches _, NoMatch
      | NoMatch, Matches _ ->  NoMatch, env
      (* if both matches succeeded, we have a match *)
      | Matches _, Matches _ ->
         concat_match_results_nodup [cur_res; later_res],
         env2 in

    (* Handle the case where the pattern started with a SkipAny, figuring
       out all possible amounts of text that the SkipAny could match over *)
    let rec handle_leading_skipany t_list p_list skipped_text sa_pattern env =
      match t_list with
      (* no text left, try matching pattern with the skipped text *)
      | [] -> begin
         match skipped_text with
         | [] ->
            NoMatch, env
         | _ ->
            match_single_skipany [] p_list skipped_text sa_pattern env end
      | t_hd :: t_tl ->
         (* Try skipping more of the text *)
         let rec_res, rec_env =
           handle_leading_skipany
             t_tl p_list (t_hd :: skipped_text) sa_pattern env in
         (* Try matching the SkipAny with the current amount of skipped text *)
         let norec_res, norec_env =
           match_single_skipany t_list p_list skipped_text sa_pattern env in
         (* if either of those worked, we return a success *)
         match rec_res, norec_res with
         | NoMatch, NoMatch -> NoMatch, env
         | Matches _, NoMatch -> rec_res, rec_env
         | NoMatch, Matches _ ->  norec_res, norec_env
         | Matches _, Matches _ ->
            concat_match_results_nodup [rec_res; norec_res],
            (merge_envs rec_env norec_env) in

    (* preprocess a SkipAny token from the pattern if we find one to find
       the pattern that will be matched later with the text it skips *)
    match p_list with
    | hd :: tl when is_skip_any_unproc hd ->begin
        (* next block is the pattern we want to recursively match*)
        let skipany_body, after_skipany =
          match tl with
          | hd :: tail ->
             let body =
               match hd with
               | Block bl -> star_stmt :: bl @ [star_stmt]
               (* incorrectly formatted pattern - SkipAny not followed by
                  block representing pattern to match *)
               | _ -> failwith "missing block after skip stmt" in
             body, tail
          (* incorrectly formatted pattern - SkipAny not followed by block
             representing pattern to match *)
          | _ -> failwith "missing block after skip stmt" in
        (* Deal with the leading SkipAny *)
        let p_list = star_stmt :: after_skipany in
        handle_leading_skipany t_list p_list skipped_text skipany_body env
      end
    (* If there wasn't a leading SkipAny, don't do anything, just call the
       try_match_list_fn *)
    | _ ->
       revert_env_if_no_match
         (try_match_list_fn t_list p_list skipped_text env) env

(* matches stmts by recursively matching their subcomponents *)
and match_stmt
      (t_stmt : stmt)
      (p_stmt : stmt)
      (env0 : matcher_env) : (match_result * matcher_env) =
  let env =
    add_mvar_node env0 is_meta_stmt p_stmt (Hh_match_utils.Stmt t_stmt) in
  let p_stmt = instantiate_mvar_stmt env p_stmt in
  let old_env = env in
  (if is_wildcard_stmt p_stmt
  then
    match t_stmt with
    | Noop -> NoMatch, env
    | _ -> (dummy_success_res, env)
  else
    match t_stmt, p_stmt with
    | Unsafe, Unsafe ->
       (dummy_success_res, env)
    | Fallthrough, Fallthrough ->
       (dummy_success_res, env)
    | Ast.Expr t_expr, Ast.Expr p_expr ->
       match_expr t_expr p_expr env
    | Block t_sl, Block p_sl ->
       LM.match_list_with_skips
         is_star_stmt
         (Some is_meta_stmt)
         (Some (fun stm -> Hh_match_utils.Stmt stm))
         match_stmt
         try_match
         act_if_skip_any
         t_sl
         p_sl
         env
    | Break loc, Break _
    | Continue loc, Continue _ ->
       (* use start because we want line number of beginning of construct *)
       (Matches [(DummyNode, Pos.pos_start loc)], env)
    | Throw t_e, Throw p_e ->
       match_expr t_e p_e env
    | Return (loc, t_eopt), Return (_, p_eopt) ->
       let opt_res = LM.match_option match_expr t_eopt p_eopt env in
       (* if it matched a return where the expression does not have a
          position *)
       if fst opt_res == dummy_success_res
       then (Matches [(DummyNode, Pos.pos_start loc)], env)
       else opt_res
    | Static_var t_el, Static_var p_el ->
       match_expr_list t_el p_el env
    | If (t_e, t_b1, t_b2), If (p_e, p_b1, p_b2) ->
       LM.match_attributes
         [match_expr t_e p_e;
          match_stmt (Block t_b1) (Block p_b1);
          match_stmt (Block t_b2) (Block p_b2)]
         env
    | Do (t_b, t_e), Do (p_b, p_e) ->
       LM.match_attributes
         [match_stmt (Block t_b) (Block p_b);
          match_expr t_e p_e]
         env
    | While (t_e, t_b) , While (p_e, p_b) ->
       LM.match_attributes
         [match_expr t_e p_e;
          match_stmt (Block t_b) (Block p_b)]
         env
    | For (t_e1, t_e2, t_e3, t_b), For (p_e1, p_e2, p_e3, p_b) ->
       LM.match_attributes
         [match_expr t_e1 p_e1;
          match_expr t_e2 p_e2;
          match_expr t_e3 p_e3;
          match_stmt (Block t_b) (Block p_b)]
         env
    | Switch (t_e, t_cl), Switch (p_e, p_cl) ->
       LM.match_attributes
         [match_expr t_e p_e;
          LM.match_list is_star_case match_case t_cl p_cl]
         env
    | Foreach (t_e, t_aopt, t_ase, t_b), Foreach (p_e, p_aopt, p_ase, p_b) ->
       let match_aopt t_aopt p_aopt env =
         match t_aopt, p_aopt with
         | None, Some _ | Some _, None -> NoMatch, env
         | None, None | Some _, Some _ -> dummy_success_res, env in
       LM.match_attributes
         [match_expr t_e p_e;
          match_aopt t_aopt p_aopt;
          match_as_expr t_ase p_ase;
          match_stmt (Block t_b) (Block p_b)]
         env
    | Try (t_b1, t_cl, t_b2), Try (p_b1, p_cl, p_b2) ->
       LM.match_attributes
         [match_stmt (Block t_b1) (Block p_b1);
          LM.match_list is_star_catch match_catch t_cl p_cl;
          match_stmt (Block t_b2) (Block p_b2)]
         env
    | Noop, Noop->
       (dummy_success_res, env)
    | _, _ -> (NoMatch, old_env))
  |> LM.patch_if_necc
       t_stmt p_stmt adjust_range_stmt
       env.transformations.stmt_delete_list
       env.transformations.stmt_transf_map
       stmt_to_text
       (Ast_code_extent.source_extent_stmt env.file env.source)
  |> function ret -> revert_env_if_no_match ret env0

and is_wildcard_expr (exp : expr_) : bool =
  match exp with
  | String pstr -> (get_id_type pstr) = Wildcard
  | _ -> false

and match_afield
      (t_afl : afield)
      (p_afl : afield)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_afl, p_afl with
  | AFvalue t_e, AFvalue p_e -> match_expr t_e p_e env
  | AFkvalue (t_e1, t_e2), AFkvalue (p_e1, p_e2) ->
     LM.match_attributes
       [match_expr t_e1 p_e1;
        match_expr t_e2 p_e2]
       env
  | _, _ -> (NoMatch, env)

and match_shape_field_name
      (t_sfn : shape_field_name)
      (p_sfn : shape_field_name)
      (env : matcher_env) : (match_result * matcher_env) =
  let old_env = env in
  match t_sfn, p_sfn with
  | SFlit t_pstr, SFlit p_pstr ->
     revert_env_if_no_match (match_id_res t_pstr p_pstr env) old_env
  | SFclass_const (t_i, t_pstr), SFclass_const (p_i, p_pstr) ->
     LM.match_attributes
       [match_id_res t_i p_i;
        match_id_res t_pstr p_pstr]
       env
  | _, _ -> (NoMatch, old_env)

and match_og_null_flavor
      (t_onf : og_null_flavor)
      (p_onf : og_null_flavor)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_onf = p_onf
  then dummy_success_res, env
  else NoMatch, env

and match_uop
      (t_uop : uop)
      (p_uop : uop)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_uop = p_uop
  then dummy_success_res, env
  else NoMatch, env

and match_bop
      (t_bop : bop)
      (p_bop : bop)
      (env : matcher_env) : (match_result * matcher_env) =
  match t_bop, p_bop with
  | Eq t_bopt, Eq p_bopt ->
     LM.match_option match_bop t_bopt p_bopt env
  | _, _ ->
     if t_bop = p_bop
     then dummy_success_res, env
     else NoMatch, env

and match_import_flavor
      (t_if : import_flavor)
      (p_if : import_flavor)
      (env : matcher_env) : (match_result * matcher_env) =
  if t_if = p_if
  then dummy_success_res, env
  else NoMatch, env

(* Tries to match the text with the pattern recursively, trying to match
   the given pattern in the program specified *)
and handle_skipany
      (text : program)
      (pat : skipany_ast_node)
      (env : matcher_env) : match_result * matcher_env =
    (* given a chunk of text that was skipped over by a SkipAny
       find all the possible chunks that could match the SkipAny pattern
       (all child blocks + self) *)
    let find_child_text visitor =
      visitor#on_program [] text in

    (* given an output of find_child_text, find all the matches,
       update env if necessary *)
    let match_child_text
          (child_text)
          (pat)
          (match_fn)
          (env : matcher_env) : match_result * matcher_env =
      List.fold_left
        ~f:(fun (res_so_far, env) text_node ->
            let text_res, env' = match_fn text_node pat env in
            (concat_match_results_nodup [res_so_far; text_res]), env')
        ~init:(NoMatch, env)
        child_text in

    match pat with
    | SkipanyExpr e -> match_child_text
      (find_child_text (new expr_finding_visitor ())) e match_expr env
    | SkipanyBlock b -> match_child_text
      (find_child_text (new block_finding_visitor ())) b
      match_statements_in_block env

and handle_expr_skipany
      (text : program)
      (p_expr : expr)
      (env : matcher_env) : match_result * matcher_env =
    handle_skipany text (SkipanyExpr p_expr) env

and handle_stmt_skipany
      (text : program)
      (p_stmts : block)
      (env : matcher_env) : match_result * matcher_env =
    handle_skipany text (SkipanyBlock p_stmts) env

(* Finds all matches of the statement list p_stmts inside the block t_block *)
and match_statements_in_block
      (t_block : block)
      (p_stmts : stmt list)
      (env : matcher_env) : (match_result * matcher_env) =
  let p_len = List.length p_stmts in

  (* Walk through a list of statements and check at each one if the sequence of
     statements starting there matches the pattern *)
  let rec find_matching_sublists t_stmts env : match_result list * matcher_env =
    match t_stmts with
    | [] -> ([], env)
    | _::t_stmts' as stmts ->
      let t_stmts = List.take stmts p_len in
      (* Produces an option on a list of matched statements and the matching
         environment.  If there is one statement that isn't matched by the
         pattern this will be None *)
      let rec match_stmt_lists t_stmts p_stmts env =
        match (t_stmts, p_stmts) with
        | ([], []) -> Some ([], env)
        | (ts::t_stmts', ps::p_stmts') -> begin
          match match_stmt ts ps env with
          | (NoMatch, _) -> None
          | (m, env') -> begin
            match match_stmt_lists t_stmts' p_stmts' env' with
            | None -> None
            | Some (l, env'') -> Some (m :: l, env'') end
          end
        | _ -> None in

      let (matches, env') = match match_stmt_lists t_stmts p_stmts env with
        | None -> ([], env)
        | Some p -> p in

      (* Collapse match results into one match, and recurse *)
      let (l, e) = find_matching_sublists t_stmts' env' in
      (concat_match_results_nodup matches :: l, e) in

  let matches, env' = find_matching_sublists t_block env in
  (concat_match_results_nodup matches, env')

and match_expr
      (t_expr : expr)
      (p_expr : expr)
      (env0 : matcher_env) : (match_result * matcher_env) =
  (* remember do revert *)
  let env =
    add_mvar_node env0 is_meta_expr p_expr (Hh_match_utils.Expr t_expr) in
  let p_expr = instantiate_mvar_expr env p_expr in
  let (t_pos, t_expr_) = t_expr in
  let (_, p_expr_) = p_expr in
  let success_res =
    Matches [(Hh_match_utils.Expr t_expr, Pos.pos_start t_pos)] in
  (* if pattern is a wildcard we will always match*)
  (if is_wildcard_expr p_expr_
  then (success_res, env)
  else
  (* if the match was successful, we stick on a result corresponding to the
     expr as a whole. *)
  let exp_res =
  match t_expr_, p_expr_ with
  | Array t_afl, Array p_afl ->
     LM.match_list is_star_afield match_afield t_afl p_afl env
  | Shape t_sfnel, Shape p_sfnel ->
       LM.match_list
         (fun _ -> false)
         (LM.match_pair_fn match_shape_field_name match_expr)
         t_sfnel
         p_sfnel
         env
  | Collection (t_i, t_al), Collection (p_i, p_al) ->
     LM.match_attributes
       [match_id_res t_i p_i;
        LM.match_list
          is_star_afield
          match_afield
          t_al
          p_al]
       env
  | Null, Null
  | True, True
  | False, False
  | Yield_break, Yield_break ->
     dummy_success_res, env
  | Id t_id, Id p_id
  | Lvar t_id, Lvar p_id ->
     match_id_res t_id p_id env
  | Obj_get (t_e1, t_e2, t_onf), Obj_get (p_e1, p_e2, p_onf) ->
     LM.match_attributes
       [match_expr t_e1 p_e1;
        match_expr t_e2 p_e2;
        match_og_null_flavor t_onf p_onf]
       env
  | Array_get (t_e, t_eopt), Array_get (p_e, p_eopt) ->
     LM.match_attributes
       [match_expr t_e p_e;
        LM.match_option match_expr t_eopt p_eopt]
       env
  | Class_get (t_id, t_pstr), Class_get (p_id, p_pstr)
  | Class_const (t_id, t_pstr), Class_const (p_id, p_pstr) ->
     LM.match_attributes
       [match_id_res t_id p_id;
        match_id_res t_pstr p_pstr]
       env
  | Call (t_e, t_el1, t_el2), Call (p_e, p_el1, p_el2)
  | New (t_e, t_el1, t_el2), New (p_e, p_el1, p_el2) ->
     LM.match_attributes
       [match_expr t_e p_e;
        match_expr_list t_el1 p_el1;
        match_expr_list t_el2 p_el2]
       env
  | Int t_pstr, Int p_pstr
  | Float t_pstr, Float p_pstr
  | String t_pstr, String p_pstr ->
     revert_env_if_no_match (match_id_res t_pstr p_pstr env) env
  | String2 t_el, String2 p_el ->
     LM.match_attributes
       [match_expr_list t_el p_el]
       env
  | Yield t_af, Yield p_af ->
     revert_env_if_no_match (match_afield t_af p_af env) env
  | Clone t_e, Clone p_e
  | Await t_e, Await p_e
  | Unsafeexpr t_e, Unsafeexpr p_e ->
     revert_env_if_no_match (match_expr t_e p_e env) env
  | List t_el, List p_el
  | Expr_list t_el, Expr_list p_el->
     match_expr_list t_el p_el env
  | Cast (t_h, t_e), Cast (p_h, p_e) ->
     LM.match_attributes
       [match_hint t_h p_h;
        match_expr t_e p_e]
       env
  | Unop (t_uop, t_e), Unop (p_uop, p_e) ->
     LM.match_attributes
       [match_uop t_uop p_uop;
        match_expr t_e p_e]
       env
  | Binop (t_bop, t_e1, t_e2), Binop (p_bop, p_e1, p_e2) ->
     LM.match_attributes
       [match_bop t_bop p_bop;
        match_expr t_e1 p_e1;
        match_expr t_e2 p_e2]
       env
  | Eif (t_e1, t_eopt, t_e2), Eif (p_e1, p_eopt, p_e2) ->
     LM.match_attributes
       [match_expr t_e1 p_e1;
        LM.match_option match_expr t_eopt p_eopt;
        match_expr t_e2 p_e2]
       env
  | InstanceOf (t_e1, t_e2), InstanceOf (p_e1, p_e2) ->
     LM.match_attributes
       [match_expr t_e1 p_e1;
        match_expr t_e2 p_e2]
       env
  | Efun (t_f, t_ibl), Efun (p_f, p_ibl) ->
     LM.match_attributes
       [match_fun_ t_f p_f;
        LM.match_list
          (fun _ -> false)
          (LM.match_pair_fn match_id_res match_bool)
          t_ibl
          p_ibl]
       env
  | Lfun t_f, Lfun p_f ->
     revert_env_if_no_match (match_fun_ t_f p_f env) env
  | Xml (t_i, t_iel, t_el), Xml (p_i, p_iel, p_el) ->
     (*TODO make XML matching order-insensitive*)
     LM.match_attributes
       [match_id_res t_i p_i;
        LM.match_list
          (fun _ -> false)
          (LM.match_pair_fn match_id_res match_expr)
          t_iel
          p_iel;
        match_expr_list t_el p_el]
       env
  | Import (t_if, t_e), Import (p_if, p_e) ->
     LM.match_attributes
       [match_import_flavor t_if p_if;
        match_expr t_e p_e]
       env
  | _, _ -> (NoMatch, env) in
  (match fst exp_res with
  | NoMatch -> exp_res
  | Matches _ ->
     concat_match_results [(fst exp_res); success_res], snd exp_res))
  |> LM.patch_if_necc
       t_expr p_expr adjust_range_expr
       env.transformations.expr_delete_list
       env.transformations.expr_transf_map
       expr_to_text
       (Ast_code_extent.source_extent_expr env.file env.source)
  |> function ret -> revert_env_if_no_match ret env0

and match_shape_field
      (t_sf : shape_field)
      (p_sf : shape_field)
      (env : matcher_env) : (match_result * matcher_env) =
  (LM.match_pair_fn match_shape_field_name match_hint) t_sf p_sf env

and match_hint
      (t_hint : hint)
      (p_hint : hint)
      (env : matcher_env) : (match_result * matcher_env) =
  let child_res =
  match (snd t_hint), (snd p_hint) with
  | Hoption t_hint, Hoption p_hint ->
     match_hint t_hint p_hint env
  | Hfun (t_hl, t_b, t_h), Hfun (p_hl, p_b, p_h) ->
     LM.match_attributes
       [LM.match_list (fun _ -> false) match_hint t_hl p_hl;
        match_bool t_b p_b;
        match_hint t_h p_h]
       env
  | Htuple t_hl, Htuple p_hl ->
     LM.match_list (fun _ -> false) match_hint t_hl p_hl env
  | Happly (t_id, t_hl), Happly (p_id, p_hl) ->
     LM.match_attributes
       [match_id_res t_id p_id;
        LM.match_list (fun _ -> false) match_hint t_hl p_hl]
       env
  | Hshape t_sfl, Hshape p_sfl ->
     LM.match_list (fun _ -> false) match_shape_field t_sfl p_sfl env
  | Haccess (t_id1, t_id2, t_idl), Haccess (p_id1, p_id2, p_idl) ->
     LM.match_attributes
       [match_id_res t_id1 p_id1;
        match_id_res t_id2 p_id2;
        LM.match_list (fun _ -> false) match_id_res t_idl p_idl]
       env
  | _, _ -> NoMatch, env in
  match fst child_res with
  | NoMatch -> NoMatch, env
  | Matches _ ->
    (update_res_with
      (fst child_res) (Hint t_hint) (Pos.pos_start (fst t_hint))),
    snd child_res

(* to help with printing, will also remove any
   dummy_success_res elements *)
let sort_and_remove_duplicates compare l =
  let sl = List.sort compare l in
  let rec go l acc = match l with
    | [] -> List.rev acc
    | (x::xs) when x = (DummyNode, File_pos.dummy) -> go xs acc
    | [x] -> List.rev (x::acc)
    | (x1::x2::xs) ->
      if File_pos.line (snd x1) = File_pos.line (snd x2)
      then go (x2::xs) acc
      else go (x2::xs) (x1::acc)
  in go sl []

(* Actual function for finding matching nodes *)
let find_matches
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
    : (ast_node * File_pos.t) list =
  let match_res =
    match_ast_nodes
      (Program text)
      (Program pattern_parsed.Parser_hack.ast)
      { file = text_file;
        source = text_content;
        uses_regexp = false;
        comments = pattern_parsed.Parser_hack.comments;
        metavars = MetavarMap.empty;
        transformations =
          { stmt_delete_list = [];
            expr_delete_list = [];
            stmt_transf_map = [];
            expr_transf_map = [] };
        patches = PatchSet.empty } in
  match fst match_res with
  | Matches result -> result
  | NoMatch -> []

(* takes a program that consists of a single toplevel block i.e. a pattern used
   with the -s switch, and returns a list of that blocks' statements *)
let get_skipany_stmt = function
  | [Ast.Stmt (Ast.Block stmts)] -> Some stmts
  | _ -> None

let find_matches_expr_or_stmt
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
      (skipany_fn)
      (skipany_handler_fn) : (ast_node * File_pos.t) list =
  match skipany_fn pattern_parsed.Parser_hack.ast with
    | Some stmts -> begin
        let res, _ =
          skipany_handler_fn text stmts
            { file = text_file;
              source = text_content;
              uses_regexp = false;
              comments = pattern_parsed.Parser_hack.comments;
              metavars = MetavarMap.empty;
              transformations =
                { stmt_delete_list = [];
                  expr_delete_list = [];
                  stmt_transf_map = [];
                  expr_transf_map = [] };
              patches = PatchSet.empty } in
       match res with
       | Matches result -> result
       | NoMatch -> [] end
    | None -> []

(* function for searching for statements *)
let find_matches_stmt
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
    : (ast_node * File_pos.t) list =
  find_matches_expr_or_stmt text text_file text_content pattern_parsed
    get_skipany_stmt handle_stmt_skipany

(* Gets the expression that is the pattern while verifying the pattern
   is of the correct form (returning None if the pattern is not) *)
let get_skipany_expr = function
  | [Ast.Stmt (Ast.Expr exp)] -> Some exp
  | _ -> None

(* function for searching for expressions *)
let find_matches_expr
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
    : (ast_node * File_pos.t) list =
  find_matches_expr_or_stmt text text_file text_content pattern_parsed
    get_skipany_expr handle_expr_skipany

(* general patching function for statements and expressions *)
let patch_expr_or_stmt
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
      (transformations : patch_maps)
      ~(use_hh_format : bool)
      (patch_fn) (skipany_fn): string option =
  match skipany_fn pattern_parsed.Parser_hack.ast with
  | Some pat -> begin
     let res, env =
       patch_fn
         text
         pat
         { file = text_file;
           source = text_content;
           uses_regexp = false;
           comments = pattern_parsed.Parser_hack.comments;
           metavars = MetavarMap.empty;
           transformations;
           patches = PatchSet.empty } in
     match res with
     | NoMatch -> None
     | Matches _ ->
        if PatchSet.is_empty env.patches
        then None
        else
          Some (Patcher.apply_patches
                  ~src:text_content
                  ~patches:(PatchSet.elements env.patches)
                  ~format_result:use_hh_format) end
  | None -> None

(* function for patching statements *)
let patch_stmt
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
      (transformations : patch_maps)
      ~(use_hh_format : bool) : string option =
  patch_expr_or_stmt text text_file text_content pattern_parsed transformations
    use_hh_format handle_stmt_skipany get_skipany_stmt

(* function for patching expressions *)
let patch_expr
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
      (transformations : patch_maps)
      ~(use_hh_format : bool) :
      string option =
  patch_expr_or_stmt text text_file text_content pattern_parsed transformations
    use_hh_format handle_expr_skipany get_skipany_expr

let match_and_patch
      (text : program)
      (text_file : Relative_path.t)
      (text_content : string)
      (pattern_parsed : Parser_hack.parser_return)
      (transformations : patch_maps)
      ~(use_hh_format : bool) :
      string option =
  let res, env =
    match_ast_nodes
      (Program text)
      (Program pattern_parsed.Parser_hack.ast)
      { file = text_file;
        source = text_content;
        uses_regexp = false;
        comments = pattern_parsed.Parser_hack.comments;
        metavars = MetavarMap.empty;
        transformations = transformations;
        patches = PatchSet.empty } in
  match res with
  | NoMatch -> None
  | Matches _ ->
     if PatchSet.is_empty env.patches
     then None
     else
       Some (Patcher.apply_patches
               ~src:text_content
               ~patches:(PatchSet.elements env.patches)
               ~format_result:use_hh_format)

let format_matches
      (matches : (ast_node * File_pos.t) list)
      (text_code : string) : string =
  let match_list =
    sort_and_remove_duplicates
      (fun (m1:(ast_node * File_pos.t))
           (m2:(ast_node * File_pos.t)) ->
       File_pos.line (snd m1) - File_pos.line (snd m2))
      matches in
  (* format a single match as "line num: line" *)
  let format_single_match (single_match : ast_node * File_pos.t) : string =
    let pos = snd single_match in
    if File_pos.is_dummy pos then "" else
    let line_num, bol_pos = File_pos.line_beg pos in
    let eol_pos = String.index_from text_code bol_pos '\n' in
    Printf.sprintf
      "%d: %s"
      line_num
      (String.sub text_code bol_pos (eol_pos - bol_pos)) in
  (String.concat
     "\n"
     (List.map ~f:format_single_match match_list))
