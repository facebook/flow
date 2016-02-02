(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Pass to check for unsequenced modifications to local variables.
 *
 * PHP has a somewhat weird evaluation order (technically unspecified,
 * but...) that can cause us trouble. Evaluation is *usually*
 * left-to-right except that for certain operators, if the left hand side
 * is a variable, it is right-to-left.
 *
 * If we didn't handle this in some way, the typechecker would be unsound,
 * since code like '$x + f($x = array())' typechecks if you assume
 * left-to-right evaluation but will fail at runtime.
 *
 * We deal with this by outlawing unsequenced modification and access
 * to local variables. We consider certain operations to be "sequencing";
 * (that is, they sequence their subexpressions)
 * two expressions are unsequenced if their least common
 * ancestor in the syntax tree is *not* sequencing.
 *
 * Basically everything is sequencing except:
 *   all binops except && and ||, array/object accesses,
 *   array field specifiers, list(...),
 *   assignment (sort of; '$x = $x+1' is OK, '$x = $x++' is not)
 * (The first three correspond to when eval order can get reversed;
 *  the others just correspond to bad code.)
 *
 * This all solves the unsoundness problem without needing to commit the
 * typechecker to exactly understanding the evaluation order and also
 * closes off most of the ways a strict Hack program can observe unusual
 * evaluation order (side-effectful __toString methods can observe it;
 * maybe other ways also).
 *
 *)

open Core
open Nast
open Utils

type env = {
  (* tracking will be set when we are under an unsequenced operation
   * that requires us to be tracking variable accesses *)
  tracking : bool;
  (* We should maybe use a better data structure than lists. *)
  used : (Pos.t * Ident.t) list;
  assigned : (Pos.t * Ident.t) list;
}

let empty_env = {
  tracking = false;
  used = [];
  assigned = [];
}
let tracking_env = { empty_env with tracking = true }

let add_id map px = px :: map
let assign_local env id =
  if env.tracking then
  { env with assigned = add_id env.assigned id }
  else env
let use_local env id =
  if env.tracking then
  { env with used = add_id env.used id }
  else env

(* Traverse down through list(...) destructuring to find all
 * the "atomic" lvals that are being assigned to. *)
let unpack_lvals e =
  let rec unpack acc = function
    | _, List es -> List.fold_left ~f:unpack ~init:acc es
    | e -> e :: acc
  in List.rev (unpack [] e)

let get_lvar = function
  | _, Lvar id -> `Fst id
  | e -> `Snd e

(* Pass that just collects all the variables used (and not just written to)
 * in fragments of code.
 * We need this because a common idiom is to assign to unused variables
 * as a form of documentation, and we want to ignore those variables
 * when doing our sequencing checks. *)
let used_variables_visitor =
  object(this)
  inherit [ISet.t] Nast_visitor.nast_visitor as parent

  method! on_lvar acc (_, id) = ISet.add id acc
  (* We have to handle expressions just enough to avoid counting
   * assignments as uses of variables.
   * (We do still count operator-assignments) *)
  method! on_expr acc (_, e_ as e) =
    match e_ with
    | Binop (Ast.Eq None, e1, e2) ->
      let _, not_vars = List.partition_map (unpack_lvals e1) get_lvar in
      let acc = List.fold_left ~f:this#on_expr ~init:acc not_vars in
      this#on_expr acc e2
    | _ -> parent#on_expr acc e
  end

(* Actual sequence checking pass *)
let sequence_visitor ~require_used used_vars =
  (* First declare some helpers that don't need to be in the object *)

  (* Merge a separately checked environment into the current one *)
  let merge env new_env =
    if env.tracking then
    { used = new_env.used @ env.used;
      assigned = new_env.assigned @ env.assigned;
      tracking = true
    }
    else empty_env
  in
  (* Check the used/assigned sets from two unsequenced expressions and report
   * errors if they conflict.
   * N.B that the sets are only checked for conflicts against the other
   * environment and not for other accesses in itself. There may well be
   * multiple writes to the same location in one env; these will either
   * have been properly sequenced or an error would already have been reported.
   *)
  let check_unsequenced env1 env2 =
    let id_matches (_p1, x1) (_p2, x2) = x1 = x2 in
    let check_write_conflicts reads writes (p, _ as id) =
        let conflicting_reads = List.filter reads (id_matches id) in
        let conflicting_writes = List.filter writes (id_matches id) in

        (* Ignore conflicts when writing to variables that are never read
         * in order to support the idiom where pointless variable
         * assignments are used as documentation. *)
        let conflicting_writes =
          if not require_used then conflicting_writes else
          List.filter conflicting_writes (fun (_, x) -> ISet.mem x used_vars) in

        let cleanup = List.map ~f:fst in
        if conflicting_reads <> [] then
        Errors.local_variable_modified_and_used p (cleanup conflicting_reads);
        if conflicting_writes <> [] then
        Errors.local_variable_modified_twice p (cleanup conflicting_writes)
    in

    (* reversing the lists makes things sorted more naturally in the output *)
    let reads1, writes1 = List.rev env1.used, List.rev env1.assigned in
    let reads2, writes2 = List.rev env2.used, List.rev env2.assigned in

    List.iter writes1 (check_write_conflicts reads2 writes2);
    (* Only check writes2 for conflicts with the reads from env1,
     * since we've already checked for write/write conflicts. *)
    List.iter writes2 (check_write_conflicts reads1 [])
  in

  let merge_unsequenced env env1 env2 =
    check_unsequenced env1 env2;
    let env = merge env env1 in
    let env = merge env env2 in
    env
  in

  let merge_unsequenced_list envs =
    List.fold_left envs ~f:begin fun env1 env2 ->
      check_unsequenced env1 env2;
      merge env1 env2
    end ~init:tracking_env
  in

  (* And now the actual visitor object *)
  object(this)
  inherit [env] Nast_visitor.nast_visitor as parent

  method check_unsequenced_exprs env e1 e2 =
    let env1 = this#on_expr tracking_env e1 in
    let env2 = this#on_expr tracking_env e2 in
    merge_unsequenced env env1 env2

  method! on_expr env (_, e_ as e) =
    match e_ with
    | Lvar id ->
      use_local env id

    | Unop ((Ast.Uincr | Ast.Udecr | Ast.Upincr | Ast.Updecr),
            (_, Lvar id)) ->
      assign_local env id

    (* Assignment. This is pretty hairy because of list(...)
     * destructuring and the treatment necessary to allow
     * code like '$x = $x + 1'. *)
    | Binop (Ast.Eq _, e1, e2) ->
      (* Unpack any list(...) destructuring and separate out locals
       * we are assigning to from other lvals. *)
      let lvars, lval_exprs =
        List.partition_map (unpack_lvals e1) get_lvar in

      (* Build separate envs for the direct variable assignments and
       * for the other lvals assigned to. We treat all these lvals
       * as unsequenced. *)
      let lvar_envs = List.map lvars (assign_local tracking_env) in
      let lhs_var_env = merge_unsequenced_list lvar_envs in

      let lval_expr_envs = List.map lval_exprs (this#on_expr tracking_env) in
      let lval_expr_env = merge_unsequenced_list lval_expr_envs in

      let rhs_env = this#on_expr tracking_env e2 in
      (* Our lhs local var writes only conflict with other *writes* on
       * the rhs, not with reads (need to allow '$x = $x + 1' but
       * disallow '$x = $x++'), so we do a check_unsequenced against
       * a version of env2 containing only the writes and then merge
       * with the real thing. *)
      let rhs_writes = { empty_env with assigned = rhs_env.assigned } in
      check_unsequenced lhs_var_env rhs_writes;
      (* Also check local assignments against the other lvalues *)
      check_unsequenced lhs_var_env lval_expr_env;

      (* We've manually handled everything relating to the lhs locals,
       * so merge them into the env and then do a regular unsequenced
       * merge of the non local lhs stuff against the rhs. *)
      let env = merge env lhs_var_env in
      merge_unsequenced env lval_expr_env rhs_env


    (* leave && and || sequenced before making all
     * the other binops unsequenced *)
    | Binop ((Ast.AMpamp | Ast.BArbar), _, _) -> parent#on_expr env e

    (* These operations have unsequenced subexpressions. *)
    | Binop (_, e1, e2)
    | Obj_get (e1, e2, _)
    | Array_get (e1, Some e2)
      -> this#check_unsequenced_exprs env e1 e2

    | Efun (f, idl) ->
      let nb = Nast.assert_named_body f.f_body in
      (* Ignore the current environment and start fresh. *)
      let _acc = this#on_block empty_env nb.fnb_nast in
      (* we use all the variables we are capturing *)
      List.fold_left ~f:use_local ~init:env idl

    | _ -> parent#on_expr env e

  method! on_field env (e1, e2) = this#check_unsequenced_exprs env e1 e2
  method! on_afield env = function
    | AFvalue e -> this#on_expr env e
    | AFkvalue (e1, e2) -> this#check_unsequenced_exprs env e1 e2

  (* Handle case to disallow assigning to vars inside case labels. *)
  method! on_case acc = function
    | Default b ->
      let acc = this#on_block acc b in
      acc
    | Case (e, b) ->
      let env = this#on_expr tracking_env e in
      List.iter env.assigned (fun (p, _) -> Errors.assign_during_case p);
      let acc = this#on_block acc b in
      acc
  end

let sequence_check_block block =
  let used_vars = used_variables_visitor#on_block ISet.empty block in
  let visitor = sequence_visitor ~require_used:true used_vars in
  let _ = visitor#on_block empty_env block in
  ()
let sequence_check_expr expr =
  let visitor = sequence_visitor ~require_used:false ISet.empty in
  let _ = visitor#on_expr empty_env expr in
  ()
