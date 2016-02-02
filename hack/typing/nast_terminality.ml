(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Nast

module SN = Naming_special_names

module FuncTerm = Typing_heap.FuncTerminality

let static_meth_terminal env ci meth_id =
  let class_name = match ci with
    | CI cls_id -> Some (snd cls_id)
    | CIself | CIstatic -> Some (Typing_env.get_self_id env)
    | CIparent -> Some (Typing_env.get_parent_id env)
    | CIexpr _ -> None (* we declared the types, but didn't check the bodies yet
                       so can't tell anything here *)
  in match class_name with
    | Some class_name -> FuncTerm.raise_exit_if_terminal
        (FuncTerm.get_static_meth class_name (snd meth_id))
    | None -> ()

(* Module coded with an exception, if we find a terminal statement we
 * throw the exception Exit.
 *)
module Terminal: sig
  val case: Typing_env.env -> case -> bool
  val block: Typing_env.env -> block -> bool

end = struct

  let rec terminal env inside_case stl =
    List.iter stl (terminal_ env inside_case)

  and terminal_ env inside_case = function
    | Break _ -> if inside_case then () else raise Exit
    | Continue _
    | Throw _
    | Return _
    | Expr (_, Yield_break)
    | Expr (_, Assert (AE_assert (_, False)))
      -> raise Exit
    | Expr (_, Call (Cnormal, (_, Id (_, fun_name)), _, _)) ->
      FuncTerm.raise_exit_if_terminal (FuncTerm.get_fun fun_name)
    | Expr (_, Call (Cnormal, (_, Class_const (ci, meth_id)), _, _)) ->
      static_meth_terminal env ci meth_id
    | If ((_, True), b1, _) -> terminal env inside_case b1
    | If ((_, False), _, b2) -> terminal env inside_case b2
    | If (_, b1, b2) ->
      (try terminal env inside_case b1; () with Exit ->
        terminal env inside_case b2)
    | Switch (_, cl) ->
      terminal_cl env cl
    | Try (b, catch_list, _) ->
      (* Note: return inside a finally block is allowed in PHP and
       * overrides any return in try or catch. It is an error in <?hh,
       * however. The only way that a finally block can thus be
       * terminal is if it throws unconditionally -- however, there's
       * no good case I (eletuchy) could think of for why one would
       * write *always* throwing code inside a finally block.
       *)
      (try terminal env inside_case b; () with Exit ->
        terminal_catchl env inside_case catch_list)
    | While ((_, True), b)
    | Do (b, (_, True))
    | For ((_, Expr_list []), (_, Expr_list []), (_, Expr_list []), b) ->
        if not (Nast_visitor.HasBreak.block b) then raise Exit
    | Do _
    | While _
    | For _
    | Foreach _
    | Noop
    | Fallthrough
    | Expr _
    | Static_var _ -> ()

  and terminal_catchl env inside_case = function
    | [] -> raise Exit
    | (_, _, x) :: rl ->
      (try
         terminal env inside_case x
       with Exit ->
         terminal_catchl env inside_case rl
      )

  and terminal_cl env = function
    (* Empty list case should only be when switch statement is malformed and has
       no case or default blocks *)
    | [] -> ()
    | [Case (_, b)] | [Default b] -> terminal env true b
    | Case (_, b) :: rl ->
      (try
         terminal env true b;
          (* TODO check this *)
         if List.exists b (function Break _ -> true | _ -> false)
         then ()
         else raise Exit
       with Exit -> terminal_cl env rl)
    | Default b :: rl ->
      (try terminal env true b with Exit ->
        terminal_cl env rl)

  and terminal_case env = function
    | Case (_, b) | Default b -> terminal env true b

  let block env stl =
    try terminal env false stl; false with Exit -> true

  let case env c =
    try terminal_case env c; false with Exit -> true

end

(* TODO jwatzman #3076304 convert this and Terminal to visitor pattern to
 * remove copy-pasta *)
module SafeCase: sig
  val check: Pos.t -> Typing_env.env -> case list -> unit
end = struct

  let rec terminal env stl =
    List.iter stl (terminal_ env)

  and terminal_ env = function
    | Fallthrough
    | Break _
    | Continue _
    | Throw _
    | Return _
    | Expr (_, Yield_break)
    | Expr (_, Assert (AE_assert (_, False))) -> raise Exit
    | Expr (_, Call (Cnormal, (_, Id (_, fun_name)), _, _)) ->
      FuncTerm.raise_exit_if_terminal (FuncTerm.get_fun fun_name)
    | Expr (_, Call (Cnormal, (_, Class_const (ci, meth_id)), _, _)) ->
      static_meth_terminal env ci meth_id
    | If ((_, True), b1, _) -> terminal env b1
    | If ((_, False), _, b2) -> terminal env b2
    | If (_, b1, b2) ->
      (try terminal env b1; () with Exit -> terminal env b2)
    | Switch (_, cl) ->
      terminal_cl env cl
    | Try (b, catches, _) ->
      (* NOTE: contents of finally block are not executed in normal flow, so
       * they cannot contribute to terminality *)
      (try terminal env b; ()
        with Exit -> terminal_catchl env catches)
    | Do _
    | While _
    | For _
    | Foreach _
    | Noop
    | Expr _
    | Static_var _ -> ()

  and terminal_catchl env = function
    | [] -> raise Exit
    | (_, _, x) :: rl ->
      (try
         terminal env x
       with Exit ->
         terminal_catchl env rl
      )

  and terminal_cl env = function
    (* Empty list case should only be when switch statement is malformed and has
       no case or default blocks *)
    | [] -> ()
    | [Case (_, b)] | [Default b] -> terminal env b
    | Case (_, b) :: rl ->
      (try
         terminal env b;
          (* TODO check this *)
         if List.exists b (function Break _ -> true | _ -> false)
         then ()
         else raise Exit
       with Exit -> terminal_cl env rl)
    | Default b :: rl ->
      (try terminal env b with Exit -> terminal_cl env rl)

  let check p env = function
    | [] -> () (* Skip empty cases so we can use tl below *)
    | cl -> (* Skip the last case *)
      List.iter (List.tl_exn (List.rev cl)) begin fun c ->
        try match c with
          (* Allow empty cases to fall through *)
          | Case (_, [])
          | Default [] -> ()
          | Case (e, b) -> begin
            terminal env b;
            Errors.case_fallthrough p (fst e)
          end
          | Default b -> begin
            terminal env b;
            Errors.default_fallthrough p
          end
        with Exit -> ()
      end
end
