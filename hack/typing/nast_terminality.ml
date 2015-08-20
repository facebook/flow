(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Nast

module SN = Naming_special_names

module FuncTerm = Typing_heap.FuncTerminality

(* Module coded with an exception, if we find a terminal statement we
 * throw the exception Exit.
 *)
module Terminal: sig
  val case: case -> bool
  val block: block -> bool

end = struct

  let rec terminal inside_case stl =
    List.iter (terminal_ inside_case) stl

  and terminal_ inside_case = function
    | Break _ -> if inside_case then () else raise Exit
    | Continue _
    | Throw _
    | Return _
    | Expr (_, Yield_break)
    | Expr (_, Assert (AE_assert (_, False)))
      -> raise Exit
    | Expr (_, Call (Cnormal, (_, Id (_, fun_name)), _, _)) ->
      FuncTerm.raise_exit_if_terminal (FuncTerm.get_fun fun_name)
    | Expr (_, Call (Cnormal, (_, Class_const (CI cls_id, meth_id)), _, _)) ->
      FuncTerm.raise_exit_if_terminal
        (FuncTerm.get_static_meth (snd cls_id) (snd meth_id))
    | If ((_, True), b1, _) -> terminal inside_case b1
    | If ((_, False), _, b2) -> terminal inside_case b2
    | If (_, b1, b2) ->
      (try terminal inside_case b1; () with Exit ->
        terminal inside_case b2)
    | Switch (_, cl) ->
      terminal_cl cl
    | Try (b, catch_list, _) ->
      (* Note: return inside a finally block is allowed in PHP and
       * overrides any return in try or catch. It is an error in <?hh,
       * however. The only way that a finally block can thus be
       * terminal is if it throws unconditionally -- however, there's
       * no good case I (eletuchy) could think of for why one would
       * write *always* throwing code inside a finally block.
       *)
      (try terminal inside_case b; () with Exit ->
        terminal_catchl inside_case catch_list)
    | While ((_, True), b)
    | Do (b, (_, True))
    | For ((_, Expr_list []), (_, Expr_list []), (_, Expr_list []), b) ->
        if not (NastVisitor.HasBreak.block b) then raise Exit
    | Do _
    | While _
    | For _
    | Foreach _
    | Noop
    | Fallthrough
    | Expr _
    | Static_var _ -> ()

  and terminal_catchl inside_case = function
    | [] -> raise Exit
    | (_, _, x) :: rl ->
      (try
         terminal inside_case x
       with Exit ->
         terminal_catchl inside_case rl
      )

  and terminal_cl = function
    (* Empty list case should only be when switch statement is malformed and has
       no case or default blocks *)
    | [] -> ()
    | [Case (_, b)] | [Default b] -> terminal true b
    | Case (_, b) :: rl ->
      (try
         terminal true b;
          (* TODO check this *)
         if List.exists (function Break _ -> true | _ -> false) b
         then ()
         else raise Exit
       with Exit -> terminal_cl rl)
    | Default b :: rl ->
      (try terminal true b with Exit -> terminal_cl rl)

  and terminal_case = function
    | Case (_, b) | Default b -> terminal true b

  let block stl =
    try terminal false stl; false with Exit -> true

  let case c =
    try terminal_case c; false with Exit -> true

end

(* TODO jwatzman #3076304 convert this and Terminal to visitor pattern to
 * remove copy-pasta *)
module SafeCase: sig
  val check: Pos.t -> case list -> unit
end = struct

  let rec terminal stl =
    List.iter (terminal_) stl

  and terminal_ = function
    | Fallthrough
    | Break _
    | Continue _
    | Throw _
    | Return _
    | Expr (_, Yield_break)
    | Expr (_, Assert (AE_assert (_, False))) -> raise Exit
    | Expr (_, Call (Cnormal, (_, Id (_, fun_name)), _, _)) ->
      FuncTerm.raise_exit_if_terminal (FuncTerm.get_fun fun_name)
    | Expr (_, Call (Cnormal, (_, Class_const (CI cls_id, meth_id)), _, _)) ->
      FuncTerm.raise_exit_if_terminal
        (FuncTerm.get_static_meth (snd cls_id) (snd meth_id))
    | If ((_, True), b1, _) -> terminal b1
    | If ((_, False), _, b2) -> terminal b2
    | If (_, b1, b2) ->
      (try terminal b1; () with Exit -> terminal b2)
    | Switch (_, cl) ->
      terminal_cl cl
    | Try (b, catches, _) ->
      (* NOTE: contents of finally block are not executed in normal flow, so
       * they cannot contribute to terminality *)
      (try terminal b; () with Exit -> terminal_catchl catches)
    | Do _
    | While _
    | For _
    | Foreach _
    | Noop
    | Expr _
    | Static_var _ -> ()

  and terminal_catchl = function
    | [] -> raise Exit
    | (_, _, x) :: rl ->
      (try
         terminal x
       with Exit ->
         terminal_catchl rl
      )

  and terminal_cl = function
    (* Empty list case should only be when switch statement is malformed and has
       no case or default blocks *)
    | [] -> ()
    | [Case (_, b)] | [Default b] -> terminal b
    | Case (_, b) :: rl ->
      (try
         terminal b;
          (* TODO check this *)
         if List.exists (function Break _ -> true | _ -> false) b
         then ()
         else raise Exit
       with Exit -> terminal_cl rl)
    | Default b :: rl ->
      (try terminal b with Exit -> terminal_cl rl)

  let check p = function
    | [] -> () (* Skip empty cases so we can use tl below *)
    | cl -> List.iter begin fun c ->
      try match c with
        (* Allow empty cases to fall through *)
        | Case (_, [])
        | Default [] -> ()
        | Case (e, b) -> begin
          terminal b;
          Errors.case_fallthrough p (fst e)
        end
        | Default b -> begin
          terminal b;
          Errors.default_fallthrough p
        end
      with Exit -> ()
    end (List.tl (List.rev cl)) (* Skip the last case *)
end
