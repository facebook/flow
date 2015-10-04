(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Ast

(* helpers for GetLocals below *)
(* TODO It really sucks that this and Nast_terminality.Terminal are very
 * slightly different (notably, this version is somewhat buggier). Fixing that
 * exposes a lot of errors in www unfortunately -- we should bite the bullet on
 * fixing switch all the way when we do that, most likely though -- see tasks
 * #3140431 and #2813555. *)
let rec terminal in_try stl = List.iter (terminal_ in_try) stl
and terminal_ in_try = function
  | Throw _ when not in_try -> raise Exit
  | Throw _ -> ()
  | Continue _
  | Expr (_, (Call ((_, Id (_, "assert")), [_, False], [])
                 | Call ((_, Id (_, "invariant")), (_, False) :: _ :: _, [])
                 | Call ((_, Id (_, "invariant_violation")), _ :: _, [])))
  | Return _
  | Expr (_, Call ((_, Id (_, "exit")), _, _)) -> raise Exit
  | If (_, b1, b2) ->
      (try terminal in_try b1; () with Exit ->
        terminal in_try b2)
  | Switch (_, cl) ->
      terminal_cl in_try cl
  | Block b -> terminal in_try b
  | Try (b, catch_l, finb) ->
    (* return is not allowed in finally, so we can ignore fb *)
    (terminal true b;
     List.iter (terminal_catch in_try) catch_l)
  | Do _
  | While _
  | For _
  | Foreach _
  | Noop
  | Expr _
  | Unsafe
  | Fallthrough
  | Break _ (* TODO this is terminal sometimes too, except switch, see above. *)
  | Static_var _ -> ()

and terminal_catch in_try (_, _, b) =
  terminal in_try b

and terminal_cl in_try = function
  | [] -> raise Exit
  | Case (_, b) :: rl ->
      (try
        terminal in_try b;
        if List.exists (function Break _ -> true | _ -> false) b
        then ()
        else raise Exit
      with Exit -> terminal_cl in_try rl)
  | Default b :: rl ->
      (try terminal in_try b with Exit -> terminal_cl in_try rl)

let is_terminal stl = try terminal false stl; false with Exit -> true

(* Module calculating the locals for a statement
* This is useful when someone uses $x on both sides
* of an If statement, for example:
* if(true) {
*   $x = 0;
* } else {
    $x = 1;
* }
*)
module GetLocals = struct

  let rec lvalue acc = function
    | (p, Lvar (_, x)) -> SMap.add x p acc
    | _, List lv -> List.fold_left lvalue acc lv
    (* Ref forms a local inside a foreach *)
    | (_, Ref (p, Lvar (_, x))) ->  SMap.add x p acc
    | _ -> acc

  let rec stmt acc st =
    match st with
    | Expr (_, Binop (Eq None, lv, rv))
    | Expr (_, Eif ((_, Binop (Eq None, lv, rv)), _, _)) ->
        let acc = stmt acc (Expr rv) in
        lvalue acc lv
    | Unsafe
    | Fallthrough
    | Expr _ | Break _ | Continue _ | Throw _
    | Do _ | While _ | For _ | Foreach _
    | Return _ | Static_var _ | Noop -> acc
    | Block b -> block acc b
    | If (_, b1, b2) ->
        let term1 = is_terminal b1 in
        let term2 = is_terminal b2 in
        if term1 && term2
        then acc
        else if term1
        then smap_union acc (block SMap.empty b2)
        else if term2
        then smap_union acc (block SMap.empty b1)
        else
          let b1 = block SMap.empty b1 in
          let b2 = block SMap.empty b2 in
          smap_union acc (smap_inter b1 b2)
    | Switch (e, cl) ->
        let cl = List.filter begin function
          | Case (_, b)
          | Default b -> not (is_terminal b)
        end cl in
        let cl = casel cl in
        let c = smap_inter_list cl in
        smap_union acc c
    | Try (b, cl, f) ->
        let c = block SMap.empty b in
        let lcl = List.map catch cl in
        let tcl = List.map (fun (_, _, b) -> is_terminal b) cl in
        let cl = List.fold_right2 begin fun x y acc ->
          if y then acc else x :: acc
        end lcl tcl [] in
        let c = smap_inter_list (c :: cl) in
        smap_union acc c

  and block acc l = List.fold_left stmt acc l

  and casel = function
    | [] -> []
    | Case (_, []) :: rl -> casel rl
    | Default b :: rl
    | Case (_, b) :: rl ->
        let b = block SMap.empty b in
        b :: casel rl

  and catch (_, _, b) = block SMap.empty b

end

(* Module forbidding cyclic type constraints *)
module HintCycle = struct

  let rec hint stack params (p, h) =
    hint_ stack p params h

  and hint_ stack p params x =
    match x with
    | Htuple hl -> hintl stack params hl
    | Hoption h -> hint stack params h
    | Hfun (hl,_, h) -> hintl stack params hl; hint stack params h
    | Happly ((_, x), []) when SSet.mem x stack ->
        Errors.cyclic_constraint p
    | Happly ((_, x), []) when SMap.mem x params ->
        let stack = SSet.add x stack in
        (match SMap.get x params with
        | Some (Some param) ->
            hint stack params param
        | _ -> ()
        )
    | Happly (_, hl) ->
        hintl stack params hl
    | Hshape l ->
        List.iter (fun (_, x) -> hint stack params x) l
    (* do we need to do anything here? probably when we add type params *)
    | Haccess (_, _, _) -> ()

  and hintl stack params l = List.iter (hint stack params) l

  let check_constraint cstrs (_, _, hopt) =
    match hopt with
    | None -> ()
    | Some h -> hint SSet.empty cstrs h
end
