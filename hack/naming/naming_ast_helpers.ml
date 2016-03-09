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
open Core

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
        | Some (Some (_, param)) ->
            hint stack params param
        | _ -> ()
        )
    | Happly (_, hl) ->
        hintl stack params hl
    | Hshape l ->
        List.iter l (fun (_, x) -> hint stack params x)
    (* do we need to do anything here? probably when we add type params *)
    | Haccess (_, _, _) -> ()

  and hintl stack params l = List.iter l (hint stack params)

  let check_constraint cstrs (_, _, cstr_opt) =
    match cstr_opt with
    | None -> ()
    | Some (_, h) -> hint SSet.empty cstrs h
end
