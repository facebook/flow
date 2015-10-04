(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Remember (when we care) the type found at a position *)
let save_infer result_ty result_pos target_line target_column ty pos env =
  if Pos.inside pos target_line target_column && !result_ty = None
  then begin
    match ty with
    | Typing_defs.DeclTy ty ->
       result_ty := Some (Typing_print.full_strip_ns env ty);
       result_pos := Some (Typing_reason.to_pos (fst ty));
    | Typing_defs.LoclTy ty ->
       result_ty := Some (Typing_print.full_strip_ns env ty);
       result_pos := Some (Typing_reason.to_pos (fst ty));
  end

let attach_hooks line column =
  let result_ty = ref None in
  let result_pos = ref None in
  let get_result () =
    !result_pos, !result_ty
  in
  Typing_hooks.attach_infer_ty_hook
    (save_infer result_ty result_pos line column);
  get_result

let detach_hooks () =
  Typing_hooks.remove_all_hooks ()
