(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module TLazyHeap = Typing_lazy_heap

(* Not adding a Typing_dep here because it will be added when the
 * Nast is fully processed (by the caller of this code) *)
let get_fun = TLazyHeap.get_fun

let get_static_meth tcopt (cls_name:string) (meth_name:string) =
  match TLazyHeap.get_class tcopt cls_name with
  | None -> None
  | Some { Typing_defs.tc_smethods ; _ } ->
    begin match SMap.get meth_name tc_smethods with
      | None -> None
      | Some { Typing_defs.ce_type = (_r, Typing_defs.Tfun fty) ; _} ->
        Some fty
      | Some _ -> None
    end

let raise_exit_if_terminal = function
  | None -> ()
  | Some ({ Typing_defs.ft_ret = (_r, Typing_defs.Tprim Nast.Tnoreturn); _})
    -> raise Exit
  | Some _ -> ()
