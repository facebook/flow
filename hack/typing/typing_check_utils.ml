(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Usually used when we want to run typing hooks *)
(*****************************************************************************)
let check_defs nenv {FileInfo.funs; classes; typedefs; _} =
  fst (Errors.do_ (fun () ->
    List.iter (fun (_, x) -> Typing_check_service.type_fun nenv x) funs;
    List.iter (fun (_, x) -> Typing_check_service.type_class nenv x) classes;
    List.iter (fun (_, x) -> Typing_check_service.check_typedef x) typedefs;
  ))
