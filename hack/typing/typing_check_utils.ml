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

(*****************************************************************************)
(* Usually used when we want to run typing hooks *)
(*****************************************************************************)
let check_defs tcopt fn {FileInfo.funs; classes; typedefs; _} =
  fst (Errors.do_ (fun () ->
    List.iter funs begin fun (_, x) ->
      Typing_check_service.type_fun tcopt fn x
    end;
    List.iter classes begin fun (_, x) ->
      Typing_check_service.type_class tcopt fn x
    end;
    List.iter typedefs begin fun (_, x) ->
      Typing_check_service.check_typedef tcopt fn x
    end;
  ))
