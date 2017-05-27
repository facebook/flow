(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

(* Read in all the flowlib files *)
let get_flowlibs dir =
 Sys.readdir dir
 |> Array.fold_left (fun acc file ->
    let contents = Utils.string_of_file (Filename.concat dir file) in
    (file, contents)::acc
 ) []

(* Turn the (name, contents) list into a PPX ast (string * string) array
 * expression *)
let contents =
  let flowlib_dir = Sys.argv.(1) in
  get_flowlibs flowlib_dir
  |> List.map (fun (name, contents) -> Exp.tuple [
      Exp.constant (Const.string name); Exp.constant (Const.string contents);
    ])
  |> Exp.array

(* Whenever we see [%flowlib_contents], replace it wil the flowlib contents *)
let ppx_gen_flowlibs_mapper _argv =
 { default_mapper with
   expr = fun mapper expr ->
     match expr with
     | { pexp_desc = Pexp_extension ({ txt = "flowlib_contents"; _ }, PStr []); _} ->
       contents
     | other -> default_mapper.expr mapper other; }

 let () =
   register "ppx_gen_flowlibs" ppx_gen_flowlibs_mapper
