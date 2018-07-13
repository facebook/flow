(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

(* Read in all the flowlib files *)
let get_libs dir =
 Sys.readdir dir
 |> Array.fold_left (fun acc file ->
    let contents = Script_utils.string_of_file (Filename.concat dir file) in
    (file, contents)::acc
 ) []

(* Turn the (name, contents) list into a PPX ast (string * string) array
 * expression *)
let contents lib_dir =
  get_libs lib_dir
  |> List.map (fun (name, contents) -> Exp.tuple [
      Exp.constant (Const.string name); Exp.constant (Const.string contents);
    ])
  |> Exp.array

(* Whenever we see [%flowlib_contents], replace it wil the flowlib contents *)
let ppx_gen_flowlibs_mapper argv =
  let flowlib_contents, prelude_contents =
    match argv with
    | [flowlib_dir; prelude_dir] -> contents flowlib_dir, contents prelude_dir
    | _ ->
      failwith
        (Printf.sprintf "Expected two arguments, got %d." (List.length argv))
  in
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "flowlib_contents"; _ }, PStr []); _} ->
        flowlib_contents
      | { pexp_desc = Pexp_extension ({ txt = "prelude_contents"; _ }, PStr []); _} ->
        prelude_contents
      | other -> default_mapper.expr mapper other; }

  let () =
    register "ppx_gen_flowlibs" ppx_gen_flowlibs_mapper
