(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Migrate_parsetree
open Ast_405

let ocaml_version = Versions.ocaml_405

open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

(* Read in all the flowlib files *)
let get_libs dir =
  Sys.readdir dir
  |> Array.fold_left
       (fun acc file ->
         let contents = Script_utils.string_of_file (Filename.concat dir file) in
         (file, contents) :: acc)
       []

(* Turn the (name, contents) list into a PPX ast (string * string) array
 * expression *)
let contents lib_dir =
  get_libs lib_dir
  |> List.map (fun (name, contents) ->
         Exp.tuple [Exp.constant (Const.string name); Exp.constant (Const.string contents)])
  |> Exp.array

(* Whenever we see [%flowlib_contents], replace it wil the flowlib contents *)
let ppx_gen_flowlibs_mapper ~flowlib_contents ~prelude_contents =
  {
    default_mapper with
    expr =
      (fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "flowlib_contents"; _ }, PStr []); _ } ->
          flowlib_contents
        | { pexp_desc = Pexp_extension ({ txt = "prelude_contents"; _ }, PStr []); _ } ->
          prelude_contents
        | other -> default_mapper.expr mapper other);
  }

let () =
  let flowlib_dir_ref = ref "" in
  let prelude_dir_ref = ref "" in
  let args =
    [
      ("-flowlib", Arg.Set_string flowlib_dir_ref, "Path to flowlib directory");
      ("-prelude", Arg.Set_string prelude_dir_ref, "Path to prelude directory");
    ]
  in
  Driver.register ~name:"ppx_gen_flowlibs" ~args ocaml_version (fun _config _cookies ->
      let (flowlib_contents, prelude_contents) =
        match (!flowlib_dir_ref, !prelude_dir_ref) with
        | ("", _)
        | (_, "") ->
          failwith "Expected two arguments."
        | (flowlib_dir, prelude_dir) -> (contents flowlib_dir, contents prelude_dir)
      in
      ppx_gen_flowlibs_mapper ~flowlib_contents ~prelude_contents)
